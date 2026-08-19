#!@python@
"""A minimal xdg-desktop-portal backend: Screenshot and Access, X11 only.

Flameshot 14 captures through org.freedesktop.portal.Screenshot and calls its
useX11LegacyScreenshot escape hatch deprecated. Every packaged backend that
implements org.freedesktop.impl.portal.Screenshot delegates to a desktop shell
(gnome-shell, kwin, cinnamon-screenshot, xfce4-screenshooter), none of which
exist under xmonad. This implements the two interfaces needed to make the
portal path work on a bare X11 window manager, and captures with scrot.

Access is implemented as an unconditional yes. That is only defensible because
this backend declares exactly two interfaces: the frontend will never consult
it for Camera, Location or Background, and on X11 any client that wants the
root window can already have it without asking anyone.
"""

import os
import sys
import tempfile

import gi

gi.require_version("Gio", "2.0")
from gi.repository import Gio, GLib  # noqa: E402

SCROT = "@scrot@"

BUS_NAME = "org.freedesktop.impl.portal.desktop.x11shim"
OBJECT_PATH = "/org/freedesktop/portal/desktop"

# impl.portal.Screenshot target bits: 1=Screen, 2=Window, 4=Area,
# 8=ActiveWindow. scrot covers all but the non-interactive window picker.
TARGET_SCREEN = 1
TARGET_AREA = 4
TARGET_ACTIVE_WINDOW = 8
AVAILABLE_TARGETS = TARGET_SCREEN | TARGET_AREA | TARGET_ACTIVE_WINDOW

RESPONSE_SUCCESS = 0
RESPONSE_CANCELLED = 1
RESPONSE_ERROR = 2

# Captures land in XDG_RUNTIME_DIR (0700, wiped at logout). Well-behaved
# clients unlink the file once they have read it -- flameshot does -- but
# nothing in the protocol obliges them to, so sweep after ourselves.
LEFTOVER_TIMEOUT_SECONDS = 300

INTERFACE_XML = """
<node>
  <interface name='org.freedesktop.impl.portal.Screenshot'>
    <method name='Screenshot'>
      <arg type='o' name='handle' direction='in'/>
      <arg type='s' name='app_id' direction='in'/>
      <arg type='s' name='parent_window' direction='in'/>
      <arg type='a{sv}' name='options' direction='in'/>
      <arg type='u' name='response' direction='out'/>
      <arg type='a{sv}' name='results' direction='out'/>
    </method>
    <method name='PickColor'>
      <arg type='o' name='handle' direction='in'/>
      <arg type='s' name='app_id' direction='in'/>
      <arg type='s' name='parent_window' direction='in'/>
      <arg type='a{sv}' name='options' direction='in'/>
      <arg type='u' name='response' direction='out'/>
      <arg type='a{sv}' name='results' direction='out'/>
    </method>
    <property name='AvailableTargets' type='u' access='read'/>
    <property name='version' type='u' access='read'/>
  </interface>
  <interface name='org.freedesktop.impl.portal.Access'>
    <method name='AccessDialog'>
      <arg type='o' name='handle' direction='in'/>
      <arg type='s' name='app_id' direction='in'/>
      <arg type='s' name='parent_window' direction='in'/>
      <arg type='s' name='title' direction='in'/>
      <arg type='s' name='subtitle' direction='in'/>
      <arg type='s' name='body' direction='in'/>
      <arg type='a{sv}' name='options' direction='in'/>
      <arg type='u' name='response' direction='out'/>
      <arg type='a{sv}' name='results' direction='out'/>
    </method>
    <property name='version' type='u' access='read'/>
  </interface>
</node>
"""

REQUEST_XML = """
<node>
  <interface name='org.freedesktop.impl.portal.Request'>
    <method name='Close'/>
  </interface>
</node>
"""

REQUEST_INTERFACE = Gio.DBusNodeInfo.new_for_xml(REQUEST_XML).interfaces[0]


def log(message):
    print(f"xdg-desktop-portal-x11-shim: {message}", file=sys.stderr, flush=True)


class Request:
    """The impl-side Request object the frontend closes to cancel us."""

    def __init__(self, connection, handle, on_close):
        self._connection = connection
        self._on_close = on_close
        # ...with_closures2 rather than register_object: the latter is
        # deprecated in GLib and warns on every export.
        self._id = connection.register_object_with_closures2(
            handle, REQUEST_INTERFACE, self._on_call, None, None
        )

    def _on_call(self, _connection, _sender, _path, _interface, method, _args, invocation):
        if method != "Close":
            invocation.return_error_literal(
                Gio.io_error_quark(), Gio.IOErrorEnum.NOT_SUPPORTED, method
            )
            return
        invocation.return_value(None)
        self._on_close()

    def unexport(self):
        if self._id:
            self._connection.unregister_object(self._id)
            self._id = 0


class Screenshot:
    """One in-flight Screenshot request."""

    def __init__(self, connection, handle, options, invocation):
        self._invocation = invocation
        self._answered = False
        self._process = None
        self._cancellable = Gio.Cancellable()
        self._request = Request(connection, handle, self._cancel)

        handle_fd, self._path = tempfile.mkstemp(
            prefix="portal-screenshot-", suffix=".png", dir=GLib.get_user_runtime_dir()
        )
        os.close(handle_fd)

        target = options.get("target", TARGET_SCREEN)
        interactive = options.get("interactive", False)
        if target == TARGET_ACTIVE_WINDOW:
            selection = ["--focused"]
        elif target == TARGET_AREA and interactive:
            selection = ["--select", "--freeze"]
        else:
            selection = []

        # Compression 1 rather than scrot's default: the file is written,
        # read once and deleted, so encode time matters and size does not.
        argv = [SCROT, *selection, "--overwrite", "--compression", "1",
                "--file", self._path]

        try:
            process = Gio.Subprocess.new(argv, Gio.SubprocessFlags.STDERR_SILENCE)
        except GLib.Error as error:
            log(f"could not spawn scrot: {error.message}")
            self._answer(RESPONSE_ERROR)
            return

        self._process = process
        process.wait_check_async(self._cancellable, self._on_scrot_exit)

    def _cancel(self):
        """Close() on the request: cancelling the wait would leave scrot
        running -- and an interactive --select holds a screen-wide grab --
        so kill the child too."""
        self._cancellable.cancel()
        if self._process is not None:
            self._process.force_exit()

    def _on_scrot_exit(self, process, result):
        try:
            process.wait_check_finish(result)
        except GLib.Error as error:
            if self._cancellable.is_cancelled():
                self._answer(RESPONSE_CANCELLED)
            else:
                # scrot exits non-zero when the user aborts --select too.
                log(f"scrot failed: {error.message}")
                self._answer(RESPONSE_CANCELLED)
            return

        if not os.path.exists(self._path) or os.path.getsize(self._path) == 0:
            log("scrot exited cleanly but produced no image")
            self._answer(RESPONSE_ERROR)
            return

        self._answer(RESPONSE_SUCCESS, {"uri": GLib.Variant("s", GLib.filename_to_uri(self._path))})

    def _answer(self, response, results=None):
        if self._answered:
            return
        self._answered = True
        self._request.unexport()

        if response == RESPONSE_SUCCESS:
            GLib.timeout_add_seconds(LEFTOVER_TIMEOUT_SECONDS, self._sweep)
        else:
            self._sweep()

        self._invocation.return_value(GLib.Variant("(ua{sv})", (response, results or {})))

    def _sweep(self):
        try:
            os.unlink(self._path)
        except OSError:
            pass
        return GLib.SOURCE_REMOVE


class Backend:
    def __init__(self, connection):
        self._connection = connection
        for interface in Gio.DBusNodeInfo.new_for_xml(INTERFACE_XML).interfaces:
            connection.register_object_with_closures2(
                OBJECT_PATH, interface, self._on_call, self._on_get_property, None
            )

    def _on_call(self, _connection, _sender, _path, interface, method, args, invocation):
        if method == "Screenshot":
            handle, _app_id, _parent, options = args.unpack()
            Screenshot(self._connection, handle, options, invocation)
        elif method == "AccessDialog":
            invocation.return_value(GLib.Variant("(ua{sv})", (RESPONSE_SUCCESS, {})))
        elif method == "PickColor":
            # Deliberately unimplemented: no X11 tool here picks a colour
            # interactively, and flameshot's own picker never uses the portal.
            invocation.return_value(GLib.Variant("(ua{sv})", (RESPONSE_ERROR, {})))
        else:
            invocation.return_error_literal(
                Gio.io_error_quark(), Gio.IOErrorEnum.NOT_SUPPORTED,
                f"{interface}.{method}",
            )

    def _on_get_property(self, _connection, _sender, _path, interface, prop):
        if prop == "AvailableTargets":
            return GLib.Variant("u", AVAILABLE_TARGETS)
        if prop == "version":
            return GLib.Variant("u", 2 if interface.endswith("Screenshot") else 1)
        return None


def main():
    if not os.environ.get("DISPLAY"):
        log("DISPLAY is unset; this backend is X11 only")
        return 1

    loop = GLib.MainLoop()
    flags = Gio.BusNameOwnerFlags.NONE
    if "-r" in sys.argv[1:] or "--replace" in sys.argv[1:]:
        flags = Gio.BusNameOwnerFlags.REPLACE | Gio.BusNameOwnerFlags.ALLOW_REPLACEMENT

    def on_bus_acquired(connection, _name):
        Backend(connection)

    def on_name_lost(_connection, name):
        log(f"lost the bus name {name}")
        loop.quit()

    Gio.bus_own_name(
        Gio.BusType.SESSION, BUS_NAME, flags, on_bus_acquired, None, on_name_lost
    )
    loop.run()
    return 0


if __name__ == "__main__":
    sys.exit(main())
