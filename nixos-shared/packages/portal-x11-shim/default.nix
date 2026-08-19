# xdg-desktop-portal backend providing Screenshot on a bare X11 WM.
#
# Why this exists: flameshot >= 14 captures through the Screenshot portal and
# documents its useX11LegacyScreenshot bypass as deprecated. Of the packaged
# backends, only gnome, kde, cinnamon and xfce implement
# org.freedesktop.impl.portal.Screenshot, and each shells out to a desktop
# component xmonad does not run. This one shells out to scrot instead.
#
# Not enabled by default -- see nixos-shared/xdg-portal-x11.nix for the
# wiring, which stays unimported while the legacy flag still works.
{
  lib,
  runCommand,
  writeText,
  python3,
  scrot,
}:
let
  python = python3.withPackages (ps: [ ps.pygobject3 ]);
  dbusName = "org.freedesktop.impl.portal.desktop.x11shim";
  binary = "xdg-desktop-portal-x11-shim";

  # Deliberately no UseIn=: which backend serves which interface is decided
  # by xdg.portal.config, not by XDG_CURRENT_DESKTOP matching.
  portalFile = writeText "x11-shim.portal" ''
    [portal]
    DBusName=${dbusName}
    Interfaces=org.freedesktop.impl.portal.Screenshot;org.freedesktop.impl.portal.Access;
  '';
in
runCommand "xdg-desktop-portal-x11-shim-1.0"
  {
    meta = {
      description = "Minimal xdg-desktop-portal Screenshot backend for bare X11 window managers";
      platforms = lib.platforms.linux;
      mainProgram = binary;
    };
  }
  ''
    install -Dm755 /dev/null $out/libexec/${binary}
    substitute ${./shim.py} $out/libexec/${binary} \
      --replace-fail '@python@' ${python}/bin/python3 \
      --replace-fail '@scrot@' ${lib.getExe scrot}
    chmod +x $out/libexec/${binary}

    install -Dm644 ${portalFile} $out/share/xdg-desktop-portal/portals/x11-shim.portal

    install -Dm644 ${writeText "${dbusName}.service" ''
      [D-BUS Service]
      Name=${dbusName}
      Exec=@out@/libexec/${binary}
      SystemdService=${binary}.service
    ''} $out/share/dbus-1/services/${dbusName}.service

    install -Dm644 ${writeText "${binary}.service" ''
      [Unit]
      Description=Portal service (minimal X11 implementation)
      PartOf=graphical-session.target

      [Service]
      Type=dbus
      BusName=${dbusName}
      ExecStart=@out@/libexec/${binary}
      Restart=on-failure
    ''} $out/share/systemd/user/${binary}.service

    substituteInPlace $out/share/dbus-1/services/${dbusName}.service \
      $out/share/systemd/user/${binary}.service --replace-fail '@out@' "$out"

    # Smoke test: without DISPLAY the shim must refuse cleanly rather than
    # traceback. Cheap, and it proves gi/Gio actually import -- a missing
    # typelib is otherwise a runtime surprise at screenshot time.
    if DISPLAY= $out/libexec/${binary}; then
      echo "shim did not refuse to start without DISPLAY" >&2
      exit 1
    fi
  ''
