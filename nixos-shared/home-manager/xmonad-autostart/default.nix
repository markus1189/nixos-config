{ config, lib, ... }:

# Declarative autostart for the graphical session: one supervised systemd user
# unit per program, instead of backgrounded commands in
# `services.xserver.displayManager.sessionCommands`.
#
# Ordering is the entire point. The NixOS xsession wrapper activates
# `graphical-session.target` *before* it execs the window manager, so anything
# hung off that target races xmonad. `xmonad-session.target` is started by
# xmonad's own startupHook instead -- the only party that knows when the WM is
# actually up. Starting an already-active target is a no-op, so restarting
# xmonad (mod-q) does not relaunch the programs.
#
# Placement stays where it belongs: systemd decides *that* a program runs,
# xmonad's manageHook decides *where* its window lands. The autostarted windows
# therefore need a stable, matchable identity (see the WM_CLASS instance
# markers in xmonad.hs), not a spawn-time workspace argument.

let
  cfg = config.my.xmonadAutostart;
in
{
  options.my.xmonadAutostart = lib.mkOption {
    default = { };
    example = lib.literalExpression ''
      {
        firefox.command = "''${pkgs.firefox}/bin/firefox";
      }
    '';
    description = ''
      Programs to start once xmonad is up, as an attribute set of systemd user
      services bound to `xmonad-session.target`. Each becomes
      `<name>.service`: inspect with `systemctl --user status <name>`, read its
      output with `journalctl --user -u <name>`, bring a closed one back with
      `systemctl --user start <name>`.
    '';
    type = lib.types.attrsOf (
      lib.types.submodule (
        { name, ... }:
        {
          options = {
            command = lib.mkOption {
              type = lib.types.str;
              description = ''
                ExecStart command line. Must be an absolute path: systemd user
                units do not get a login shell's PATH.
              '';
            };

            description = lib.mkOption {
              type = lib.types.str;
              default = "Autostart ${name}";
              description = "Unit description.";
            };

            after = lib.mkOption {
              type = lib.types.listOf lib.types.str;
              default = [ ];
              description = ''
                Extra units to order this one after, without pulling them in.
              '';
            };

            requires = lib.mkOption {
              type = lib.types.listOf lib.types.str;
              default = [ ];
              description = ''
                Units this program cannot run without. Pulled in and ordered
                before, so a client unit can depend on its daemon.
              '';
            };
          };
        }
      )
    );
  };

  config = lib.mkIf (cfg != { }) {
    systemd.user.targets.xmonad-session.Unit = {
      Description = "xmonad session (started from xmonad's startupHook)";
      # Logout stops graphical-session.target, which stops this one, which
      # stops every autostarted program below via PartOf.
      BindsTo = [ "graphical-session.target" ];
      After = [ "graphical-session.target" ];
    };

    systemd.user.services = lib.mapAttrs (_name: app: {
      Unit = {
        Description = app.description;
        PartOf = [ "xmonad-session.target" ];
        After = [ "xmonad-session.target" ] ++ app.after ++ app.requires;
        # Honoured by sd-switch (home-manager's `systemd.user.startServices`
        # default): a rebuild that merely bumps a store path must not kill a
        # running window -- and with a text editor or a chat client, closing
        # the window is the user's decision to make, not the activation
        # script's.
        X-RestartIfChanged = false;
      }
      // lib.optionalAttrs (app.requires != [ ]) { Requires = app.requires; };

      Install.WantedBy = [ "xmonad-session.target" ];

      Service = {
        Type = "simple";
        ExecStart = app.command;
        # Quitting a program is a decision, not a failure. `systemctl --user
        # start <name>` brings it back within the session.
        Restart = "no";
        # The user manager is started by pam_systemd at login, long before any
        # shell profile runs, so its environment block never learns about the
        # agent -- and a unit started from it hands that gap down to every
        # child. Magit then shells out to git, git to ssh, ssh finds no agent
        # and falls back to reading ~/.ssh directly, i.e. a passphrase prompt
        # for an already-unlocked key. `programs.ssh.startAgent = true` puts
        # the socket at $XDG_RUNTIME_DIR/ssh-agent, which %t resolves to here
        # (same reasoning as laptop/atuin-sync.nix).
        Environment = [ "SSH_AUTH_SOCK=%t/ssh-agent" ];
      };
    }) cfg;
  };
}
