{ pkgs, ... }:
{
  systemd.user.services.claude-remote-control = {
    Unit = {
      Description = "Claude Code Remote Control in ~/Stuff";
      # Give up after 5 failed starts; the app then shows the host offline.
      StartLimitIntervalSec = "10min";
      StartLimitBurst = 5;
      # Starting with the X session hands spawned sessions DISPLAY/XAUTHORITY
      # (headed agent-browser, screenshots); default.target comes too early.
      After = [ "graphical-session.target" ];
      PartOf = [ "graphical-session.target" ];
    };
    Service = {
      WorkingDirectory = "%h/Stuff";
      # The store path makes a claude bump change this unit, so the
      # home-manager switch restarts it onto the new version.
      ExecStart = "${pkgs.masterPkgs.claude-code}/bin/claude remote-control";
      # ssh-agent runs as a user service but doesn't export its socket to
      # the manager; gpg finds its socket on its own.
      Environment = [ "SSH_AUTH_SOCK=%t/ssh-agent" ];
      # The TUI repaints its status every second: 420 journal lines/min.
      StandardOutput = "null";
      Restart = "always";
      RestartSec = "30s";
    };
    Install.WantedBy = [ "graphical-session.target" ];
  };
}
