{ pkgs, ... }:
let
  claude = "${pkgs.masterPkgs.claude-code}/bin/claude";
  maxAgeHours = 24;
  cleanup = pkgs.writeShellScript "claude-session-cleanup" ''
    set -euo pipefail
    cutoff=$(${pkgs.coreutils}/bin/date -d '${toString maxAgeHours} hours ago' +%s%3N)
    ${claude} agents --json --all \
      | ${pkgs.jq}/bin/jq -r --argjson cutoff "$cutoff" '.[]
          | select(.kind == "background" and .status == "idle" and .startedAt < $cutoff)
          | .id' \
      | while read -r id; do
          ${claude} stop "$id" || true
          ${claude} rm "$id"
        done
  '';
in
{
  systemd.user.services.claude-remote-control = {
    Unit = {
      Description = "Claude Code Remote Control in ~/Stuff";
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
      ExecStart = "${claude} remote-control";
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

  systemd.user.services.claude-session-cleanup = {
    Unit.Description = "Stop and remove idle Claude background sessions older than ${toString maxAgeHours}h";
    Service = {
      Type = "oneshot";
      ExecStart = "${cleanup}";
      WorkingDirectory = "%h/Stuff";
    };
  };
  systemd.user.timers.claude-session-cleanup = {
    Timer = {
      OnCalendar = "*-*-* 04:00";
      Persistent = true;
    };
    Install.WantedBy = [ "timers.target" ];
  };
}
