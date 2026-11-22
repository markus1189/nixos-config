{ pkgs, lib, ... }:
let
  claude = "${pkgs.masterPkgs.claude-code}/bin/claude";
  doneMaxAgeDays = 7;
  idleMaxDays = 3;
  cleanup = pkgs.writeShellApplication {
    name = "claude-session-cleanup";
    runtimeInputs = with pkgs; [ coreutils jq procps systemd ];
    text = ''
      now=$(date +%s)
      sessions=$(${claude} agents --json --all)

      # Transcript mtime is the only last-activity signal.
      idle_days() {
        local t
        for t in "$HOME"/.claude/projects/*/"$1".jsonl; do
          [[ -e $t ]] || return 1
          echo $(( (now - $(stat -c %Y "$t")) / 86400 ))
          return 0
        done
      }

      jq -r --argjson cutoff "$(( (now - ${toString doneMaxAgeDays} * 86400) * 1000 ))" '.[]
          | select(.kind == "background" and .state == "done" and .status == null and .startedAt < $cutoff)
          | .id' <<<"$sessions" \
        | while read -r id; do ${claude} rm "$id" || true; done

      jq -r '.[] | select(.kind == "background" and .status == "idle")
          | "\(.id) \(.sessionId)"' <<<"$sessions" \
        | while read -r id sid; do
            d=$(idle_days "$sid") || continue
            if (( d >= ${toString idleMaxDays} )); then ${claude} stop "$id" || true; fi
          done

      rc=$(systemctl --user show -p MainPID --value claude-remote-control.service)
      [[ $rc == 0 ]] && exit 0
      jq -r '.[] | select(.kind == "interactive" and .status == "idle")
          | "\(.pid) \(.sessionId)"' <<<"$sessions" \
        | while read -r pid sid; do
            [[ $(ps -o ppid= -p "$pid" | tr -d ' ') == "$rc" ]] || continue
            d=$(idle_days "$sid") || continue
            if (( d >= ${toString idleMaxDays} )); then kill -TERM "$pid" || true; fi
          done
    '';
  };
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
    Unit.Description = "Clean up finished and long-idle Claude sessions";
    Service = {
      Type = "oneshot";
      ExecStart = lib.getExe cleanup;
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
