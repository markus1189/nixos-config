{ pkgs, lib, ... }:
let
  claude = "${pkgs.masterPkgs.claude-code}/bin/claude";
  prefix = "scheduled-agent-";

  mkAgent =
    {
      name,
      schedule,
      prompt,
      model ? "opus",
      effort ? "medium",
      permissionMode ? "auto",
      tools ? [ ],
    }:
    let
      flags = lib.cli.toCommandLineShellGNU { } {
        bg = true;
        name = "${prefix}${name}";
        inherit model effort;
        permission-mode = permissionMode;
        allowedTools = if tools == [ ] then null else lib.concatStringsSep "," tools;
      };
    in
    {
      services."${prefix}${name}" = {
        Unit.Description = "Scheduled agent: ${name}";
        Service = {
          Type = "oneshot";
          ExecStart = "${pkgs.writeShellScript "${prefix}${name}" "${claude} ${flags} ${lib.escapeShellArg prompt}"}";
          # claude refuses background sessions in an untrusted $HOME.
          WorkingDirectory = "%h/Stuff";
          # The --bg daemon would otherwise die with the oneshot's cgroup.
          KillMode = "process";
          ExecStopPost = "-${pkgs.notifySendTelegram}/bin/notifySendTelegram 'agent ${name} dispatched: \${SERVICE_RESULT}'";
          Environment = [ "SSH_AUTH_SOCK=%t/ssh-agent" ];
        };
      };
      timers."${prefix}${name}" = {
        Timer = {
          OnCalendar = schedule;
          Persistent = true;
        };
        Install.WantedBy = [ "timers.target" ];
      };
    };

  jobs = [
    {
      name = "warm-start";
      # Runs on p1. 01:00 so its 5h usage window expires before the 07:30 start;
      # the --bg daemon retires sessions idle for 8h, so check in by ~09:00.
      schedule = "*-*-* 01:00";
      prompt = ''
        Morning warm start, based on what happened recently in ~/Stuff (start at llms.txt; any host).
        Pick a few things you judge most worth my morning, ranked; fewer is fine.
        Do the legwork now, e.g. web research, filling gaps in notes, reviewing new code
        for high-value improvements (logic, performance, bugs).
        Read-only everywhere except today's dir ($(stuff-today --default)), where you may
        write drafts, scripts and diffs. Never send anything that reaches other people:
        mail, comments, PRs, issues, posts.
        Every item ends in a result, an artifact plus the exact command to apply it,
        or "dropped: tried X, failed because Y". No "we could look into X".
        Friction/tooling fixes cost context and maintenance: propose one only if it
        recurred, you verified it against the real case, and you state its cost.
      '';
    }
  ];
in
{
  systemd.user = lib.mkMerge (map mkAgent jobs);
}
