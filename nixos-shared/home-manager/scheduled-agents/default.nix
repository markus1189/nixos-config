{ pkgs, lib, ... }:
let
  claude = "${pkgs.masterPkgs.claude-code}/bin/claude";
  prefix = "scheduled-agent-";

  mkAgent =
    {
      name,
      schedule,
      prompt,
      model ? "sonnet",
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
      schedule = "*-*-* 20:00";
      prompt = "List three short ideas for what to look into tomorrow morning. Answer in markdown, under 200 words. Also look at today's Stuff dirs.";
    }
  ];
in
{
  systemd.user = lib.mkMerge (map mkAgent jobs);
}
