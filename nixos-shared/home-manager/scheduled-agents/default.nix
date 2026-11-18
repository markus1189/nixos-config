{ pkgs, lib, ... }:
let
  claude = "${pkgs.masterPkgs.claude-code}/bin/claude";
  maxAgeHours = 12;
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
          ExecStopPost = "${pkgs.notifySendTelegram}/bin/notifySendTelegram 'agent ${name} dispatched: \${SERVICE_RESULT}'";
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

  cleanup = pkgs.writeShellScript "${prefix}cleanup" ''
    set -euo pipefail
    cut=$(( ($(${pkgs.coreutils}/bin/date +%s) - ${toString maxAgeHours} * 3600) * 1000 ))
    ${claude} agents --json --all \
      | ${pkgs.jq}/bin/jq -r --argjson cut "$cut" '.[]
          | select(.kind == "background" and (.name | startswith("${prefix}")) and .startedAt < $cut)
          | .id' \
      | while read -r id; do
          ${claude} stop "$id" || true
          ${claude} rm "$id"
        done
  '';

  jobs = [
    {
      name = "warm-start";
      schedule = "*-*-* 18:00";
      prompt = "List three short ideas for what to look into tomorrow morning. Answer in markdown, under 200 words.";
    }
  ];
in
{
  systemd.user = lib.mkMerge (
    map mkAgent jobs
    ++ [
      {
        services."${prefix}cleanup" = {
          Unit.Description = "Stop and remove scheduled agents older than ${toString maxAgeHours}h";
          Service = {
            Type = "oneshot";
            ExecStart = "${cleanup}";
            WorkingDirectory = "%h/Stuff";
          };
        };
        timers."${prefix}cleanup" = {
          Timer = {
            OnCalendar = "*-*-* 04:00";
            Persistent = true;
          };
          Install.WantedBy = [ "timers.target" ];
        };
      }
    ]
  );
}
