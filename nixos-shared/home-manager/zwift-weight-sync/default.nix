{ environmentFile }:
{ writeShellScriptBin, curl, jq, coreutils, cacert, lib }:
let
  scriptName = "sync-weight-to-zwift";

  script = writeShellScriptBin scriptName ''
    set -e

    export PATH=${lib.makeBinPath [ curl jq coreutils ]}

    : "''${HC_PING_URL:=}"

    ${builtins.replaceStrings [ "@cacert@" ] [ "${cacert}" ]
      (builtins.readFile ./sync-weight-to-zwift.sh)}
  '';
in {
  service = {
    Unit.Description = "Sync weight from Beeminder to Zwift";
    Service = {
      Type = "oneshot";
      EnvironmentFile = environmentFile;
      ExecStart = "${script}/bin/${scriptName}";
    };
  };

  timer = {
    Unit.After = [ "time-set.target" "time-sync.target" ];
    Install.WantedBy = [ "timers.target" ];
    Timer = {
      OnCalendar = "*-*-* 07:00:00";
      Persistent = true;
    };
  };

  inherit script;
}
