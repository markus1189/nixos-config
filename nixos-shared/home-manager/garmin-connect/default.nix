{
  targetDir,
  environmentFile,
  tokenStore,
}:
{ writeScriptBin, python3 }:
let
  myPython = python3.withPackages (
    ps: with ps; [
      garminconnect
      python-dateutil
      requests
    ]
  );

  scriptName = "garmin-connect-fetch";

  fetchScript = writeScriptBin scriptName ''
    #!${myPython}/bin/python3

    ${builtins.readFile ./garmin-connect-fetch.py}
  '';
in
{
  service = {
    Unit = {
      Description = "Sync activities from Garmin Connect";
    };

    Service = {
      Type = "oneshot";
      # GARMIN_CONNECT_PASSWORD comes from the agenix-decrypted environmentFile,
      # so it stays out of the world-readable nix store.
      EnvironmentFile = environmentFile;
      Environment = [
        "GARMIN_CONNECT_TARGET_DIR=${targetDir}"
        "GARMIN_CONNECT_USER=markus1189@gmail.com"
        "GARMINTOKENS=${tokenStore}"
      ];
      ExecStart = "${fetchScript}/bin/${scriptName}";
    };
  };

  timer = {
    Unit = {
      After = [
        "time-set.target"
        "time-sync.target"
      ];
    };

    # network-online.target does not exist in the user manager, so hooking
    # the timer there left it dead after every reboot.
    Install = {
      WantedBy = [ "timers.target" ];
    };

    Timer = {
      OnCalendar = "*-*-* 9,14,21:00:00";
      Persistent = true;
    };
  };

  script = fetchScript;
}
