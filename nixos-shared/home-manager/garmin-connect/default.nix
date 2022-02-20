{ targetDir, password }:
{ writeScriptBin, python310 }:
let
  scriptName = "garmin-connect-fetch";
  # fetchScript = writers.writePython3Bin scriptName {
  #   libraries = [ python310Packages.garminconnect python310Packages.dateutil python310Packages.requests ];
  # } (builtins.readFile ./garmin-connect-fetch.py);
  fetchScript = writeScriptBin "garmin-connect-fetch" ''
    #!${myPython}/bin/python3

    ${builtins.readFile ./garmin-connect-fetch.py}
  '';
  myPython =
    python310.withPackages (ps: with ps; [ garminconnect dateutil requests ]);
in {
  service = {
    Unit = { Description = "Sync activities from Garmin Connect"; };

    Install = { WantedBy = [ "network-online.target" ]; };

    Service = {
      Type = "oneshot";
      Environment = [
        "GARMIN_CONNECT_TARGET_DIR=${targetDir}"
        "GARMIN_CONNECT_USER=markus1189@gmail.com"
        "GARMIN_CONNECT_PASSWORD=${password}"
      ];
      ExecStart = "${fetchScript}/bin/${scriptName}";
    };
  };

  timer = {
    Unit = {
      After = ["time-set.target" "time-sync.target"];
    };

    Install = {
      WantedBy = [ "network-online.target" ];
    };

    Timer = {
      OnCalendar = "*-*-* 9,14,21:00:00";
      Persistent = true;
    };
  };

  script = fetchScript;
}
