{ targetDir, password }:
{ writeScriptBin, python3 }:
let
  myPython = python3.withPackages (ps:
    with ps; [
      (ps.garminconnect.overridePythonAttrs (old: rec {
        version = "0.2.23";
        src = ps.fetchPypi {
          pname = "garminconnect";
          inherit version;
          hash = "sha256-IQErVwla8GdUtT8wfz1vMoCoaYFoR30VQMyQ4t85Yn0=";
        };
      }))
      python-dateutil
      requests
    ]);

  scriptName = "garmin-connect-fetch";

  fetchScript = writeScriptBin scriptName ''
    #!${myPython}/bin/python3

    ${builtins.readFile ./garmin-connect-fetch.py}
  '';
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
    Unit = { After = [ "time-set.target" "time-sync.target" ]; };

    Install = { WantedBy = [ "network-online.target" ]; };

    Timer = {
      OnCalendar = "*-*-* 9,14,21:00:00";
      Persistent = true;
    };
  };

  script = fetchScript;
}
