{ dataDir }:
{ pkgs, lib, opentelemetry-collector-contrib, coreutils }:
let
  configFile = pkgs.writeText "otel-collector-config.yaml"
    (lib.generators.toYAML { } {
      receivers.otlp.protocols = {
        grpc.endpoint = "127.0.0.1:4317";
        http.endpoint = "127.0.0.1:4318";
      };
      exporters.file = {
        path = "${dataDir}/claude-code.jsonl";
        format = "json";
        rotation = {
          max_days = 1;
          max_backups = 30;
        };
      };
      service = {
        pipelines = {
          metrics = {
            receivers = [ "otlp" ];
            exporters = [ "file" ];
          };
          logs = {
            receivers = [ "otlp" ];
            exporters = [ "file" ];
          };
        };
        telemetry.logs.level = "info";
      };
    });
in
{
  service = {
    Unit = {
      Description = "OpenTelemetry Collector for Claude Code";
      After = [ "network.target" ];
    };

    Install.WantedBy = [ "default.target" ];

    Service = {
      Type = "simple";
      ExecStartPre = "${coreutils}/bin/mkdir -p ${dataDir}";
      ExecStart = "${opentelemetry-collector-contrib}/bin/otelcol-contrib --config=${configFile}";
      Restart = "always";
      RestartSec = 5;
      UMask = "0077";
    };
  };

  inherit configFile;
}
