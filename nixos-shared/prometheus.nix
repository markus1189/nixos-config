{ config, pkgs, ... }:

let
  secrets = import ../nixos-shared/secrets.nix;
  shellyPlugModule = "shellyPlug";
  jsonExporterPort = 7979;
  toYAMLFile = data:
    (pkgs.writeText "prometheus-rule-file"
      (pkgs.lib.generators.toYAML { } data));
  shellyPlugMetricPrefix = "shelly_plug";
  shellyPlugType = "shelly_plug";

  shellyUniModule = "shellyUni";
  shellyUniMetricPrefix = "shelly_uni";
  shellyUniType = "shelly_uni";

  rooms = {
    markus = "Markus Zimmer";
    kitchen = "Kueche";
    waschkueche = "waschkueche";
  };
  devices = {
    light = "light";
    dishwasher = "spuehlmaschine";
  };
in {
  services.prometheus = rec {
    enable = true;

    globalConfig = {
      evaluation_interval = "30s";
      scrape_interval = "30s";
    };

    retentionTime = "8d";

    alertmanagers = [{
      scheme = "http";
      static_configs = [{
        targets =
          [ "${alertmanager.listenAddress}:${toString alertmanager.port}" ];
      }];
    }];

    ruleFiles = [
      (toYAMLFile {
        groups = [
          {
            name = "shelly-plugs";
            rules = [
              # {
              #   alert = "Spühlmaschine ist fertig";
              #   expr = let
              #     m = ''
              #       ${shellyPlugMetricPrefix}_power{device="${devices.dishwasher}",room="${rooms.kitchen}",type="${shellyPlugType}"}'';
              #   in ''
              #       ${m} < 1
              #     and
              #       ${m} > 0
              #     and
              #       ((${m} offset 10m) < 30)
              #   '';
              #   for = "60s";
              # }
              {
                alert = "ShellyHasUpdate";
                expr =
                  "shelly_plug_has_update > 0 or shelly_uni_has_update > 0";
                for = "30m";
              }
            ];
          }
          # {
          #   name = "shelly-unis";
          #   rules = [
          #     {
          #       alert = "WindowStillClosed";
          #       expr = ''
          #         shelly_uni_ison1{room="Markus Zimmer"} > 0 and on() hour() >= 15'';
          #       for = "10m";
          #     }
          #   ];
          # }
        ];
      })
    ];

    scrapeConfigs = [
      {
        job_name = "shellyViaJsonExporter";
        scrape_interval = "10s";
        metrics_path = "/probe";
        params = { module = [ shellyPlugModule ]; };
        static_configs = [
          {
            targets = [ "http://192.168.178.22/status" ];
            labels = {
              type = shellyPlugType;
              room = rooms.markus;
              device = "light";
            };
          }
          {
            targets = [ "http://192.168.178.34/status" ];
            labels = {
              type = shellyPlugType;
              room = rooms.kitchen;
              device = devices.dishwasher;
            };
          }
          {
            targets = [ "http://192.168.178.31/status" ];
            labels = {
              type = shellyPlugType;
              room = rooms.waschkueche;
              device = "sp3";
            };
          }
          {
            targets = [ "http://192.168.178.28/status" ];
            labels = {
              type = shellyPlugType;
              room = rooms.waschkueche;
              device = "sp4";
            };
          }
        ];
        relabel_configs = [
          {
            source_labels = [ "__address__" ];
            target_label = "__param_target";
          }
          {
            source_labels = [ "__param_target" ];
            target_label = "instance";
          }
          {
            target_label = "__address__";
            replacement =
              "${exporters.json.listenAddress}:${toString exporters.json.port}";
          }
        ];
      }
      {
        job_name = "shellyUniViaJsonExporter";
        scrape_interval = "10s";
        metrics_path = "/probe";
        params = { module = [ shellyUniModule ]; };
        static_configs = [{
          targets = [ "http://192.168.178.29/status" ];
          labels = {
            type = shellyUniType;
            room = rooms.markus;
            device = "uni1";
          };
        }];
        relabel_configs = [
          {
            source_labels = [ "__address__" ];
            target_label = "__param_target";
          }
          {
            source_labels = [ "__param_target" ];
            target_label = "instance";
          }
          {
            target_label = "__address__";
            replacement =
              "${exporters.json.listenAddress}:${toString exporters.json.port}";
          }
        ];
      }
      {
        job_name = "jsonExporterStatus";
        static_configs = [{
          targets = [
            "${exporters.json.listenAddress}:${toString exporters.json.port}"
          ];
        }];
      }
    ];

    alertmanager = {
      enable = true;
      port = 9093;
      listenAddress = "0.0.0.0";
      configuration = rec {
        route = {
          group_wait = "30s";
          group_interval = "5m";
          repeat_interval = "3h";
          receiver = (builtins.elemAt receivers 0).name;
        };

        receivers = [{
          name = "smart-home-telegram";
          telegram_configs = [{
            bot_token = secrets.telegramBotToken;
            chat_id = -1001896177541;
            api_url = "https://api.telegram.org";
            parse_mode = "HTML";
            message = ''
              {{ if gt (len .Alerts.Firing) 0 }}🚨 {{if gt (len .Alerts.Firing) 1 }}Aktive Alarme{{else}}Aktiver Alarm{{end}} ({{ len .Alerts.Firing }})
              {{ range .Alerts.Firing }}- {{ .Labels.alertname }}
              {{ end }}{{ end }}
              {{ if gt (len .Alerts.Resolved) 0 }}✅ {{if gt (len .Alerts.Resolved) 1 }}Erledigte Alarme{{else}}Erledigter Alarm{{end}} ({{ len .Alerts.Resolved }})
              {{ range .Alerts.Resolved }}- {{ .Labels.alertname }}
              {{ end }}{{ end }}
            ''; # see https://gotemplate.io/
          }];
        }];
      };
    };

    exporters = {
      json = {
        enable = true;
        port = jsonExporterPort;
        listenAddress = "0.0.0.0";
        configFile = toYAMLFile {
          modules = {
            "${shellyPlugModule}" = {
              metrics = [{
                name = shellyPlugMetricPrefix;
                type = "object";
                path = "{@}";
                values = {
                  power = "{.meters[0].power}";
                  temperature = "{.temperature}";
                  ram_free = "{.ram_free}";
                  fs_free = "{.fs_free}";
                  meter_total = "{.meters[0].total}";
                  ison = "{.relays[0].ison}";
                  has_update = "{.has_update}";
                };
              }];
              http_client_config = {
                basic_auth = {
                  inherit (secrets.shellyWebUI) username password;
                };
              };
            };
            "${shellyUniModule}" = {
              metrics = [{
                name = shellyUniMetricPrefix;
                type = "object";
                path = "{@}";
                values = {
                  ison1 = "{.relays[0].ison}";
                  ison2 = "{.relays[1].ison}";
                  adcs = "{.adcs[0].voltage}";
                  temperature = "{.ext_temperature.0.tC}";
                  humidity = "{.ext_humidity.0.hum}";
                  has_update = "{.has_update}";
                  ram_free = "{.ram_free}";
                  fs_free = "{.fs_free}";
                  uptime = "{.uptime}";
                };
              }];
              http_client_config = {
                basic_auth = {
                  inherit (secrets.shellyWebUI) username password;
                };
              };
            };
          };
        };
      };
    };
  };
}
