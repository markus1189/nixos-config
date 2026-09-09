{ pkgs, ... }:

{
  systemd = {
    services.healthcheck-ping = {
      description = "healthchecks.io liveness ping";

      script = ''
        ${pkgs.curl}/bin/curl -sS --fail --retry 3 --max-time 30 \
          --cacert ${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt \
          https://hc-ping.com/6656b215-0e49-48ff-9af0-a79c64faab9f
      '';

      serviceConfig = {
        Type = "oneshot";
        DynamicUser = true;
      };

      after = [ "network-online.target" ];
      wants = [ "network-online.target" ];

      startAt = "*:0/5";
    };

    timers.healthcheck-ping = {
      description = "Five-minute healthchecks.io liveness ping";
      timerConfig.AccuracySec = "30s";
    };
  };
}
