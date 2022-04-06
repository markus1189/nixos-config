{ config, pkgs, ... }:

let secrets = import ../nixos-shared/secrets.nix;
in {
  systemd = {
    services = {
      rclonePremiumizeMount = let
        mountPoint = "/home/${config.lib._custom_.userName}/mounts/rclone/premiumize";
        configFile = pkgs.writeText "rclone-config" secrets.rclone.premiumize;
      in {
        description = "Rclone mount for premiumize";
        serviceConfig = {
          User = config.lib._custom_.userName;
          Group = "users";
          ExecStop = "/run/wrappers/bin/fusermount -u ${mountPoint}";
          ExecStartPre = "${pkgs.coreutils}/bin/mkdir -p ${mountPoint}";
          ExecStart = ''
            ${pkgs.rclone}/bin/rclone mount \
              --config ${configFile} \
              --vfs-cache-mode full \
              --vfs-cache-max-age 48h \
              --vfs-read-chunk-size 128M \
              --vfs-read-chunk-size-limit 512M \
              premiumize: \
              ${mountPoint}
          '';
          Restart = "always";
          Environment = [
            "PATH=/run/wrappers/bin/:$PATH" # required for fusermount setuid wrapper ...
          ];
        };
        wantedBy = [ "network-online.target" ];
      };
    };
  };
}
