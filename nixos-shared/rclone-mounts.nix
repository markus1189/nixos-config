{ config, pkgs, ... }:

let
  mkMount = { mountPoint, configFile, remote }: {
    description = "Rclone mount for ${remote}";
    serviceConfig = {
      User = config.lib._custom_.userName;
      ExecStop = "/run/wrappers/bin/fusermount -u ${mountPoint}";
      ExecStartPre = "${pkgs.coreutils}/bin/mkdir -p ${mountPoint}";
      ExecStart = ''
        ${pkgs.rclone}/bin/rclone mount \
          -v \
          --config ${configFile} \
          --vfs-cache-mode full \
          --vfs-cache-max-age 48h \
          --vfs-read-chunk-size 128M \
          --vfs-read-chunk-size-limit 512M \
          ${remote}: \
          ${mountPoint}
      '';
      Restart = "always";
      Environment = [
        "PATH=/run/wrappers/bin/:$PATH" # required for fusermount setuid wrapper ...
      ];
    };
    wantedBy = [ "network-online.target" ];
  };

in {
  age = {
    secrets = {
      rclonePremiumize = {
        file = ../secrets/rclone-premiumize.age;
        name = "rclone/premiumize";
        owner = config.lib._custom_.userName;
      };

      rcloneGDrive = {
        file = ../secrets/rclone-gdrive.age;
        name = "rclone/gdrive";
        owner = config.lib._custom_.userName;
      };
    };
  };

  systemd = {
    services = {
      rclonePremiumizeMount = mkMount {
        mountPoint =
          "/home/${config.lib._custom_.userName}/mounts/rclone/premiumize";
        configFile = config.age.secrets.rclonePremiumize.path;
        remote = "premiumize";
      };

      rcloneGdriveMount = mkMount {
        mountPoint =
          "/home/${config.lib._custom_.userName}/mounts/rclone/gdrive";
        configFile = config.age.secrets.rcloneGDrive.path;
        remote = "gdrive";
      };
    };
  };
}
