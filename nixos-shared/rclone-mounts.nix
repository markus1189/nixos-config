{ config, pkgs, ... }:

let
  mkMount =
    {
      mountPoint,
      configFile,
      remote,
    }:
    {
      description = "Rclone mount for ${remote}";
      serviceConfig = {
        User = config.my.userName;
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
            --daemon-timeout 1m \
            ${remote}: \
            ${mountPoint}
        '';
        Restart = "on-failure";
        RestartSec = "10s";
        Environment = [
          "PATH=/run/wrappers/bin/:$PATH" # required for fusermount setuid wrapper ...
        ];
      };
      wantedBy = [ "network-online.target" ];
    };

in
{
  # Provides the fusermount/fusermount3 setuid wrappers rclone mount needs.
  # Defaulted to true in nixpkgs until 0e251e2 flipped it off.
  programs.fuse.enable = true;

  age = {
    secrets = {
      rclonePremiumize = {
        file = ../secrets/rclone-premiumize.age;
        name = "rclone/premiumize";
        owner = config.my.userName;
      };

      rcloneGDrive = {
        file = ../secrets/rclone-gdrive.age;
        name = "rclone/gdrive";
        owner = config.my.userName;
      };
    };
  };

  systemd = {
    services = {
      rclonePremiumizeMount = mkMount {
        mountPoint = "/home/${config.my.userName}/mounts/rclone/premiumize";
        configFile = config.age.secrets.rclonePremiumize.path;
        remote = "premiumize";
      };

      rcloneGdriveMount = mkMount {
        mountPoint = "/home/${config.my.userName}/mounts/rclone/gdrive";
        configFile = config.age.secrets.rcloneGDrive.path;
        remote = "gdrive";
      };
    };
  };
}
