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

      # wantedBy alone is an Install relation and carries no ordering, so the
      # mount could otherwise be started before the network was up.
      wants = [ "network-online.target" ];
      after = [ "network-online.target" ];
      wantedBy = [ "multi-user.target" ];

      # Supplies the fusermount setuid wrappers. Replaces a hand-written
      # Environment = [ "PATH=/run/wrappers/bin/:$PATH" ]: systemd performs no
      # variable expansion, so that added a literal "$PATH" directory and
      # clobbered the generated PATH.
      path = [ "/run/wrappers" ];

      serviceConfig = {
        # rclone signals readiness once the mountpoint is up; under Type=simple
        # systemd considers the unit started before the mount exists.
        Type = "notify";
        User = config.my.userName;

        ExecStartPre = [
          # Defence in depth for a crash that skips ExecStopPost. test -d is
          # false for a stale (ENOTCONN) mountpoint as well as an absent one;
          # unmounting an absent path is harmless.
          "-${pkgs.writeShellScript "rclone-clear-stale-${remote}" ''
            test -d ${mountPoint} || /run/wrappers/bin/fusermount3 -uz ${mountPoint} || true
          ''}"
          "${pkgs.coreutils}/bin/mkdir -p ${mountPoint}"
        ];

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

        # Non-lazy first, so a process still holding the mount shows up in the
        # journal as EBUSY; the "-" keeps that from failing the unit. rclone
        # also unmounts on SIGTERM, so this is mainly diagnostic.
        ExecStop = "-/run/wrappers/bin/fusermount3 -u ${mountPoint}";

        # The guaranteed sweep: runs even when ExecStop or ExecStartPre failed.
        # Safe to be lazy here because the daemon has already exited, and this
        # is what stops a stale mount from looping the unit forever.
        ExecStopPost = "-/run/wrappers/bin/fusermount3 -uz ${mountPoint}";

        # The 90s default can be too short to flush a large vfs writeback
        # cache; a SIGKILL there recreates exactly the stale-mount state.
        TimeoutStopSec = "5min";

        Restart = "on-failure";
        RestartSec = "10s";
      };
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
