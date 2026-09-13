{ config, pkgs, ... }:

let
  # Shared by every mount; rclone namespaces beneath it per remote
  # (vfs/<remote>, vfsMeta/<remote>).
  cacheDir = "/home/${config.my.userName}/.cache/rclone";

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

          # The cache must be its own subvolume or snapper pins every block it
          # frees: btrfs snapshots are per-subvolume and all-or-nothing, so a
          # subdirectory of @home cannot be excluded. Without this, deleting
          # 423G of cache reclaimed 0 bytes (2026-09-13) -- the same argument
          # disko.nix already makes for keeping @nix out of snapshots.
          # Snapshots do not recurse into nested subvolumes, so this is the
          # whole fix. Unprivileged `subvolume create` is allowed for the owner
          # of the parent directory, which is why this works as User=.
          # A subvolume root always has inode 256; anything else is a plain
          # directory. Never destructive -- an existing cache is left alone
          # (and warned about) rather than replaced.
          "${pkgs.writeShellScript "rclone-cache-subvol-${remote}" ''
            set -eu
            if [ ! -e ${cacheDir} ]; then
              ${pkgs.coreutils}/bin/mkdir -p "$(${pkgs.coreutils}/bin/dirname ${cacheDir})"
              # Every mount unit runs this and they start in parallel, so they
              # race: the loser gets "Could not create subvolume: File exists".
              # Swallow that and let the inode check below be the only verdict.
              ${pkgs.btrfs-progs}/bin/btrfs subvolume create ${cacheDir} 2>/dev/null \
                || ${pkgs.coreutils}/bin/mkdir -p ${cacheDir}
            fi
            if [ "$(${pkgs.coreutils}/bin/stat -c %i ${cacheDir})" != 256 ]; then
              echo "WARNING: ${cacheDir} is not a btrfs subvolume;" \
                   "snapper will pin every block the VFS cache frees." >&2
            fi
          ''}"
        ];

        # max-age alone cannot bound the cache: it evicts on the ATime rclone
        # records in vfsMeta, and any full pass over the mount (a Stash library
        # scan, say) reads every object and resets that clock on all of them at
        # once. In Sep 2026 that left 423G of premiumize cache the 60s cleaner
        # dutifully reported and never evicted. max-size is the guardrail that
        # survives a scanner: the target is computed from the cache's own size,
        # so it always converges.
        #
        # Deliberately NOT --vfs-cache-min-free-space. That target comes from
        # statfs on a volume snapper also snapshots, so deleting cache frees
        # nothing until the snapshots referencing it are gone. The target stays
        # unmet, and rclone keeps evicting -- it took the cache 4522 objects ->
        # 0 on 2026-09-13 and would do it again on every restart.
        #
        # systemd parses ExecStart itself -- no comments inside the string.
        ExecStart = ''
          ${pkgs.rclone}/bin/rclone mount \
            -v \
            --config ${configFile} \
            --cache-dir ${cacheDir} \
            --vfs-cache-mode full \
            --vfs-cache-max-age 48h \
            --vfs-cache-max-size 50G \
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
