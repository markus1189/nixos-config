{ config, ... }:

let
  mkMount =
    {
      name,
      uuid,
      fsType ? "ntfs-3g",
    }:
    {
      inherit fsType;
      mountPoint = "/media/${name}";
      device = "/dev/disk/by-uuid/${uuid}";
      # USB disks: without nofail a missing one drops the boot to emergency
      # before sshd. noCheck: ntfs-3g isn't in fsToSkipCheck, so passno 2.
      noCheck = true;
      options = [
        "nofail"
        "nosuid"
        "nodev"
        "nls=utf8"
        # fmask, not umask: umask=000 made every file on all three disks 0777.
        "fmask=027"
        "dmask=027"
        "uid=${toString config.users.users.${config.my.userName}.uid}"
        "gid=${toString config.users.groups.users.gid}"
        "windows_names"
      ];
    };
in
{
  fileSystems = {
    "multimedia1" = mkMount {
      name = "multimedia";
      uuid = "C6B89CABB89C9B8D";
    };

    "multimedia2" = mkMount {
      name = "multimedia2";
      uuid = "9E167A141679EE21";
    };

    "backups" = mkMount {
      name = "backups";
      uuid = "AADEEA03DEE9C7A1";
    };

    # The 2018 WD10JPVX stays attached after the 2026-09-06 move to NVMe.
    # Its ext4 reads "not clean with errors" with an FS error count of
    # 12.7 M and was last fsck'd at install time, so nothing is migrated off
    # it wholesale -- but Syncthing (69 G) and Downloads (31 G) still live
    # only there. SMART is clean (0 reallocated, 0 pending), so reading it is
    # fine; writing to it is not.
    #
    # `ro` so "fetch it later" cannot quietly become "write to it".
    # `nofail` so physically pulling the disk is a hardware change and not a
    # boot failure.
    #
    # If this ever fails with "recovery required on readonly filesystem"
    # after an unclean shutdown, mount it by hand with `-o ro,noload`. Do NOT
    # put noload here: it would skip journal recovery permanently.
    #
    # `noCheck` because `ro` protects the *mount*, not the *device*: without it
    # NixOS emits fstab passno 2, systemd-fsck sees "contains a file system with
    # errors, check forced" and runs a full repairing e2fsck across 12.7 M
    # errors at every boot -- writing to the one disk this block is trying to
    # keep pristine. Observed 2026-09-07 on the first SSD boot: e2fsck ran 31 s
    # before being cancelled, and only a hand-typed fsck.mode=skip at the
    # systemd-boot menu got the machine up.
    "old-hdd" = {
      mountPoint = "/mnt/old";
      device = "/dev/disk/by-uuid/588ec614-3925-475c-9003-d0ca8146e17f";
      fsType = "ext4";
      noCheck = true;
      options = [
        "ro"
        "nofail"
        "noatime"
      ];
    };
  };
}
