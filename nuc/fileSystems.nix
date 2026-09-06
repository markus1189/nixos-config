{ config, pkgs, ... }:

let
  mkMount =
    {
      name,
      uuid,
      neededForBoot ? false,
      fsType ? "ntfs-3g",
    }:
    {
      inherit neededForBoot fsType;
      mountPoint = "/media/${name}";
      device = "/dev/disk/by-uuid/${uuid}";
      options = [
        "defaults"
        "nls=utf8"
        "umask=000"
        "dmask=027"
        "uid=1000"
        "gid=100"
        "windows_names"
      ]
      ++ pkgs.lib.optionals (fsType == "ntfs-3g") [ "big_writes" ];
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
    "old-hdd" = {
      mountPoint = "/mnt/old";
      device = "/dev/disk/by-uuid/588ec614-3925-475c-9003-d0ca8146e17f";
      fsType = "ext4";
      options = [
        "ro"
        "nofail"
        "noatime"
      ];
    };
  };
}
