{ config, pkgs, ... }:

let
  mkMount = { name, uuid, neededForBoot ? false, fsType ? "ntfs-3g" }: {
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
    ] ++ pkgs.lib.optionals (fsType == "ntfs-3g") [ "big_writes" ];
  };
in {
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
  };
}
