{
  config,
  lib,
  pkgs,
  ...
}:

{
  options.my.resticPhotoBackupDir = lib.mkOption {
    type = lib.types.str;
    description = "Directory the nightly restic photo backup archives";
  };

  config = {
    systemd = {
      services = {
        resticPhotoBackup = {
          description = "Restic photo backup (${config.my.resticPhotoBackupDir})";
          script = "${pkgs.resticPhotoBackup}/bin/restic-photo-backup ${config.my.resticPhotoBackupDir}";
          startAt = "*-*-* 00:01:00";
        };

        resticPhotoForget = {
          description = "Restic photo forget";
          script = "${pkgs.resticPhotoForget}/bin/restic-photo-forget";
          startAt = "Wed, 5:00:00";
        };
      };
    };
  };
}
