backupDir:
{ config, pkgs, ...}:

{
  systemd = {
   services = {
     resticPhotoBackup = {
       description = "Restic photo backup (${backupDir})";
       script = "${pkgs.resticPhotoBackup}/bin/restic-photo-backup ${backupDir}";
       startAt = "*-*-* 00:01:00";
     };

     resticPhotoForget = {
       description = "Restic photo forget";
       script = "${pkgs.resticPhotoForget}/bin/restic-photo-forget";
       startAt = "Wed, 5:00:00";
     };
   };
  };
}
