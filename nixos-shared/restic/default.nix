{ writeText, lib, writeScriptBin, stdenv, restic, coreutils, myScripts }:
let
  secrets = import ../secrets.nix;
  excludefile = writeText "restic-excludefile" (lib.strings.concatStringsSep "\n" [
    ".git"
    ".shake"
  ] + "\n");
  restic-pw-file = writeText "restic-PW-file" secrets.restic.b2bucket.password;
  configuredRestic = args: ''
    #!${stdenv.shell}

    export RESTIC_REPOSITORY="b2:${secrets.restic.b2bucket.name}:/photos";
    export RESTIC_PASSWORD_FILE=${restic-pw-file};
    export B2_ACCOUNT_ID=${secrets.restic.b2bucket.account-id};
    export B2_ACCOUNT_KEY=${secrets.restic.b2bucket.account-key};

    echo "[$(date)] Started restic command"
    ${restic}/bin/restic --verbose ${lib.strings.concatStringsSep " " args}
    echo "[$(date)] Finished restic command [$?]"
    ${myScripts.notifySendTelegram secrets.pushBulletToken}/bin/notifySendTelegram "Restic finished! Date: $(${coreutils}/bin/date) with args: ${toString args}"
  '';
in
{
  resticPhotoBackup = writeScriptBin "restic-photo-backup" (configuredRestic [
    "backup"
    "--exclude-file=${excludefile}"
    ''''${1:?no directory to backup given}''
  ]);
  resticPhotoForget = writeScriptBin "restic-photo-forget" (configuredRestic [
    "forget"
    "--dry-run"
    "--keep-last=3"
    "--prune"
  ]);
}
