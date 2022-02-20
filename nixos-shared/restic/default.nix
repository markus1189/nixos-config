{ writeText, lib, writeScriptBin, stdenv, restic, coreutils, myScripts, curl, cacert }:
let
  secrets = import ../secrets.nix;
  excludefile = writeText "restic-excludefile" (lib.strings.concatStringsSep "\n" [
    ".git"
    ".shake"
  ] + "\n");
  restic-pw-file = writeText "restic-PW-file" secrets.restic.b2bucket.password;
  configuredRestic = healthcheckId: args: ''
    #!${stdenv.shell}

    export RESTIC_CACHE_DIR="/tmp/restic-cache-dir"
    export RESTIC_REPOSITORY="b2:${secrets.restic.b2bucket.name}:/photos"
    export RESTIC_PASSWORD_FILE="${restic-pw-file}"
    export B2_ACCOUNT_ID="${secrets.restic.b2bucket.account-id}"
    export B2_ACCOUNT_KEY="${secrets.restic.b2bucket.account-key}"

    echo "[$(date)] Started restic command"
    ${restic}/bin/restic --verbose unlock || echo "Unlock operation unsuccessful"

    ${restic}/bin/restic --verbose ${lib.strings.concatStringsSep " " args} &&
      ${curl}/bin/curl --retry 3 --cacert ${cacert}/etc/ssl/certs/ca-bundle.crt "https://hc-ping.com/${healthcheckId}/$?"
  '';
in
{
  resticPhotoBackup = writeScriptBin "restic-photo-backup" (configuredRestic "16ec3eb5-482f-45d0-808a-a6fb24304d2a" [
    "backup"
    "--exclude-file=${excludefile}"
    ''''${1:?no directory to backup given}''
  ]);
  resticPhotoForget = writeScriptBin "restic-photo-forget" (configuredRestic "b78e39ed-daf0-4c0c-b599-f8a75dfecff9" [
    "forget"
    "--keep-last=3"
    "--prune"
  ]);
}
