{
  writeText,
  lib,
  writeScriptBin,
  stdenv,
  restic,
  curl,
  cacert,
}:
let
  excludefile = writeText "restic-excludefile" (
    lib.strings.concatStringsSep "\n" [
      ".git"
      ".shake"
    ]
    + "\n"
  );
  # /run/agenix/restic-b2.env provides RESTIC_REPOSITORY, RESTIC_PASSWORD,
  # B2_ACCOUNT_ID and B2_ACCOUNT_KEY (see nixos-shared/restic/module.nix)
  configuredRestic = healthcheckId: args: ''
    #!${stdenv.shell}

    set -a
    . /run/agenix/restic-b2.env
    set +a

    export RESTIC_CACHE_DIR="/tmp/restic-cache-dir"

    echo "[$(date)] Started restic command"
    ${restic}/bin/restic --verbose unlock || echo "Unlock operation unsuccessful"

    ${restic}/bin/restic --verbose ${lib.strings.concatStringsSep " " args} &&
      ${curl}/bin/curl --retry 3 --cacert ${cacert}/etc/ssl/certs/ca-bundle.crt "https://hc-ping.com/${healthcheckId}/$?"
  '';
in
{
  resticPhotoBackup = writeScriptBin "restic-photo-backup" (
    configuredRestic "16ec3eb5-482f-45d0-808a-a6fb24304d2a" [
      "backup"
      "--exclude-file=${excludefile}"
      "\${1:?no directory to backup given}"
    ]
  );
  resticPhotoForget = writeScriptBin "restic-photo-forget" (
    configuredRestic "b78e39ed-daf0-4c0c-b599-f8a75dfecff9" [
      "forget"
      "--keep-last=3"
      "--prune"
    ]
  );
}
