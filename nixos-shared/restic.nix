{ config, pkgs, ...}:

let
  secrets = import ./secrets.nix;
  excludefile = pkgs.writeText "restic-excludefile" (pkgs.lib.strings.concatStringsSep "\n" [
    ".git"
    ".shake"
  ] + "\n");
  restic-pw-file = pkgs.writeText "restic-PW-file" secrets.restic.b2bucket.password;
  resticScript = pkgs.writeScriptBin "restic-do-backup" ''
    #!${pkgs.stdenv.shell}

    DIR="''${1}"

    if [[ -z "$DIR" ]]; then
      echo "Usage: $0 <dir>"
      exit 1
    fi

    export RESTIC_REPOSITORY="b2:${secrets.restic.b2bucket.name}:/photos";
    export RESTIC_PASSWORD_FILE=${restic-pw-file};
    export B2_ACCOUNT_ID=${secrets.restic.b2bucket.account-id};
    export B2_ACCOUNT_KEY=${secrets.restic.b2bucket.account-key};

    echo "[$(date)] Syncing directory: '$DIR'"
    ${pkgs.restic}/bin/restic backup --exclude-file=${excludefile} "$DIR"
    echo "[$(date)] Finished syncing directory: '$DIR'"
  '';
in
{
  environment = {
    systemPackages = [ resticScript ];
  };
}
