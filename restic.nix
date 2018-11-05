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

    export RESTIC_REPOSITORY="b2:${secrets.restic.b2bucket.name}:/photos";
    export RESTIC_PASSWORD_FILE=${restic-pw-file};
    export B2_ACCOUNT_ID=${secrets.restic.b2bucket.account-id};
    export B2_ACCOUNT_KEY=${secrets.restic.b2bucket.account-key};

    ${pkgs.restic}/bin/restic backup --exclude-file=${excludefile} /home/markus/Photos
  '';
in
{
  environment = {
    systemPackages = [ resticScript ];
  };
}
