{ config, pkgs, ...}:

let
  wirelessInterface = pkgs.lib.head config.networking.wireless.interfaces;
  secrets = import ./secrets.nix;
  excludefile = pkgs.writeText "restic-excludefile" (pkgs.lib.strings.concatStringsSep "\n" [
    ".git"
    "ordered-*"
    "*.db"
    "web"
    "developed"
    "darktable_exported"
    "converted"
  ] + "\n");
  restic-pw-file = pkgs.writeText "restic-PW-file" secrets.restic.b2bucket.password;
  resticScript = pkgs.writeScript "restic-do-backup" ''
    #!${pkgs.stdenv.shell}

    NETWORK=''$(${pkgs.wpa_supplicant}/bin/wpa_cli -i ${wirelessInterface} status | ${pkgs.gnugrep}/bin/grep '^ssid' | ${pkgs.coreutils}/bin/cut -d'=' -f 2)
    if [[ "''${NETWORK}" == "Our FRITZ Box" ]]; then
      ${pkgs.restic}/bin/restic backup --exclude-file=${excludefile} /home/markus/Photos
    else
      echo "Not connected to home network, refusing to backup!"
      exit 1
    fi
  '';
in
{
  systemd = {
    services = {
      restic-backup-photos = {
        description = "photos backup";
        environment = {
          RESTIC_REPOSITORY = "b2:${secrets.restic.b2bucket.name}:/photos";
          RESTIC_PASSWORD_FILE = restic-pw-file;
          B2_ACCOUNT_ID = secrets.restic.b2bucket.account-id;
          B2_ACCOUNT_KEY = secrets.restic.b2bucket.account-key;
        };
        serviceConfig = {
          Type = "oneshot";
          ExecStart = "${resticScript}";
        };
      };
    };

    timers = {
      restic-backup-photos = {
        description = "photos backup timer";
        wantedBy = [ "timers.target" ];
        timerConfig = {
          Persistent = true;
          OnCalendar = "Sat";
        };
      };
    };
  };
}
