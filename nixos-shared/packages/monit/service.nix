{ config, pkgs, ...}:

{
  services = {
    monit = {
      enable = true;
      config = ''
        set httpd
             port 2812
             use address 127.0.0.1
             allow localhost

        set daemon 60

        check program smoketest with path ${pkgs.coreutils}/bin/true
             if status != 0 then exec "${pkgs.libnotify}/bin/notify-send 'Monit Alert' 'Executing true failed!'"
      '';
    };
  };
}
