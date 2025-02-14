{ config, pkgs, ... }:

{
  services = {
    atd.enable = true;

    cron = {
      enable = true;
      mailto = config.lib._custom_.userName;
    };

    udisks2.enable = true;

    unclutter-xfixes.enable = true;

    locate = {
      enable = true;
      interval = "hourly";
    };

    sysstat = { enable = false; };

    tailscale = { enable = false; };

    xserver = {
      xkb = {
        layout = "us";
        variant = "altgr-intl";
        options = "eurosign:e";
      };
    };
  };
}
