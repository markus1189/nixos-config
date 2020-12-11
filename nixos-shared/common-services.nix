{ config, pkgs, ...}:

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
      localuser = config.lib._custom_.userName;
    };

    sysstat = {
      enable = false;
    };

    tailscale = {
      enable = true;
    };

    xserver = {
      layout = "us";
      xkbVariant = "altgr-intl";
      xkbOptions = "eurosign:e,caps:ctrl_modifier";
    };
  };
}
