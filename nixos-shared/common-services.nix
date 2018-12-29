userName:
{ config, pkgs, ...}:

{
  services = {
    atd.enable = true;

    cron = {
      enable = true;
      mailto = userName;
    };

    udisks2.enable = true;

    unclutter.enable = true;

    locate = {
      enable = true;
      interval = "hourly";
      localuser = userName;
    };

    sysstat = {
      enable = false;
    };

    xserver = {
      layout = "us";
      xkbVariant = "altgr-intl";
      xkbOptions = "eurosign:e,caps:ctrl_modifier,grp:rctrl_switch";
    };
  };
}
