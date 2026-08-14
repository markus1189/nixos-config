{
  config,
  options,
  pkgs,
  ...
}:

{
  services = {
    atd.enable = true;

    cron = {
      enable = true;
      mailto = config.my.userName;
    };

    udisks2.enable = true;

    unclutter-xfixes.enable = true;

    locate = {
      enable = true;
      # not "daily": 00:00 is always suspend time, so it fires at lid-open
      interval = "20:00";
      # updatedb does not prune btrfs, so each snapshot indexes as a full tree
      prunePaths = options.services.locate.prunePaths.default ++ [
        "/.snapshots"
        "/home/.snapshots"
        "/var/lib/docker"
        "/var/lib/containers"
      ];
    };

    sysstat = {
      enable = false;
    };

    tailscale = {
      enable = false;
    };

    xserver = {
      xkb = {
        layout = "us";
        variant = "altgr-intl";
        options = "eurosign:e";
      };
    };
  };
}
