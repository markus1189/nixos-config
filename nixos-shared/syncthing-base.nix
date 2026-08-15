{ config, pkgs, ... }:

# Base Syncthing service (enable, dirs, user) shared by all hosts.
# The declarative device/folder mesh lives in ./syncthing.nix and is
# opt-in per host on top of this.
{
  services.syncthing = {
    enable = true;
    package = pkgs.syncthing;
    configDir = "/home/${config.my.userName}/.config/syncthing";
    dataDir = "/home/${config.my.userName}/Sync";
    openDefaultPorts = true;
    systemService = true;
    user = "${config.my.userName}";
  };
}
