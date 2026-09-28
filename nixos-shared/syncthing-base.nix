{ config, pkgs, ... }:

# Base Syncthing service (enable, dirs, user) shared by all hosts.
# Imported by ./syncthing.nix, which adds the declarative
# device/folder mesh; hosts import that, not this file.
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
