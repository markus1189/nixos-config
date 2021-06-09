userName: { config, pkgs, ...}:

{
  services.syncthing = {
    enable = true;
    package = pkgs.syncthing;
    configDir = "/home/${userName}/.config/syncthing";
    dataDir = "/home/${userName}/Sync";
    openDefaultPorts = true;
    systemService = true;
    user = "${userName}";
  };
}
