{ config, pkgs, ...}:

{
  services.jellyfin = {
    enable = true;
    user = "mediacenter";
    group = "users";
  };

  networking.firewall.allowedTCPPorts = [
    8096
  ];
}
