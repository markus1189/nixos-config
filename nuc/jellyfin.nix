{ config, pkgs, ...}:

{
  services.jellyfin = {
    enable = true;
    user = "mediacenter";
    group = "users";
    package = pkgs.jellyfin;
  };

  networking.firewall.allowedTCPPorts = [
    8096
  ];
}
