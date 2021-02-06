{ config, pkgs, ...}:

{
  environment.systemPackages = [ pkgs.plex ];

  networking.firewall.allowedTCPPorts = [
    32400
  ];
}
