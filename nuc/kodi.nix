{ config, pkgs, ...}:

{
  environment.systemPackages = [ pkgs.kodi ];

  networking.firewall.allowedTCPPorts = [
    42424 # kodi mediacenter
  ];
}
