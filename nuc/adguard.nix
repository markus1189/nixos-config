{ config, pkgs, ... }:

{
  environment.systemPackages = [ pkgs.adguardhome ];

  networking.firewall = {
    allowedTCPPorts = [ 3000 3001 53 ];
    allowedUDPPorts = [ 53 ];
  };
}
