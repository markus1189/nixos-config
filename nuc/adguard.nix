{ config, pkgs, ...}:

{
  environment.systemPackages = [ pkgs.adguardhome ];

  networking.firewall.allowedTCPPorts = [
    3000
    53
  ];
}
