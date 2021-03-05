{ config, pkgs, ...}:

{
  environment.systemPackages = [ pkgs.adguard ];

  networking.firewall.allowedTCPPorts = [
    3000
    53
  ];
}
