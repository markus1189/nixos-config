{ config, pkgs, ... }:

{
  services = {
    globalprotect = {
      enable = true;
      csdWrapper = "/home/markus/repos/otto/vpn/hipreport.sh";
    };
  };
}
