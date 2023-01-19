{ config, pkgs, ... }:

{
  imports = [
    ../nixos-shared/packages/kmonad/service.nix
    ./hardware-configuration.nix
    ../laptop/laptop.nix
    ./p1.nix
    ./otto.nix
  ];

  services.globalprotect = {
    enable = true;
    # if you need a Host Integrity Protection report
    # csdWrapper = "${pkgs.openconnect}/libexec/openconnect/hipreport.sh";
  };
}
