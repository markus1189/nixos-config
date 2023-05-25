{ config, pkgs, ... }:

{
  imports = [
    ../nixos-shared/packages/kmonad/service.nix
    ./hardware-configuration.nix
    ../laptop/laptop.nix
    ./p1.nix
    ./globalprotect/default.nix
  ];
}
