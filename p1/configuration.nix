{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ../laptop/laptop.nix
    ./p1.nix
  ];
}
