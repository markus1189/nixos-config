{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix # hardware
    ../laptop/laptop.nix # generic laptop things
    ./xps.nix # xps specific
  ];
}
