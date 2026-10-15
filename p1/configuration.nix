{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ../laptop/laptop.nix
    ./p1.nix
  ];

  # Secret is wg-nyc-p1.age, but this host's hostname is nixos-p1 —
  # so the wgProfile default (hostName) would miss.
  my.wgProfile = "p1";

  home-manager.users.${config.my.userName}.imports = [ ./home.nix ];

}
