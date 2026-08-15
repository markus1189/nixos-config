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

  # services.udev = {
  #   # for digispark
  #   extraRules = ''
  #     SUBSYSTEMS=="usb", ATTRS{idVendor}=="16d0", ATTRS{idProduct}=="0753", MODE:="0666"
  #     KERNEL=="ttyACM*", ATTRS{idVendor}=="16d0", ATTRS{idProduct}=="0753", MODE:="0666", ENV{ID_MM_DEVICE_IGNORE}="1"
  #   '';
  # };

}
