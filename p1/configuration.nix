{ config, pkgs, ... }:

{
  imports = [
    ../nixos-shared/packages/kmonad/service.nix
    ./hardware-configuration.nix
    ../laptop/laptop.nix
    ./p1.nix
    ./globalprotect/default.nix
  ];

  hardware.graphics.extraPackages = with pkgs; [
    intel-compute-runtime
  ];


  # services.udev = {
  #   # for digispark
  #   extraRules = ''
  #     SUBSYSTEMS=="usb", ATTRS{idVendor}=="16d0", ATTRS{idProduct}=="0753", MODE:="0666"
  #     KERNEL=="ttyACM*", ATTRS{idVendor}=="16d0", ATTRS{idProduct}=="0753", MODE:="0666", ENV{ID_MM_DEVICE_IGNORE}="1"
  #   '';
  # };


}
