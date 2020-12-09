# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  lib = {
    _custom_ = {
      wirelessInterface = "wlp0s20f3";
      name = "p1";
    };
  };

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.initrd.luks.devices = {
    "crypted" = {
      device = "/dev/nvme0n1p2";
      preLVM = true;
    };
  };

  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking = {
    hostName = "nixos-p1";
  };

  networking.interfaces.${config.lib._custom_.wirelessInterface}.useDHCP = true;

  system.stateVersion = "20.09"; # Did you read the comment?
}

