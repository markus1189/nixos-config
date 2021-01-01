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

  boot = {
    kernelPackages = pkgs.linuxPackages_latest;
  };

  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking = {
    hostName = "nixos-p1";
  };

  networking.interfaces.${config.lib._custom_.wirelessInterface}.useDHCP = true;

  system.stateVersion = "20.09";

  hardware.opengl.extraPackages = with pkgs; [
    intel-ocl
  ];

  programs.steam.enable = true;

  services = {
    fprintd.enable = false;

    acpid = {
      enable = true;
      handlers =  {
        acDisconnect = {
          event = "ac_adapter ACPI0003:00 00000080 00000000";
          action = ''
            echo -n 700 > /sys/class/backlight/intel_backlight/brightness
            echo -n 0 > /sys/class/leds/tpacpi::kbd_backlight/brightness
          '';
        };

        acConnect = {
          event = "ac_adapter ACPI0003:00 00000080 00000001";
          action = ''
            cat /sys/class/backlight/intel_backlight/max_brightness > /sys/class/backlight/intel_backlight/brightness
            echo -n 2 > /sys/class/leds/tpacpi::kbd_backlight/brightness
          '';
        };
      };
    };
  };
}
