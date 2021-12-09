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

  boot = { kernelPackages = pkgs.linuxPackages_latest; };

  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking = { hostName = "nixos-p1"; };

  networking.interfaces.${config.lib._custom_.wirelessInterface}.useDHCP = true;

  system.stateVersion = "20.09";

  hardware = {
    opengl.extraPackages = with pkgs; [ intel-ocl ];
    cpu.intel.updateMicrocode = true;
    trackpoint = {
      device = "TPPS/2 Elan TrackPoint";
      emulateWheel = true;
      enable = true;
      sensitivity = 112;
      speed = 97;
    };
  };

  programs.steam.enable = true;

  services = {
    throttled.enable = true;

    fprintd.enable = false;

    fstrim.enable = true;

    fwupd = { enable = true; };

    dnsmasq = {
      enable = true;
      resolveLocalQueries = false;
      extraConfig = ''
        listen-address=127.0.0.1 # only bind to the localhost IP

        log-queries

        cache-size=1000 # default is 150
        domain-needed # anything without dots in it doesn't get forwarded to DNS

        clear-on-reload
        no-resolv # don't use  /etc/resolv.conf
        no-poll   # don't poll /etc/resolv.conf

        interface=lo
        no-dhcp-interface=lo
        bind-interfaces

        # server=10.64.0.1 # wireguard
        server=1.1.1.1 # if wireguard is inactive
        server=/otto.de/10.79.255.100
        server=/otto.de/10.79.255.200
        server=/ottogroup.com/10.79.255.100
        server=/ottogroup.com/10.79.255.200
        server=/otto.de/# # fallback to default dns
      '';
    };

    acpid = {
      enable = true;
      handlers = {
        acDisconnect = {
          event = "ac_adapter ACPI0003:00 00000080 00000000";
          action = ''
            echo -n 5000 > /sys/class/backlight/intel_backlight/brightness
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
