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
    kernelParams = [
      "psmouse.synaptics_intertouch=0" # fix touchpad button not working
    ];
  };

  networking = {
    hostName = "nixos-p1";
    interfaces.${config.lib._custom_.wirelessInterface}.useDHCP = true;
    dhcpcd.denyInterfaces = [ "veth*" "docker*" "br-*" ];
  };

  system.stateVersion = "20.09";

  hardware = {
    # 403 on url
    graphics.extraPackages = with pkgs; [ intel-media-driver ];
    cpu.intel.updateMicrocode = true;
    trackpoint = {
      device = "TPPS/2 Elan TrackPoint";
      emulateWheel = true;
      enable = true;
      sensitivity = 112;
      speed = 97;
    };
  };

  # programs.steam.enable = true;

  services = {
    throttled.enable = true;

    fprintd.enable = false;

    fstrim.enable = true;

    fwupd = { enable = true; };

    dnsmasq = {
      enable = true;
      resolveLocalQueries = false;
      settings = {
        listen-address = "127.0.0.1";
        log-queries = true;
        cache-size = 1000; # default = 150
        domain-needed = true;
        clear-on-reload = true;
        no-resolv = true;
        no-poll = true;
        interface = "lo";
        no-dhcp-interface = "lo";
        bind-interfaces = true;
        server = [
          "1.1.1.1"
          "/byod.gp.ottogroup.com/1.1.1.1"
          "/otto.de/10.79.255.100"
          "/otto.de/10.79.255.200"
          "/ottogroup.com/10.79.255.100"
          "/ottogroup.com/10.79.255.200"
          "/otto.de/#"
        ];
      };

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
