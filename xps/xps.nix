{ config, pkgs, ...}:

let
  usrPkgs = pkgs.callPackage ../nixos-shared/packages/scripts { };
in
{
  lib = {
    _custom_ = {
      wirelessInterface = "wlp2s0";
    };
  };

  boot = {
    kernelParams = [
      "acpi_backlight=vendor"
      "acpi_osi=Linux"
      "nouveau.modeset=0" # fix X11 freeze on start, archwiki
    ];
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };

    kernelPackages = pkgs.linuxPackages_latest;

    initrd.luks.devices = {
      # Name of the luks device (from cryptsetup luksOpen)
      "root" = {
        device = "/dev/nvme0n1p4";
        preLVM = true;
      };
    };
  };

  environment = {
    shellAliases = (with pkgs; {
      wpa_cli = "${wpa_supplicant}/bin/wpa_cli -i ${config.lib._custom_.wirelessInterface}";
    });
  };

  networking = rec {
    hostName = "nixos-xps";

    supplicant = {
      "${config.lib._custom_.wirelessInterface}" = {
        configFile.path = "/etc/wpa_supplicant.conf";
      };
    };
  };

  hardware = {
    bumblebee = {
      enable = false;
      driver = "nouveau";
    };
  };

  services = {
    acpid = {
      enable = true;
      handlers = let step = "250"; in {
        videoBrightnessUp = {
          event = "video/brightnessup" ;
          action = ''
            echo -n "$(($(cat /sys/class/backlight/intel_backlight/brightness) + ${step}))" > \
              /sys/class/backlight/intel_backlight/brightness
          '';
        };

        videoBrightnessDown = {
          event = "video/brightnessdown" ;
          action = ''
            echo -n "$(($(cat /sys/class/backlight/intel_backlight/brightness) - ${step}))" > \
              /sys/class/backlight/intel_backlight/brightness
          '';
        };

        acDisconnect = {
          event = "ac_adapter ACPI0003:00 00000080 00000000";
          action = "${usrPkgs.acDisconnected}/bin/acDisconnected";
        };

        acConnect = {
          event = "ac_adapter ACPI0003:00 00000080 00000001";
          action = "${usrPkgs.acConnected}/bin/acConnected";
        };

        toggleSound = {
          event = "button/mute";
          action = "${usrPkgs.toggleSoundMute}/bin/toggleSoundMute";
        };
      };
    };


    xserver = {
      videoDrivers = [ "intel" "vesa" ];

      synaptics = {
        enable = false;
      };

      libinput = {
        enable = true;
        tapping = true;
        disableWhileTyping = true;
        naturalScrolling = false;
      };
    };
  };

}
