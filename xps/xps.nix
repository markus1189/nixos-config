{ config, pkgs, ...}:

let
  usrPkgs = pkgs.callPackage ../nixos-shared/packages/scripts { };
  wirelessInterface = pkgs.lib.head config.networking.wireless.interfaces;
in
{
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

    initrd.luks.devices = [
      {
        name = "root";
        device = "/dev/nvme0n1p4";
        preLVM = true;
      }
    ];
  };

  environment = {
    shellAliases = (with pkgs; {
      wpa_cli = "${wpa_supplicant}/bin/wpa_cli -i ${wirelessInterface}";
    });
  };

  networking = rec {
    hostName = "nixos-xps";

    wireless = {
      interfaces = [ "wlp2s0" ];
    };
  };

  hardware = {
    bluetooth = {
      extraConfig = ''
        [General]
        ControllerMode = bredr

        [Policy]
        AutoEnable = true
      '';
    };

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
        tapping = false;
        disableWhileTyping = true;
        naturalScrolling = true;
      };
    };
  };

}
