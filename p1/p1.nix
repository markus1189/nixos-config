{ config, pkgs, ... }:

{
  my.wirelessInterface = "wlp0s20f3";

  boot.initrd.luks.devices = {
    "crypted" = {
      device = "/dev/nvme0n1p2";
      preLVM = true;
    };
  };

  boot.kernelParams = [
    "psmouse.synaptics_intertouch=0" # fix touchpad button not working
  ];

  networking = {
    hostName = "nixos-p1";
    interfaces.${config.my.wirelessInterface}.useDHCP = true;
    dhcpcd.denyInterfaces = [
      "veth*"
      "docker*"
      "br-*"
    ];
  };

  system.stateVersion = "20.09";

  hardware = {
    cpu.intel.updateMicrocode = true;
  };

  services = {
    throttled.enable = true;

    fprintd.enable = false;

    fwupd = {
      enable = true;
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
