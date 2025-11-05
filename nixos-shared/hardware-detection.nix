{ config, lib, pkgs, ... }:

{
  options = {
    lib._custom_.wirelessInterface = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      example = "wlp2s0";
      description = ''
        The name of the wireless network interface.

        To find your wireless interface name, run:
          ip link show | grep -E '^[0-9]+: wl'

        Common patterns:
        - wlp2s0 (PCI bus 2, slot 0)
        - wlp0s20f3 (PCI bus 0, device 20, function 3)
        - wlp58s0 (PCI bus 58, slot 0)

        This value is used for:
        - WPA supplicant configuration (networking.supplicant)
        - NetworkManager configuration
        - Custom scripts (wpa_cli aliases)
      '';
    };
  };

  config = {
    # Provide a helper script to detect wireless interfaces
    environment.systemPackages = [
      (pkgs.writeScriptBin "detect-wireless-interface" ''
        #!/usr/bin/env bash
        echo "Detecting wireless network interfaces..."
        echo ""

        # Show all wireless interfaces
        interfaces=$(ip link show | grep -oP '^\d+: \Kwl\w+')

        if [ -z "$interfaces" ]; then
          echo "No wireless interfaces found (interfaces starting with 'wl')"
          echo ""
          echo "All network interfaces:"
          ip link show | grep -oP '^\d+: \K\w+'
          exit 1
        fi

        echo "Found wireless interface(s):"
        for iface in $interfaces; do
          echo "  - $iface"
        done

        echo ""
        echo "To use in your configuration, set:"
        echo "  lib._custom_.wirelessInterface = \"$(echo $interfaces | head -n1)\";"
      '')
    ];
  };
}
