{ config, pkgs, ...}:

{
  systemd = {
    timers = {
      suspend-on-low-battery = {
        wantedBy = [ "multi-user.target" ];
        timerConfig = {
          OnUnitActiveSec = "120";
        };
      };
    };

    services = {
      suspend-on-low-battery =
        let battery-level-sufficient = lowBattery: pkgs.writeShellScriptBin "battery-level-sufficient" ''
            set -o pipefail
            set -e

            BATTERY_STATUS=$(${pkgs.acpi}/bin/acpi -b | ${pkgs.gawk}/bin/awk -F'[:,%]' '{print $2}')
            BATTERY_PERCENT=$(${pkgs.acpi}/bin/acpi -b | ${pkgs.gawk}/bin/awk -F'[:,%]' '{print $3}')

            test "''${BATTERY_STATUS}" != "Discharging" -o "''${BATTERY_PERCENT}" -ge ${builtins.toString lowBattery}
          '';
        in {
          serviceConfig = { Type = "oneshot"; };
          onFailure = [ "suspend.target" ];
          script = "${battery-level-sufficient 9}/bin/battery-level-sufficient";
        };
    };
  };
}
