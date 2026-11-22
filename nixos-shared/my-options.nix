{ config, lib, ... }:

# Typed host parameters (formerly the untyped `config.lib._custom_` attrset).
# No defaults on purpose: a host that forgets one fails at option-check time
# with a proper error instead of a mid-eval `attribute missing`.
{
  options.my = {
    userName = lib.mkOption {
      type = lib.types.str;
      description = "Primary login user of this host";
    };

    hostTag = lib.mkOption {
      type = lib.types.str;
      readOnly = true;
      default = lib.removePrefix "nixos-" config.networking.hostName;
      description = "Short host name (`nixos-p1` -> `p1`): ~/Stuff's DD-<host> dirs, flake attr, messages";
    };

    wirelessInterface = lib.mkOption {
      type = lib.types.str;
      description = "Kernel name of the Wi-Fi interface";
    };
  };
}
