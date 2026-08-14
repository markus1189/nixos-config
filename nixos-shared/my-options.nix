{ lib, ... }:

# Typed host parameters (formerly the untyped `config.lib._custom_` attrset).
# No defaults on purpose: a host that forgets one fails at option-check time
# with a proper error instead of a mid-eval `attribute missing`.
{
  options.my = {
    userName = lib.mkOption {
      type = lib.types.str;
      description = "Primary login user of this host";
    };

    wirelessInterface = lib.mkOption {
      type = lib.types.str;
      description = "Kernel name of the Wi-Fi interface";
    };
  };
}
