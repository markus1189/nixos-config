{ config, lib, pkgs, ... }:

{
  services.kanata = {
    enable = true;
    keyboards.markus = {
      # Name-based matching (instead of `devices = [paths]`) lets kanata
      # pick up the external Lenovo on hotplug without a udev rule.
      extraDefCfg = ''
        process-unmapped-keys no
        linux-dev-names-include (
          "AT Translated Set 2 keyboard"
          "Lenovo ThinkPad Compact USB Keyboard with TrackPoint"
        )
      '';
      config = builtins.readFile ./markus.kbd;
    };
  };
}
