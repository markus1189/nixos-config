{ config, lib, pkgs, ... }:

let
  kmonad = pkgs.haskell.lib.unmarkBroken (pkgs.haskell.lib.doJailbreak pkgs.haskellPackages.kmonad);
  myConfigExternal = pkgs.mutate ./markus.kbd { inputDeviceFile = "/dev/input/by-id/usb-Lenovo_ThinkPad_Compact_USB_Keyboard_with_TrackPoint-event-kbd"; };
  myConfigInternal = pkgs.mutate ./markus.kbd { inputDeviceFile = "/dev/input/event0"; };
in

{
  systemd.services.mykmonadexternal = {
    description = "my custom kmonad unit for external";
    serviceConfig = {
      ExecStart = "${kmonad}/bin/kmonad ${myConfigExternal}";
      Restart = "always";
    };
  };

  systemd.services.mykmonadinternal = {
    description = "my custom kmonad unit for internal";
    serviceConfig = {
      ExecStart = "${kmonad}/bin/kmonad ${myConfigInternal}";
      Restart = "always";
    };
  };
}
