{ config, lib, pkgs, ... }:

let
  kmonadSrc = config.lib._custom_.ndtSources.kmonad;
  kmonad = with pkgs.haskell.lib;
    overrideSrc (unmarkBroken (doJailbreak pkgs.haskellPackages.kmonad)) {
      src = kmonadSrc;
    };
  myConfigExternal = pkgs.mutate ./markus.kbd {
    inputDeviceFile =
      "/dev/input/by-id/usb-Lenovo_ThinkPad_Compact_USB_Keyboard_with_TrackPoint-event-kbd";
  };
  myConfigInternal = pkgs.mutate ./markus.kbd {
    inputDeviceFile = "/dev/input/by-path/platform-i8042-serio-0-event-kbd";
  };

in {
  systemd.services.mykmonadexternal = {
    description = "my custom kmonad unit for external";
    serviceConfig = {
      ExecStart = "${kmonad}/bin/kmonad ${myConfigExternal}";
      Restart = "always";
    };
  };

  systemd.services.mykmonadinternal = {
    description = "my custom kmonad unit for internal";
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      ExecStart = "${kmonad}/bin/kmonad ${myConfigInternal}";
      Restart = "always";
    };
  };
}
