{ config, lib, pkgs, ... }:

let
  kmonadSrc = config.lib._custom_.ndtSources.kmonad;
  kmonadPackage = pkgs.haskellPackages.kmonad;
  mutatedPatch = (pkgs.mutate ./githash.patch {
    version = kmonadSrc.rev;
  });
  kmonad = (with pkgs.haskell.lib;
     (overrideSrc (dontCheck (unmarkBroken (doJailbreak kmonadPackage))) {
      src = kmonadSrc;
     })).bin;
  myConfigExternal = pkgs.mutate ./markus.kbd {
    inputDeviceFile =
      "/dev/input/by-id/usb-Lenovo_ThinkPad_Compact_USB_Keyboard_with_TrackPoint-event-kbd";
  };
  myConfigInternal = pkgs.mutate ./markus.kbd {
    inputDeviceFile = "/dev/input/by-path/platform-i8042-serio-0-event-kbd";
  };

  mykmonadexternal = "mykmonadexternal";

  lenovoKeyboardUdevName =
    "Lenovo ThinkPad Compact USB Keyboard with TrackPoint";
in rec {
  systemd.services.${mykmonadexternal} = {
    description = "my custom kmonad unit for external";
    serviceConfig = {
      ExecStart = "${kmonad}/bin/kmonad ${myConfigExternal}";
      Restart = "always";
      wantedBy = [ "multi-user.target" ];
    };
  };

  services.udev = {
    extraRules = ''
      ACTION=="add", ATTRS{name}=="${lenovoKeyboardUdevName}", RUN="${pkgs.systemd}/bin/systemctl --no-block start ${mykmonadexternal}.service"
      ACTION=="remove", ATTRS{name}=="${lenovoKeyboardUdevName}", RUN="${pkgs.systemd}/bin/systemctl --no-block stop ${mykmonadexternal}.service"
    '';
  };

  systemd.services.mykmonadinternal = {
    description = "my custom kmonad unit for internal";
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      ExecStart = "${kmonad}/bin/kmonad ${myConfigInternal}";
      Restart = "always";
      wantedBy = [ "multi-user.target" ];
    };
  };
}
