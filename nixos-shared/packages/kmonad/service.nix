{ config, lib, pkgs, ... }:

let
  kmonad = pkgs.haskell.lib.unmarkBroken (pkgs.haskell.lib.doJailbreak pkgs.haskellPackages.kmonad);
  myConfig = pkgs.mutate ./markus.kbd { inputDeviceFile = "/dev/input/event0"; };
in

{
  systemd.services.mykmonad = {
    description = "my custom kmonad unit";
    serviceConfig = {
      ExecStart = "${kmonad}/bin/kmonad ${myConfig}";
      Restart = "always";
    };
  };
}
