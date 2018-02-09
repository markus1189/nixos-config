{ config, pkgs, ... }:
let
  wirelessInterface = pkgs.lib.head config.networking.wireless.interfaces;
in
{
  nixpkgs = {
    config = rec {
      packageOverrides = pkgs:
      let
        allPkgs = pkgs // myScripts // {
          xmobarLower = xmobars.lower;
          xmobarUpper = xmobars.upper;
          xmobar = pkgs.haskellPackages.xmobar;
          xkill = pkgs.xorg.xkill;
        };
        callPackage = pkgs.lib.callPackageWith allPkgs;

        myScripts = pkgs.callPackage ./scripts { };
        xmobars = callPackage ./xmobarrc { inherit mutate; };
        mutate = callPackage ./mutate { };
      in rec {
        inherit myScripts mutate;
        myConfigFiles = {
          xmonad = callPackage ./xmonad {
            inherit mutate;
            chooseNetwork = myScripts.chooseNetwork wirelessInterface;
          };
          xmobarLower = xmobars.lower;
          xmobarUpper = xmobars.upper;
        };
      };
    };
  };
}
