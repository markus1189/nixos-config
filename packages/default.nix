{ config, pkgs, ... }:
let
  wirelessInterface = pkgs.lib.head config.networking.wireless.interfaces;
in
{
  nixpkgs = {
    config = rec {
      packageOverrides = pkgs:
      let
        allPkgs = pkgs // myScripts // pkgs.xorg // {
          xmobarLower = xmobars.lower;
          xmobarUpper = xmobars.upper;
          xmobar = pkgs.haskellPackages.xmobar;
          xkill = pkgs.xorg.xkill;
        };
        callPackage = pkgs.lib.callPackageWith allPkgs;

        myScripts = pkgs.callPackage ./scripts { };
        xmobars = callPackage ./xmobarrc { inherit mutate; };
        mutate = callPackage ./mutate { };
        insomnia = callPackage ./insomnia { };
    in rec {
        inherit myScripts mutate insomnia;
        myConfigFiles = {
          xmonad = callPackage ./xmonad {
            inherit mutate;
            chooseNetwork = myScripts.chooseNetwork wirelessInterface;
          };
          xmobarLower = xmobars.lower;
          xmobarUpper = xmobars.upper;
          gitconfig = callPackage ./git { inherit mutate; };
        };
      };
    };
  };
}
