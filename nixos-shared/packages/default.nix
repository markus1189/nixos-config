{ config, pkgs, ... }:
let
  wirelessInterface = pkgs.lib.head config.networking.wireless.interfaces;
  secrets = import ../secrets.nix;
  pkgsRelease19 = import (fetchTarball https://github.com/NixOS/nixpkgs/archive/release-19.03.tar.gz) {};
in
{
  nixpkgs = {
    config = rec {
      packageOverrides = pkgs:
      let
        allPkgs = nixpkgs: nixpkgs // myScripts // pkgs.xorg // {
          xmobarLower = xmobars.lower;
          xmobarUpper = xmobars.upper;
          xmobar = pkgs.haskellPackages.xmobar;
          xkill = pkgs.xorg.xkill;
        };
        callPackageWith = nixpkgs: nixpkgs.lib.callPackageWith (allPkgs nixpkgs);
        callPackage = callPackageWith pkgs;

        myScripts = pkgs.callPackage ./scripts { };
        xmobars = callPackage ./xmobarrc { inherit mutate; };
        mutate = callPackage ./mutate { };
    in rec {
        inherit myScripts mutate;
        notifySendPb = myScripts.notifySendPb secrets.pushBulletToken;
        myConfigFiles = {
          xmonad = callPackage ./xmonad {
            autoMonitorConfig = myScripts.autoMonitorConfig;
            inherit mutate;
            greenclip = pkgs.haskellPackages.greenclip.overrideAttrs (old: rec {
              buildInputs = old.buildInputs ++ (with pkgs.xorg; [ libXdmcp libX11 libXrandr libXext]);
            });
            chooseNetwork = myScripts.chooseNetwork wirelessInterface;
          };
          xmobarLower = xmobars.lower;
          xmobarUpper = xmobars.upper;
          gitconfig = callPackage ./git { inherit mutate; };
          offlineimap = callPackage ./offlineimap { inherit mutate; googlepw = secrets.googlepw; };
          keynavrc = callPackage ./keynavrc { inherit mutate; };
        };
        emacs = callPackageWith pkgs ./emacs { inherit mutate; };
      };
    };
  };
}
