{ config, pkgs, callPackage, ... }:

{
  services.emacs = {
    enable = false;
    package = pkgs.emacs-unstable;
  };

  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url = "https://github.com/nix-community/emacs-overlay/archive/master.tar.gz";
    }))
  ];
}
