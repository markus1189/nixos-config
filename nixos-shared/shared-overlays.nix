rec {
  stableNixpkgsOverlay = self: super: {
    stableNixpkgs = import self.ndtSources.nixpkgs-2003 { };
  };

  nixpkgs1703Overlay = self: super: {
    nixpkgs1703 = import self.ndtSources.nixpkgs-1703 { };
  };

  bukuOverlay = self: super: {
    buku =
      builtins.trace "INFO: using pinned buku version" self.stableNixpkgs.buku;
  };

  ndtOverlay = self: super: {
    ndt = import (builtins.fetchTarball
      "https://github.com/markus1189/ndt/archive/master.tar.gz") {
        nixpkgs = self;
      };
  };

  ndtSourcesOverlay = self: super: {
    ndtSources = import ../ndt/sources.nix { };
  };

  dunstWithDunstify = self: super: {
    dunst = super.dunst.override { dunstify = true; };
  };

  temporaryDdgrFix = self: super: {
    ddgr = builtins.trace
      "INFO: using overlay for ddgr until https://github.com/NixOS/nixpkgs/pull/93928"
      (self.callPackage (builtins.fetchurl
        "https://raw.githubusercontent.com/markus1189/nixpkgs/ddgr-1-9/pkgs/applications/misc/ddgr/default.nix")
        { });
  };

  overlays = [
    bukuOverlay
    ndtOverlay
    ndtSourcesOverlay
    stableNixpkgsOverlay
    nixpkgs1703Overlay
    dunstWithDunstify
    temporaryDdgrFix
  ];
}
