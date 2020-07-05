rec {
  stableNixpkgsOverlay = self: super: {
    stableNixpkgs = import self.ndtSources.nixpkgs-2003 {};
  };
  bukuOverlay = self: super: {
    buku = builtins.trace "INFO: using pinned buku version" self.stableNixpkgs.buku;
  };
  ndtOverlay = self: super: {
    ndt = import (builtins.fetchTarball https://github.com/markus1189/ndt/archive/master.tar.gz) {
      nixpkgs = self;
    };
  };
  ndtSourcesOverlay = self: super: {
    ndtSources = import ../ndt/sources.nix {};
  };
  dunstWithDunstify = self: super: {
    dunst = super.dunst.override { dunstify = true; };
  };

  overlays = [
    bukuOverlay
    ndtOverlay
    ndtSourcesOverlay
    stableNixpkgsOverlay
    dunstWithDunstify
  ];
}
