rec {
  stableNixpkgsOverlay = self: super: {
    stableNixpkgs = import self.nivSources.nixpkgs-stable {};
  };
  bukuOverlay = self: super: {
    buku = builtins.trace "INFO: using pinned buku version" self.stableNixpkgs.buku;
  };
  nivOverlay = self: super: {
    nivSources = import ../niv/nix/sources.nix;
  };
  ndtSourcesOverlay = self: super: {
    ndtSources = import ../ndt/sources.nix {};
  };
  dunstWithDunstify = self: super: {
    dunst = super.dunst.override { dunstify = true; };
  };

  overlays = [
    bukuOverlay
    nivOverlay
    ndtSourcesOverlay
    stableNixpkgsOverlay
    dunstWithDunstify
  ];
}
