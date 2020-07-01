rec {
  stableNixpkgsOverlay = self: super: {
    stableNixpkgs = import self.ndtSources.nixpkgs-2003 {};
  };
  bukuOverlay = self: super: {
    buku = builtins.trace "INFO: using pinned buku version" self.stableNixpkgs.buku;
  };
  ndtSourcesOverlay = self: super: {
    ndtSources = import ../ndt/sources.nix {};
  };
  dunstWithDunstify = self: super: {
    dunst = super.dunst.override { dunstify = true; };
  };

  overlays = [
    bukuOverlay
    ndtSourcesOverlay
    stableNixpkgsOverlay
    dunstWithDunstify
  ];
}
