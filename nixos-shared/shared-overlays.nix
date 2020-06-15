let
  stableNixpkgsOverlay = self: super: {
    stableNixpkgs = import self.nivSources.nixpkgs-stable {};
  };
  bukuOverlay = self: super: {
    buku = builtins.trace "INFO: using pinned buku version" self.stableNixpkgs.buku;
  };
  nivOverlay = self: super: {
    nivSources = import ../niv/nix/sources.nix;
  };
in

[
  bukuOverlay
  nivOverlay
  stableNixpkgsOverlay
]
