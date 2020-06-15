let
  pkgsRelease2003 = import (fetchTarball https://github.com/NixOS/nixpkgs/archive/release-20.03.tar.gz) {};
  bukuOverlay = self: super: {
    buku = builtins.trace "INFO: using pinned buku version" pkgsRelease2003.buku;
  };
  nivOverlay = self: super: {
    nivSources = import ../niv/nix/sources.nix;
  };
in

[
  bukuOverlay
  nivOverlay
]
