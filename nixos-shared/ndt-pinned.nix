# Single source of truth for the pinned `ndt` build (revision + ghc).
#
# Imported by both nixos-shared/shared-overlays.nix (the system-wide `pkgs.ndt`
# CLI) and ndt/sources.nix (the eval-time helper that emits the fetch
# expressions), so the tracked revision lives in exactly one place. Bump the
# `rev` below to update ndt everywhere.
{ nixpkgs
, ghc ? "ghc912"
}:

import (builtins.fetchGit {
  name = "ndt-fetchgit";
  url = "https://github.com/markus1189/ndt/";
  rev = "76ec23dc023c3ba3b88d183e2fd91b6254dc1d2b";
}) {
  inherit nixpkgs ghc;
}
