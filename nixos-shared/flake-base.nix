{ inputs, sources, config, lib, pkgs, ... }:

# Flake plumbing shared by all hosts: exposes the former ndtSources through
# the channels consumers already use (pkgs.ndtSources and
# config.lib._custom_.ndtSources), provides pkgs.masterPkgs and pkgs.ndt,
# applies the emacs overlay, and pins nixpkgs for legacy tooling.
{
  lib._custom_ = {
    ndtSources = sources;
  };

  nixpkgs.overlays = [
    inputs.emacs-overlay.overlays.default
    (final: prev: {
      ndtSources = sources;
      ndt = import inputs.ndt {
        nixpkgs = final;
        ghc = "ghc912";
      };
      # Replaces the unpinned `fetchTarball nixpkgs/master` imports.
      masterPkgs = import inputs.nixpkgs-master {
        inherit (final.stdenv.hostPlatform) system;
        config = {
          allowUnfreePredicate = pkg:
            builtins.elem (final.lib.getName pkg) [ "claude-code" ];
          firefox = {
            enableOfficialBranding = true;
          };
        };
      };
    })
  ];

  nix = {
    # `nix run nixpkgs#...`, `nix-shell -p ...` and stray <nixpkgs> resolve
    # to the locked flake input instead of a mutable channel.
    registry.nixpkgs.flake = inputs.nixpkgs;
    nixPath = [ "nixpkgs=flake:nixpkgs" ];
  };
}
