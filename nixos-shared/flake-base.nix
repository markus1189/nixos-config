{ inputs, config, lib, pkgs, ... }:

# Flake plumbing shared by all hosts: provides pkgs.masterPkgs, applies the
# emacs overlay, passes flake inputs to home-manager modules, and pins
# nixpkgs for legacy tooling.
{
  home-manager.extraSpecialArgs = { inherit inputs; };

  nixpkgs.overlays = [
    inputs.emacs-overlay.overlays.default
    (final: prev: {
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
