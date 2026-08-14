{
  inputs,
  config,
  lib,
  pkgs,
  ...
}:

# Flake plumbing shared by all hosts: provides pkgs.masterPkgs, applies the
# emacs overlay, passes flake inputs to home-manager modules, and pins
# nixpkgs for legacy tooling.
{
  imports = [ ./my-options.nix ];

  home-manager.extraSpecialArgs = { inherit inputs; };

  nixpkgs.config.allowUnfree = true;

  # `nixos-version --configuration-revision` names the commit a generation
  # was built from — invaluable on nuc, where autoUpgrade builds unattended.
  system.configurationRevision = inputs.self.rev or inputs.self.dirtyRev or "dirty";

  nixpkgs.overlays = [
    inputs.emacs-overlay.overlays.default
    (final: prev: {
      # Replaces the unpinned `fetchTarball nixpkgs/master` imports.
      masterPkgs = import inputs.nixpkgs-master {
        inherit (final.stdenv.hostPlatform) system;
        config = {
          allowUnfreePredicate = pkg: builtins.elem (final.lib.getName pkg) [ "claude-code" ];
          firefox = {
            enableOfficialBranding = true;
          };
        };
      };

      # marginal (rust CLI) as a normal pkgs attr, built from its own flake
      # but with our toolchain (see inputs.marginal in flake.nix).
      marginal = inputs.marginal.packages.${final.stdenv.hostPlatform.system}.marginal;
    })
  ];

  nix = {
    settings.experimental-features = [
      "nix-command"
      "flakes"
    ];

    # `nix run nixpkgs#...`, `nix-shell -p ...` and stray <nixpkgs> resolve
    # to the locked flake input instead of a mutable channel.
    registry.nixpkgs.flake = inputs.nixpkgs;
    nixPath = [ "nixpkgs=flake:nixpkgs" ];
  };
}
