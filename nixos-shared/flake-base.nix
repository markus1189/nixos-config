{
  inputs,
  config,
  lib,
  pkgs,
  ...
}:

# Flake plumbing shared by all hosts: provides pkgs.masterPkgs, applies the
# emacs and shared overlays, passes flake inputs to home-manager modules, and
# pins nixpkgs for legacy tooling.
{
  imports = [
    ./my-options.nix
    ./home-manager/module.nix
  ];

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

      # hocket (Haskell Raindrop TUI) as a normal pkgs attr, from its own
      # flake. The hocket-rpc skill drives `hocket agent`, so both come out
      # of the same input (see inputs.hocket in flake.nix).
      hocket = inputs.hocket.packages.${final.stdenv.hostPlatform.system}.hocket;

      # nixpkgs' agent-browser is 0.27.0 (2026-05); upstream ships every few
      # days, so take the daily-bumped llm-agents build instead. Built against
      # our nixpkgs (see the input's `follows`), so it shares one chromium.
      agent-browser = inputs.llm-agents.packages.${final.stdenv.hostPlatform.system}.agent-browser;

      # Harness-neutral agent skills as validated per-skill derivations,
      # consumed by the claude-code and pi home-manager modules. The
      # marginal-last / marginal-diff skills come out of the marginal package
      # itself and hocket-rpc out of the hocket input, so skill text and the
      # binary it drives are version-locked together either way.
      agentSkills = import ./agent-skills {
        pkgs = final;
        marginal = final.marginal;
        hocketSrc = inputs.hocket;
        agentBrowser = final.agent-browser;
      };
    })
  ]
  ++ import ./shared-overlays.nix inputs;

  nix = {
    settings.experimental-features = [
      "nix-command"
      "flakes"
    ];

    # `nix run nixpkgs#...`, `nix-shell -p ...` and stray <nixpkgs> resolve
    # to the locked flake input instead of a mutable channel.
    registry.nixpkgs.flake = inputs.nixpkgs;
    # `nix run self#myScripts.<x>` from any directory; `master#<x>` = the
    # locked nixpkgs-master input (same tree as pkgs.masterPkgs).
    registry.self.flake = inputs.self;
    registry.master.flake = inputs.nixpkgs-master;
    nixPath = [ "nixpkgs=flake:nixpkgs" ];
  };
}
