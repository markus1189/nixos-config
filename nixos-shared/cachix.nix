# Binary cache config, in-tree and reproducible.
#
# Replaces the old imperative `/etc/nixos/cachix.nix` glob wrapper that
# `cachix use <name>` writes out-of-tree. We consume exactly one cache
# (nix-community, for the emacs-overlay's emacs-git build artifacts);
# its substituter URL and public key are stable public constants.
#
# `cachix use <name>` will still write /etc/nixos/cachix{,.nix} but it
# is no longer imported; add caches here declaratively instead.
{
  nix.settings = {
    # cache.nixos.org is already the NixOS default substituter; only the
    # extra cache and its key need declaring here.
    substituters = [
      "https://nix-community.cachix.org"
    ];
    trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
  };
}
