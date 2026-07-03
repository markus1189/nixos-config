{ config, pkgs, inputs, ... }:

{
  services.emacs = {
    enable = false;
    package = pkgs.emacs-unstable;
  };

  # nix-community emacs-overlay as a flake input, pinned via flake.lock
  # (previously fetched unpinned from master on every evaluation).
  # Bump with `nix flake update emacs-overlay`.
  nixpkgs.overlays = [
    inputs.emacs-overlay.overlays.default
  ];
}
