{
  config,
  pkgs,
  callPackage,
  ...
}:

{
  services.emacs = {
    enable = false;
    package = pkgs.emacs-unstable;
  };

  # The emacs overlay is applied in nixos-shared/flake-base.nix (pinned via
  # flake.lock; update with `nix flake update emacs-overlay`).
}
