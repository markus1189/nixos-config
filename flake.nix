{
  description = "NixOS configurations for markus1189's machines (p1, p1g8, nuc, xps)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    # Bleeding-edge packages (claude-code, nix-direnv) — see
    # nixos-shared/common-packages.nix and laptop/home.nix.
    nixpkgs-master.url = "github:NixOS/nixpkgs/master";

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Haskell CLI that manages ndt/sources.json; also exposed as pkgs.ndt
    # via nixos-shared/shared-overlays.nix.
    ndt = {
      url = "github:markus1189/ndt";
      flake = false;
    };
  };

  # Note on secrets: the git-secret revealed nixos-shared/secrets.nix is
  # gitignored, and a git flake only sees tracked files, so pure evaluation
  # falls back to nixos-shared/secrets.dummy.nix (see load-secrets.nix).
  # Real machine builds must use a path flake to include the revealed file:
  #   sudo nixos-rebuild switch --flake "path:$PWD#<host>"
  outputs = { self, nixpkgs, ... }@inputs:
    let
      mkHost = configuration:
        nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          specialArgs = { inherit inputs; };
          modules = [ configuration ];
        };
    in
    {
      nixosConfigurations = {
        p1 = mkHost ./p1/configuration.nix;    # ThinkPad P1 (primary laptop)
        p1g8 = mkHost ./p1g8/configuration.nix; # ThinkPad P1 Gen 8
        nuc = mkHost ./nuc/configuration.nix;  # home server
        xps = mkHost ./xps/configuration.nix;  # legacy XPS laptop
      };
    };
}
