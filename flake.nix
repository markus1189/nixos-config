{
  description = "NixOS configurations (p1, p1g8, nuc)";

  inputs = {
    # Tracks the nixos-unstable branch; the exact rev lives in flake.lock.
    # Bump with `nix flake update nixpkgs`.
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    # Bleeding-edge packages (claude-code, nix-direnv); bump with
    # `nix flake update nixpkgs-master`.
    nixpkgs-master.url = "github:NixOS/nixpkgs/master";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.home-manager.follows = "home-manager";
      inputs.darwin.follows = "";
    };

    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Non-flake source trees.
    # darktable needs its submodules (rawspeed etc.) — a github: tarball
    # input would silently drop them.
    darktable = {
      url = "git+https://github.com/darktable-org/darktable?submodules=1";
      flake = false;
    };
    visidata = {
      url = "github:saulpw/visidata/develop";
      flake = false;
    };
    xclip = {
      url = "github:astrand/xclip";
      flake = false;
    };
    gptel = {
      url = "github:karthink/gptel";
      flake = false;
    };
    stevenblack-hosts = {
      url = "github:StevenBlack/hosts";
      flake = false;
    };
    zsh-histdb = {
      url = "github:larkery/zsh-histdb";
      flake = false;
    };
    # Single-file elisp packages without a MELPA recipe (consumed in
    # nixos-shared/packages/emacs). hurl is the whole CLI repo pulled in
    # for contrib/emacs/hurl-mode.el alone.
    dired-plus = {
      url = "github:emacsmirror/dired-plus";
      flake = false;
    };
    iy-go-to-char = {
      url = "github:doitian/iy-go-to-char";
      flake = false;
    };
    hurl = {
      url = "github:Orange-OpenSource/hurl";
      flake = false;
    };

    # Rust CLI for span-level markdown annotation. Consumed via its own flake
    # output (packages.marginal) but built against our nixpkgs for one toolchain.
    marginal = {
      url = "github:markus1189/marginal";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ self, nixpkgs, ... }:
    let
      mkHost = modules: nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = { inherit inputs; };
        modules = [
          inputs.home-manager.nixosModules.home-manager
          inputs.agenix.nixosModules.default
          ./nixos-shared/flake-base.nix
        ] ++ modules;
      };
    in {
      nixosConfigurations = {
        p1 = mkHost [ ./p1/configuration.nix ];
        p1g8 = mkHost [
          ./p1g8/configuration.nix
          inputs.disko.nixosModules.disko
        ];
        nuc = mkHost [ ./nuc/configuration.nix ];
        # p1's hostname is nixos-p1; alias so `nixos-rebuild --flake .`
        # resolves without an explicit attr name.
        nixos-p1 = self.nixosConfigurations.p1;
      };

      # `nix fmt` formats the whole tree with RFC-style nixfmt via treefmt
      # (bare nixfmt reads stdin / chokes on the ./result symlink).
      formatter.x86_64-linux = nixpkgs.legacyPackages.x86_64-linux.nixfmt-tree;

      # `nix develop .#xmonad` / `use flake` in nixos-shared/packages/xmonad;
      # replaces the last channel-style shell.nix.
      devShells.x86_64-linux.xmonad =
        let pkgs = nixpkgs.legacyPackages.x86_64-linux;
        in pkgs.mkShell {
          packages = [
            (pkgs.haskellPackages.ghcWithHoogle
              (ps: with ps; [ xmonad xmonad-contrib haskell-language-server ]))
          ];
        };
    };
}
