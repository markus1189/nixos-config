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

    # Non-flake source trees, formerly pinned in ndt/sources.json.
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
    ndt = {
      url = "github:markus1189/ndt";
      flake = false;
    };
  };

  outputs = inputs@{ self, nixpkgs, ... }:
    let
      # ndtSources-compatible view: consumers keep using .outPath, .rev and
      # .date (ISO-8601, synthesized from lastModifiedDate, always UTC).
      mkSource = input: {
        inherit (input) outPath;
        rev = input.rev or null;
        date =
          let
            d = input.lastModifiedDate;
            s = start: len: builtins.substring start len d;
          in "${s 0 4}-${s 4 2}-${s 6 2}T${s 8 2}:${s 10 2}:${s 12 2}+00:00";
      };

      sources = {
        darktable = mkSource inputs.darktable;
        visidata = mkSource inputs.visidata;
        xclip = mkSource inputs.xclip;
        gptel = mkSource inputs.gptel;
        hosts = mkSource inputs.stevenblack-hosts;
        zsh-histdb = mkSource inputs.zsh-histdb;
      };

      mkHost = modules: nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = { inherit inputs sources; };
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
    };
}
