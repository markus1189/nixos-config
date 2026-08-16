{
  description = "Basic example of Nix-on-Droid system config.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-on-droid = {
      url = "github:nix-community/nix-on-droid/release-24.05";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.home-manager.follows = "home-manager";
    };

    # Maintained fork of clvv/fasd; nixpkgs removed the package after the
    # original repo was archived. Same source as the parent flake's input.
    fasd = {
      url = "github:whjvenyl/fasd";
      flake = false;
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      nix-on-droid,
      home-manager,
      fasd,
    }:
    {

      nixOnDroidConfigurations.default = nix-on-droid.lib.nixOnDroidConfiguration {
        pkgs = import nixpkgs {
          system = "aarch64-linux";
          overlays = [
            (final: prev: {
              fasd = final.callPackage ../nixos-shared/packages/fasd { src = fasd; };
            })
          ];
          config = {
            allowUnfreePredicate =
              pkg:
              builtins.elem (nixpkgs.lib.getName pkg) [
                "claude-code"
                "unrar"
              ];
          };
        };
        modules = [ ./nix-on-droid.nix ];
      };

    };
}
