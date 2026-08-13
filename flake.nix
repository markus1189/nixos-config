{
  description = "NixOS configurations (p1, p1g8, nuc)";

  inputs = {
    # Pinned to the exact revision of the previously-used nixos-unstable
    # channel; bump with `nix flake update nixpkgs`.
    nixpkgs.url = "github:NixOS/nixpkgs/567a49d1913ce81ac6e9582e3553dd90a955875f";
  };

  outputs = { self, nixpkgs }:
    let
      mkHost = modules: nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        inherit modules;
      };
    in {
      nixosConfigurations = {
        p1 = mkHost [ ./p1/configuration.nix ];
        p1g8 = mkHost [ ./p1g8/configuration.nix ];
        nuc = mkHost [ ./nuc/configuration.nix ];
        # p1's hostname is nixos-p1; alias so `nixos-rebuild --flake .`
        # resolves without an explicit attr name.
        nixos-p1 = self.nixosConfigurations.p1;
      };
    };
}
