{
  outputs = { self, nixpkgs }: {
     nixosConfigurations.nixos-p1 = nixpkgs.lib.nixosSystem {
       system = "x86_64-linux";
       modules = [ ./p1/configuration.nix ];
     };
  };
}
