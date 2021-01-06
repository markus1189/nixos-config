let
  builder = configuration:
    (import <nixpkgs/nixos> { inherit configuration; }).system;

in map builder [ ./xps/configuration.nix ./nuc/configuration.nix ./p1/configuration.nix ]
