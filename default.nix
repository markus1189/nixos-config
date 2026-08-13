let
  builder = configuration:
    (import <nixpkgs/nixos> { inherit configuration; }).system;

in map builder [ ./nuc/configuration.nix ./p1/configuration.nix ]
