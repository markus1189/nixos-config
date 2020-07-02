let builder = configuration: import <nixpkgs/nixos> { inherit configuration; }; in

map builder [
  ./xps/configuration.nix
  ./nuc/configuration.nix
]
