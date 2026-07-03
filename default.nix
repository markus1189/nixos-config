# Legacy convenience entry point: `nix-build` builds every host's system
# closure.  The real interface is flake.nix, e.g.
#   nix build .#nixosConfigurations.p1.config.system.build.toplevel
let
  flake = builtins.getFlake (toString ./.);
in
builtins.mapAttrs (_: host: host.config.system.build.toplevel)
  flake.nixosConfigurations
