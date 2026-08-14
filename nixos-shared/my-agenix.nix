{ inputs, pkgs, ... }:

# The agenix NixOS module itself is imported in flake.nix; this only adds
# the CLI to the system.
{
  environment = {
    systemPackages = [ inputs.agenix.packages.${pkgs.stdenv.hostPlatform.system}.default ];
  };
}
