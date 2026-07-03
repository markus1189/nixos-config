{ homeNixFile }:
{ config, pkgs, inputs, ... }:

{
  home-manager = {
    useUserPackages = true;
    useGlobalPkgs = true;

    # Make the flake inputs available to home-manager modules
    # (laptop/home.nix uses inputs.nixpkgs-master).
    extraSpecialArgs = { inherit inputs; };

    users = { ${config.lib._custom_.userName} = import homeNixFile; };
  };
}
