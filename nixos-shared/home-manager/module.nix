{ homeNixFile }:
{ config, pkgs, homeNix, ... }:

{
  home-manager = {
    useUserPackages = true;
    useGlobalPkgs = true;

    users = { ${config.lib._custom_.userName} = import homeNixFile; };
  };
}
