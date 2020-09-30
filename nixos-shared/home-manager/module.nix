{ userName, homeNixFile }:
{ config, pkgs, homeNix, ... }:

{
  home-manager = {
    useUserPackages = true;
    useGlobalPkgs = true;

    users = { ${userName} = import homeNixFile; };
  };
}
