{ homeNixFile }:
{ config, pkgs, ... }:

{
  home-manager = {
    useUserPackages = true;
    useGlobalPkgs = true;

    users = {
      ${config.lib._custom_.userName} = import homeNixFile;
    };
  };
}
