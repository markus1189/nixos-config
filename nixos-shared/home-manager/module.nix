{ homeNixFile }:
{ config, pkgs, ... }:

{
  home-manager = {
    useUserPackages = true;
    useGlobalPkgs = true;

    users = {
      ${config.my.userName} = import homeNixFile;
    };
  };
}
