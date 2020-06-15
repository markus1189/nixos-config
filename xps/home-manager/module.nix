userName: { config, pkgs, ...}:

{
  home-manager = {
    useUserPackages = true;
    useGlobalPkgs = true;

    users = {
      ${userName} = import ./home.nix;
    };
  };
}
