# Shared home-manager invariants; imported once via flake-base. Hosts wire
# their user config with the standard idiom:
#   home-manager.users.${config.my.userName}.imports = [ ./home.nix ];
# (imports-list form so several modules can contribute to the same user,
# e.g. laptop/home.nix + a per-host home.nix).
{
  home-manager = {
    useUserPackages = true;
    useGlobalPkgs = true;
  };
}
