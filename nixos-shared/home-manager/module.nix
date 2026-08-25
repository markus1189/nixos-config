# Shared home-manager invariants; imported once via flake-base. Hosts wire
# their user config with the standard idiom:
#   home-manager.users.${config.my.userName}.imports = [ ./home.nix ];
# (imports-list form so several modules can contribute to the same user,
# e.g. laptop/home.nix + a per-host home.nix).
{
  home-manager = {
    useUserPackages = true;
    useGlobalPkgs = true;

    # Settings every user config had declared for itself. sharedModules
    # applies them to every home-manager user on every host, so a new host's
    # home.nix starts with them rather than re-stating them.
    sharedModules = [
      {
        manual = {
          html.enable = true;
          json.enable = true;
          manpages.enable = true;
        };

        fonts.fontconfig.enable = true;
      }
    ];
  };
}
