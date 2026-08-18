{
  config,
  inputs,
  pkgs,
  ...
}:

{
  # Installs nix-index and comma wrapped around the prebuilt database from
  # flake.lock, wires the command-not-found handler into bash/zsh/fish, and
  # defaults programs.command-not-found.enable to false.
  imports = [ inputs.nix-index-database.nixosModules.nix-index ];

  programs = {
    bcc.enable = true; # shellsnoop, opensnoop, exitsnoop etc

    nix-index-database.comma.enable = true;

    firejail.enable = true;

    less = {
      envVariables = {
        LESS = "-RXi";
      };
    };

    bash = {
      completion.enable = true;
      enableLsColors = true;

    };

    java = {
      enable = true;
      package = pkgs.temurin-bin;
    };

    gnupg = {
      agent = {
        enable = true;
      };
    };

    npm = {
      npmrc = ''
        ignore-scripts=true
      '';
    };
  };
}
