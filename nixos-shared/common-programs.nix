{ config, pkgs, ...}:

{
  environment.interactiveShellInit = ''
      source ${pkgs.nix-index}/etc/profile.d/command-not-found.sh
  '';

  programs = {
    bcc.enable = true;  # shellsnoop, opensnoop, exitsnoop etc

    command-not-found.enable = false;

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
