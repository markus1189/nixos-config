{ config, pkgs, ...}:

{
  environment.interactiveShellInit = ''
      source ${pkgs.nix-index}/etc/profile.d/command-not-found.sh
  '';

  programs = {
    command-not-found.enable = false;

    firejail.enable = true;

    less = {
      envVariables = {
        LESS = "-XI";
      };
    };

    bash = {
      enableCompletion = true;
      enableLsColors = true;

    };

    java = {
      enable = true;
      package = pkgs.adoptopenjdk-bin;
    };

    gnupg = {
      agent = {
        enable = true;
      };
    };
  };
}
