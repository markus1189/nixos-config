{ config, pkgs, ...}:

{
  programs = {
    command-not-found.enable = true;

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
