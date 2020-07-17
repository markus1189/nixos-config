{ config, pkgs, ...}:

{
  programs = {
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
