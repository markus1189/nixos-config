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
