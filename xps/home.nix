{ config, pkgs, ... }:

let secrets = import ../nixos-shared/secrets.nix;
in {
  home = {
    packages = [
      pkgs.source-code-pro
      pkgs.dunst
      pkgs.ndt
      pkgs.haskellPackages.brittany
    ];

    file = {
      "keynavrc" = {
        source = pkgs.callPackage ../nixos-shared/home-manager/keynav { };
        target = ".keynavrc";
      };
    };
  };

  manual = {
    html.enable = true;
    json.enable = true;
    manpages.enable = true;
  };

  programs = {
    bash.enable = true;
    zsh.enable = true;
    direnv = {
      enable = true;
      enableBashIntegration = true;
      enableNixDirenvIntegration = true;
      enableZshIntegration = true;
    };

    firefox.enable = true;

    git = (pkgs.callPackage ../nixos-shared/home-manager/git/default.nix { }).value;

    newsboat = (pkgs.callPackage ../nixos-shared/home-manager/newsboat/default.nix { inherit secrets; }).value;

    vim = (pkgs.callPackage ../nixos-shared/home-manager/vim/default.nix { }).value;
  };

  services = {
    keynav.enable = true;
    flameshot.enable = true;

    dunst = (pkgs.callPackage ../nixos-shared/home-manager/dunst/default.nix { }).value;

  };

  fonts = { fontconfig = { enable = true; }; };
}
