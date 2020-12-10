{ config, pkgs, ... }:

let
  secrets = import ../nixos-shared/secrets.nix;
  mergeAttrList = pkgs.lib.foldl' pkgs.lib.mergeAttrs { };
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

      "mrconfig" = {
        target = ".mrconfig";
        text = ''
          [repos/clones/nixpkgs]
          checkout = git clone 'https://github.com/NixOS/nixpkgs' 'nixpkgs'

          [repos/nixos-config]
          checkout = git clone 'git@github.com:markus1189/nixos-config.git' 'nixos-config'
        '';
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

    git =
      (pkgs.callPackage ../nixos-shared/home-manager/git/default.nix { }).value;

    vim =
      (pkgs.callPackage ../nixos-shared/home-manager/vim/default.nix { }).value;
  };

  services = {
    keynav.enable = true;
    flameshot.enable = true;

    dunst = (pkgs.callPackage ../nixos-shared/home-manager/dunst/default.nix
      { }).value;

  };

  fonts = { fontconfig = { enable = true; }; };
}
