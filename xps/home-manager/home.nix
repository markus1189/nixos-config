{ config, pkgs, ... }:

let secrets = import ../../nixos-shared/secrets.nix;
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
        source = pkgs.callPackage ./keynav { };
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

    git = (pkgs.callPackage ./git/default.nix { }).value;

    newsboat = (pkgs.callPackage ./newsboat/default.nix { inherit secrets; }).value;

    vim = (pkgs.callPackage ./vim/default.nix { }).value;
  };

  services = {
    keynav.enable = true;
    flameshot.enable = true;

    dunst = (pkgs.callPackage ./dunst/default.nix { }).value;

    # remind = {
    #   enable = false;
    #   remindCommand = "${pkgs.notifySendTelegram}/bin/notifySendTelegram %s";
    #   remindFile = "${config.home.homeDirectory}/.reminders";
    # };
  };

  fonts = { fontconfig = { enable = true; }; };
}
