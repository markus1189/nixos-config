{ pkgs, ...}:

{
  home = {
    packages = [
      pkgs.source-code-pro
    ];

    file = {
      "keynavrc" = {
        source = pkgs.callPackage ./keynav {};
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
    firefox.enable = true;

    git = (pkgs.callPackage ./git/default.nix {}).value;
  };

  services = {
    keynav.enable = true;

    dunst = (pkgs.callPackage ./dunst/default.nix {}).value;
  };

  fonts = {
    fontconfig = {
      enable = true;
    };
  };
}
