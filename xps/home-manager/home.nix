{ pkgs, ...}:

{
  home = {
    packages = [
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
    keynav = {
      enable = true;
    };
  };
}
