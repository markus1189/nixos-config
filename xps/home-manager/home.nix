{ pkgs, ...}:

{
  home = {
    packages = [
    ];
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
}
