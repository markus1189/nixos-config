{ config, pkgs, ... }:

let
  secrets = import ../nixos-shared/secrets.nix;
  mergeAttrList = pkgs.lib.foldl' pkgs.lib.mergeAttrs { };
in {
  home = {
  };

  manual = {
    html.enable = true;
    json.enable = true;
    manpages.enable = true;
  };

  programs = {
    newsboat =
      (pkgs.callPackage ../nixos-shared/home-manager/newsboat/default.nix {
        inherit secrets;
      }).value;
  };

  services = {
  };

  fonts = { fontconfig = { enable = true; }; };

  systemd.user = {
    startServices = true;
    services = let rsstail = pkgs.mkRsstailToPocketUnitWithSecrets;
    in mergeAttrList (map rsstail [
      {
        key = "xkcd";
        url = "http://www.xkcd.com/rss.xml";
      }
      {
        key = "commitstrip";
        url = "http://www.commitstrip.com/en/feed/";
      }
      {
        key = "raptitude";
        url = "https://www.raptitude.com/feed/";
      }
      {
        key = "farnamstreet";
        url = "fs.blog/feed";
      }
    ]);
  };
}
