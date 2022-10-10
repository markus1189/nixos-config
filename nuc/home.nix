{ config, pkgs, ... }:

let
  secrets = import ../nixos-shared/secrets.nix;
  mergeAttrList = pkgs.lib.foldl' pkgs.lib.mergeAttrs { };
  garmin = (pkgs.callPackage
    (import ../nixos-shared/home-manager/garmin-connect/default.nix {
      targetDir = "/home/mediacenter/Syncthing/activities";
      password = secrets.garminConnect.password;
    }) { });
in {
  home = {
    stateVersion = "18.09";
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

  fonts = { fontconfig = { enable = true; }; };

  systemd.user = {
    startServices = true;
    services = let
      rsstail = pkgs.mkRsstailToPocketUnitWithSecrets;
      otherServices = {
        garminConnectSync = garmin.service;
      };
      rssTailServices = map rsstail [
        {
          key = "xkcd";
          url = "https://www.xkcd.com/rss.xml";
        }
        {
          key = "commitstrip";
          url = "https://www.commitstrip.com/en/feed/";
        }
        {
          key = "raptitude";
          url = "https://www.raptitude.com/feed/";
        }
        {
          key = "farnamstreet";
          url = "https://fs.blog/feed";
        }
        {
          key = "monkeyuser";
          url = "https://www.monkeyuser.com/feed";
        }
        {
          key = "inCode";
          url = "https://feeds.feedburner.com/incodeblog";
        }
      ];
    in mergeAttrList (rssTailServices ++ [ otherServices ]);
  };

  systemd.user.timers.garminConnectSync = garmin.timer;
}
