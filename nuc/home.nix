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
  home = { };

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
        patchbayStravaResponder = (pkgs.callPackage
          (import ../nixos-shared/home-manager/patchbay/responder.nix) {
            patchbayUrl =
              "https://patchbay.pub/res/d5b1851f-448b-4c74-a2c0-1f9f62d90879-67757c09-079b-4df5-9f94-6c33b8060f10";
          }).value;
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
