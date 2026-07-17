{ config, pkgs, ... }:

let
  mergeAttrList = pkgs.lib.foldl' pkgs.lib.mergeAttrs { };
  garmin = (pkgs.callPackage
    (import ../nixos-shared/home-manager/garmin-connect/default.nix {
      targetDir = "/home/mediacenter/Syncthing/activities";
      tokenStore = "/home/mediacenter/.garminconnect";
      environmentFile = "/run/agenix/garminConnect";
    }) { });
  zwiftWeightSync = pkgs.callPackage
    (import ../nixos-shared/home-manager/zwift-weight-sync/default.nix {
      environmentFile = "/run/agenix/zwiftWeightSync";
    }) { };
in {
  imports = [ ../nixos-shared/home-manager/easyeffects-autogain.nix ];

  home = {
    stateVersion = "18.09";

    file =
      let
        claudeConfig = pkgs.callPackage ../nixos-shared/home-manager/claude-code {
          enableSoundHooks = false;
          enableDenyRules = true;
          additionalAllowedCommands = [
            "Bash(git commit:*)"
            "Bash(git show:*)"
          ];
        };
      in
      {
        "claude-code" = claudeConfig.settings;
        "claude-md" = claudeConfig.globalClaudeMd;
      }
      // claudeConfig.markdownFiles;
  };

  manual = {
    html.enable = true;
    json.enable = true;
    manpages.enable = true;
  };

  programs = {
    starship = (pkgs.callPackage ../nixos-shared/home-manager/starship/default.nix { }).value;
  };

  fonts = { fontconfig = { enable = true; }; };

  systemd.user = {
    startServices = true;
    services = let
      rsstail = pkgs.mkRsstailToRaindropUnitWithSecrets;
      otherServices = {
        garminConnectSync = garmin.service;
        syncWeightToZwift = zwiftWeightSync.service;

        kodi = {
          Unit = {
            Description = "Kodi Mediacenter";
            # Requisite (not Wants): if the Plasma session is down there is no
            # XAUTHORITY in the user manager's environment, and Kodi would fail
            # the X auth handshake anyway. Fail fast instead.
            After = [ "graphical-session.target" ];
            Requisite = [ "graphical-session.target" ];
            PartOf = [ "graphical-session.target" ];
          };

          Service = {
            Type = "simple";
            Environment = [ "KODI_AE_SINK=ALSA" ];
            ExecStart = "${pkgs.kodi}/bin/kodi";
            # SIGTERM on stop, SIGKILL five minutes later - reproduces the old
            # killall / killall -9 pair that ran at 03:00 and 03:05.
            TimeoutStopSec = 300;
          };
        };

        kodi-stop = {
          Unit = { Description = "Stop Kodi Mediacenter"; };

          Service = {
            Type = "oneshot";
            ExecStart = "${pkgs.systemd}/bin/systemctl --user stop kodi.service";
          };
        };
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
          url = "https://www.monkeyuser.com/index.xml";
        }
        {
          key = "inCode";
          url = "https://feeds.feedburner.com/incodeblog";
        }
      ];
    in mergeAttrList (rssTailServices ++ [ otherServices ]);
  };

  systemd.user.timers.garminConnectSync = garmin.timer;
  systemd.user.timers.syncWeightToZwift = zwiftWeightSync.timer;

  systemd.user.timers.kodi = {
    Unit = { Description = "Start Kodi in the evening"; };
    Install = { WantedBy = [ "timers.target" ]; };
    # No Persistent: a machine booting at 23:00 should not launch Kodi for the
    # 20:00 it missed.
    Timer = { OnCalendar = "*-*-* 20:00:00"; };
  };

  systemd.user.timers.kodi-stop = {
    Unit = { Description = "Nightly Kodi shutdown"; };
    Install = { WantedBy = [ "timers.target" ]; };
    Timer = { OnCalendar = "*-*-* 03:00:00"; };
  };
}
