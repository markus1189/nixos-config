{ config, pkgs, ... }:

let
  mergeAttrList = pkgs.lib.foldl' pkgs.lib.mergeAttrs { };
  garmin = (
    pkgs.callPackage (import ../nixos-shared/home-manager/garmin-connect/default.nix {
      targetDir = "${config.home.homeDirectory}/Syncthing/activities";
      tokenStore = "${config.home.homeDirectory}/.garminconnect";
      environmentFile = "/run/agenix/garminConnect";
    }) { }
  );
  zwiftWeightSync = pkgs.callPackage (import
    ../nixos-shared/home-manager/zwift-weight-sync/default.nix
    {
      environmentFile = "/run/agenix/zwiftWeightSync";
    }
  ) { };
in
{
  imports = [
    ../nixos-shared/home-manager/easyeffects-autogain.nix
    ../nixos-shared/home-manager/starship/default.nix
  ];

  home = {
    stateVersion = "18.09";

    file =
      let
        claudeConfig = pkgs.callPackage ../nixos-shared/home-manager/claude-code {
          # Headless: no chromium here, and the two marginal skills launch a
          # TUI into a borrowed tty that does not exist on this box. All three
          # would only drag their packages into the nightly autoUpgrade
          # closure — marginal newly so, since the skills now come out of the
          # package's $out rather than its source tree. removeAttrs is lazy,
          # so none of them is ever realised on nuc.
          agentSkills = builtins.removeAttrs pkgs.agentSkills [
            "agent-browser"
            "marginal-last"
            "marginal-diff"
          ];
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
      // claudeConfig.agentFiles;
  };

  manual = {
    html.enable = true;
    json.enable = true;
    manpages.enable = true;
  };

  fonts = {
    fontconfig = {
      enable = true;
    };
  };

  systemd.user = {
    startServices = true;
    services =
      let
        rsstail = pkgs.mkRsstailToRaindropUnit;
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
            Unit = {
              Description = "Stop Kodi Mediacenter";
            };

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
      in
      mergeAttrList (rssTailServices ++ [ otherServices ]);
  };

  systemd.user.timers.garminConnectSync = garmin.timer;
  systemd.user.timers.syncWeightToZwift = zwiftWeightSync.timer;

  systemd.user.timers.kodi = {
    Unit = {
      Description = "Start Kodi in the evening";
    };
    Install = {
      WantedBy = [ "timers.target" ];
    };
    # No Persistent: a machine booting at 23:00 should not launch Kodi for the
    # 20:00 it missed.
    Timer = {
      OnCalendar = "*-*-* 20:00:00";
    };
  };

  systemd.user.timers.kodi-stop = {
    Unit = {
      Description = "Nightly Kodi shutdown";
    };
    Install = {
      WantedBy = [ "timers.target" ];
    };
    Timer = {
      OnCalendar = "*-*-* 03:00:00";
    };
  };
}
