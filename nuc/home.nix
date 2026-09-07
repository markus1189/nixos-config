{ config, pkgs, ... }:

let
  mergeAttrList = pkgs.lib.foldl' pkgs.lib.mergeAttrs { };
  garmin = pkgs.callPackage (import ../nixos-shared/home-manager/garmin-connect/default.nix {
    targetDir = "${config.home.homeDirectory}/Syncthing/activities";
    tokenStore = "${config.home.homeDirectory}/.garminconnect";
    environmentFile = "/run/agenix/garminConnect";
  }) { };
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
    # Without this nothing sets programs.zsh.enable on the home-manager side,
    # so HM writes no ~/.zshrc at all and every module contributing to
    # programs.zsh.initContent is silently dropped -- starship above included
    # (it wrote starship.toml and never got a hook). Also brings the shared
    # history/histdb settings and the aliases.
    ../nixos-shared/home-manager/zsh/default.nix
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
        "claude-md" = claudeConfig.globalAgentMd;

        # The Plasma desktop shortcut. It used to be a hand-made symlink into
        # /nix/store/...-konsole-20.08.3 from 2021, which worked only because
        # nix-gc had been failing on the old HDD and never collected the path;
        # the 2026-09-06 reinstall brought an empty store and the icon died.
        # Declaring it here means home-manager re-points it on every switch,
        # so it cannot go stale again.
        "konsole-desktop-entry" = {
          target = "Desktop/org.kde.konsole.desktop";
          source = "${pkgs.kdePackages.konsole}/share/applications/org.kde.konsole.desktop";
        };
      }
      // claudeConfig.agentFiles;
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
