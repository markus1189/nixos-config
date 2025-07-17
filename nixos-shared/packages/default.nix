{ config, pkgs, ... }:
let
  secrets = import ../secrets.nix;
  ndtSources = import ../../ndt/sources.nix { };
in {
  nixpkgs = {
    config = rec {
      packageOverrides = pkgs:
        let
          allPkgs = nixpkgs:
            nixpkgs // myScripts // pkgs.xorg // {
              xmobarLower = xmobars.lower;
              xmobarUpper = xmobars.upper;
              xmobar = pkgs.xmobar;
              xkill = pkgs.xorg.xkill;
            };
          callPackageWith = nixpkgs:
            nixpkgs.lib.callPackageWith (allPkgs nixpkgs);
          callPackage = callPackageWith pkgs;

          myScripts = pkgs.callPackage ./scripts { };
          xmobars = callPackage ./xmobarrc {
            inherit mutate;
            togglTimer = myScripts.togglTimer secrets.toggl;
            wirelessInterface = config.lib._custom_.wirelessInterface;
          };
          mutate = callPackage ./mutate { };
        in rec {
          inherit myScripts mutate;
          notifySendPb = myScripts.notifySendPb secrets.pushBulletToken;
          notifySendTelegram =
            myScripts.notifySendTelegram secrets.telegramBotToken;
          sendTelegramPoll =
            myScripts.sendTelegramPoll secrets.telegramBotToken;
          telegramSendPhoto =
            myScripts.telegramSendPhoto secrets.telegramBotToken;
          telegramPhotosLastYear =
            myScripts.telegramPhotosLastYear secrets.telegramBotToken;
          mkRsstailToRaindropUnitWithSecrets =
            myScripts.mkRsstailToRaindropUnit { access_token = secrets.raindrop.test_token; };
          viessmannOutsideTemperature =
            myScripts.viessmannOutsideTemperature {
              botToken = secrets.telegramBotToken;
              viessmannRefreshToken = secrets.viessmannRefreshToken;
            };
          myConfigFiles = {
            xmonad = callPackage ./xmonad {
              inherit mutate;
              inherit (myScripts) bukuRun;
              autoMonitorConfig = myScripts.autoMonitorConfig config.lib._custom_.wirelessInterface;
              chooseNetwork =
                myScripts.chooseNetwork config.lib._custom_.wirelessInterface;
            };
            xmobarLower = xmobars.lower;
            xmobarUpper = xmobars.upper;
            offlineimap = callPackage ./offlineimap {
              inherit mutate;
              googlepw = secrets.googlepw;
            };
          };
          emacs = callPackageWith pkgs ./emacs { inherit mutate ndtSources; };
        };
    };
  };
}
