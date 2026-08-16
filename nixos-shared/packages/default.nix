# Custom packages as nixpkgs overlays. The pure set (myScripts, mutate, the
# emacs bundle, telegram helpers) lives in ./overlay.nix — shared with the
# flake's `.#myScripts` output. Only myConfigFiles stays here, because the
# xmonad/xmobar configs close over config.my.wirelessInterface and therefore
# need to be an overlay defined inside a NixOS module.
{ config, inputs, ... }:
{
  nixpkgs.overlays = [
    (import ./overlay.nix inputs)

    (final: prev: {
      myConfigFiles =
        let
          inherit (final) mutate myScripts;

          xmobars = final.callPackage ./xmobarrc {
            inherit mutate;
            inherit (myScripts)
              btHeadphoneBattery
              chargeRate
              currentSpotifySong
              dunstStatus
              isVpnActive
              togglTimer
              xmobarSharingIndicator
              ;
            wirelessInterface = config.my.wirelessInterface;
          };

          audioRecordScript = final.writeShellApplication {
            name = "recordScript";
            runtimeInputs = with final; [
              pulseaudio
              ffmpeg
              curl
              jq
              libnotify
              coreutils
              xdotool
              xclip
              gawk
            ];
            text = ''
              OPENAI_API_KEY="$(awk '$2 == "api.openai.com" { print $NF }' /run/agenix/authinfo)"
              export OPENAI_API_KEY
            ''
            + builtins.readFile ./xmonad/recordScript.sh;
          };
        in
        {
          xmonad = final.callPackage ./xmonad {
            inherit mutate;
            inherit (myScripts)
              bukuRun
              centerMouse
              emacsAnywhere
              flameshotOcr
              lockScreen
              multihead4k
              rofiDefaults
              rofiDownloadsPicker
              rofiStuffTodayPicker
              singlehead
              takeScreenshot
              tmx
              xmonadReset
              ;
            autoMonitorConfig = myScripts.autoMonitorConfig config.my.wirelessInterface;
            chooseNetwork = myScripts.chooseNetwork config.my.wirelessInterface;
            recordScript = audioRecordScript;
            xmobarLower = xmobars.lower;
            xmobarUpper = xmobars.upper;
          };
          xmobarLower = xmobars.lower;
          xmobarUpper = xmobars.upper;
        };
    })
  ];
}
