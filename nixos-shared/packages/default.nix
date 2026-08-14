{
  config,
  pkgs,
  inputs,
  ...
}:
{
  nixpkgs = {
    config = rec {
      packageOverrides =
        pkgs:
        let
          allPkgs =
            nixpkgs:
            nixpkgs
            // myScripts
            // pkgs.xorg
            // {
              xmobarLower = xmobars.lower;
              xmobarUpper = xmobars.upper;
              xmobar = pkgs.xmobar;
              xkill = pkgs.xkill;
            };
          callPackageWith = nixpkgs: nixpkgs.lib.callPackageWith (allPkgs nixpkgs);
          callPackage = callPackageWith pkgs;

          myScripts = pkgs.callPackage ./scripts { };
          xmobars = callPackage ./xmobarrc {
            inherit mutate;
            togglTimer = myScripts.togglTimer;
            wirelessInterface = config.my.wirelessInterface;
          };
          mutate = callPackage ./mutate { };
        in
        rec {
          inherit myScripts mutate;
          inherit (myScripts)
            notifySendPb
            notifySendTelegram
            notifySendTelegramHtml
            notifySendTelegramMd
            sendTelegramPoll
            telegramSendPhoto
            telegramPhotosLastYear
            mkRsstailToRaindropUnit
            ;
          # Viessmann refresh token expires every 180 days. To renew, run:
          # , oauth2c https://iam.viessmann-climatesolutions.com/idp/v3 \
          #   --client-id=45e59eb93fb498140de733c44637d8df \
          #   --redirect-url=http://localhost:4244/ \
          #   --scopes=IoT --scopes=User --scopes=offline_access \
          #   --response-types=code \
          #   --grant-type=authorization_code \
          #   --auth-method=none \
          #   --response-mode=query \
          #   --pkce
          # then store it: agenix -e secrets/viessmann-refresh-token.age
          inherit (myScripts) viessmannOutsideTemperature;
          myConfigFiles = {
            xmonad =
              let
                audioRecordScript = pkgs.writeShellApplication {
                  name = "recordScript";
                  runtimeInputs = with pkgs; [
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
              callPackage ./xmonad {
                inherit mutate;
                inherit (myScripts) bukuRun;
                recordScript = audioRecordScript;
                autoMonitorConfig = myScripts.autoMonitorConfig config.my.wirelessInterface;
                chooseNetwork = myScripts.chooseNetwork config.my.wirelessInterface;
              };
            xmobarLower = xmobars.lower;
            xmobarUpper = xmobars.upper;
          };
          emacs = callPackageWith pkgs ./emacs {
            inherit mutate;
            elispSrcs = {
              inherit (inputs)
                gptel
                dired-plus
                iy-go-to-char
                hurl
                ;
            };
          };
        };
    };
  };
}
