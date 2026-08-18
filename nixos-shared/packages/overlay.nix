# The pure half of the custom package set — no NixOS `config` involved.
# Applied as a plain nixpkgs overlay by ./default.nix on every host AND by
# flake.nix for the `.#myScripts` output, so both resolve to identical
# derivations. The config-dependent half (myConfigFiles) lives in
# ./default.nix, which closes over config.my.wirelessInterface.
#
# Resolution notes:
# - myScripts resolves its args from `final`, so scripts see the overlaid
#   xclip and the emacs bundle defined right here (same as the old
#   packageOverrides behavior, where pkgs.callPackage resolved from the
#   final fixpoint).
# - `emacs` is REPLACED by the withPackages bundle; the `emacs = prev.emacs`
#   arg is the plain emacs the bundle wraps (final.emacs would recurse).
inputs: final: prev:
let
  myScripts = final.callPackage ./scripts { };
in
{
  inherit myScripts;

  fasd = final.callPackage ./fasd { src = inputs.fasd; };

  mutate = final.callPackage ./mutate { };

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

  emacs = final.callPackage ./emacs {
    emacs = prev.emacs;
    elispSrcs = {
      inherit (inputs)
        gptel
        dired-plus
        iy-go-to-char
        ;
    };
  };
}
