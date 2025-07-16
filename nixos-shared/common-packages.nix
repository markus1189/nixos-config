{ config, pkgs, ... }:

{
  environment = {
    systemPackages = with pkgs;
      [
        aws-vault
        bat
        binutils
        cachix
        comma
        dnsutils
        ed
        # edbrowse
        gnupg
        gnupg1
        gitFull
        (gitAndTools.gh)
        (gitAndTools.git-extras)
        (gitAndTools.git-open)
        (gitAndTools.hub)
        git-secret
        adwaita-icon-theme
        gron
        htop
        httpie
        iotop
        jo
        jq
        jless
        moreutils
        ncdu
        nix-prefetch-docker
        nix-prefetch-git
        nix-prefetch-github
        nix-index
        nixfmt-rfc-style
        nixpkgs-lint
        nixpkgs-fmt
        niv
        notifySendPb
        notifySendTelegram
        sendTelegramPoll
        viessmannOutsideTemperature
        telegramPhotosLastYear
        pwgen
        yq
        viddy
        visidata
        watchexec
        wormhole-william
      ] ++ (with pkgs; [ (myScripts.logArgs) ]);
  };
}
