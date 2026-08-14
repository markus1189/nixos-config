{ config, pkgs, ... }:

{
  environment = {
    systemPackages =
      (with pkgs.masterPkgs; [ claude-code ])
      ++ (with pkgs; [
        aws-vault
        bat
        binutils
        cachix
        comma
        dnsutils
        ed
        ffmpeg-full
        # edbrowse
        gnupg
        gnupg1
        gitFull
        gh
        git-extras
        git-open
        hub
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
        nixfmt
        nixpkgs-lint
        nixpkgs-fmt
        notifySendPb
        notifySendTelegram
        notifySendTelegramHtml
        notifySendTelegramMd
        poppler-utils
        sendTelegramPoll
        viessmannOutsideTemperature
        telegramPhotosLastYear
        pwgen
        yq
        viddy
        visidata
        watchexec
        wormhole-william
      ])
      ++ (with pkgs; [
        (myScripts.logArgs)
        (myScripts.recordMeeting)
        (myScripts.tmuxPollPane)
        (myScripts.chronic-file)
        (myScripts.gemini-vision)
      ]);
  };
}
