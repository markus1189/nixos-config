{ config, pkgs, ...}:

{
  environment = {
    systemPackages = with pkgs; [
      aws-vault
      binutils
      cachix
      gnupg
      gnupg1
      gitFull
      (gitAndTools.gh)
      (gitAndTools.git-extras)
      (gitAndTools.git-open)
      (gitAndTools.hub)
      git-secret
      gron
      htop
      jo
      jq
      logArgs
      mpv
      nix-prefetch-docker
      nix-prefetch-git
      nix-prefetch-github
      nix-index
      nixfmt
      niv
      notifySendPb
      notifySendTelegram
      sendTelegramPoll
      sendCurrentTemperature
      telegramPhotosLastYear
      pwgen
      yq
    ];
  };
}
