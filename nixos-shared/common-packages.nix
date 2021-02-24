{ config, pkgs, ...}:

{
  environment = {
    systemPackages = with pkgs; [
      awscli2
      aws-vault
      binutils
      cachix
      ed
      edbrowse
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
      iotop
      jo
      jq
      logArgs
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
