{ config, pkgs, ...}:

{
  environment = {
    systemPackages = with pkgs; [
      aws-vault
      binutils
      cachix
      gnupg
      gnupg1
      git
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
      nix-prefetch-docker
      nix-prefetch-git
      nix-prefetch-github
      nix-index
      nixfmt
      niv
      notifySendPb
      notifySendTelegram
      telegramSendPhoto
      telegramPhotosLastYear
      pwgen
    ];
  };
}
