{ config, pkgs, ...}:

{
  environment = {
    systemPackages = with pkgs; [
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
      nixpkgs-lint
      nixpkgs-fmt
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
