{ config, pkgs, ...}:

{
  environment = {
    systemPackages = with pkgs; [
      aws-vault
      binutils
      gnupg
      gnupg1
      git
      gitFull
      (gitAndTools.git-extras)
      (gitAndTools.git-open)
      git-secret
      htop
      nix-prefetch-docker
      nix-prefetch-git
      nix-prefetch-github
      notifySendPb
    ];
  };
}
