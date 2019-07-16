{ config, pkgs, ...}:

{
  environment = {
    systemPackages = with pkgs; [
      binutils
      gnupg
      gnupg1
      git
      gitFull
      (gitAndTools.git-extras)
      git-secret
      htop
      nix-prefetch-docker
      nix-prefetch-git
      nix-prefetch-github
    ];
  };
}
