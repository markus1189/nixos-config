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
    ];
  };
}
