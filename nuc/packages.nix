{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    bashmount
    coreutils
    google-chrome
    feh
    jq
    lsof
    nix-index
    nixVersions.git
    parallel
    pciutils
    pmutils
    psmisc
    pv
    remind
    rlwrap
    rsync
    stack
    tigervnc
    tree
    unrar
    unzip
    vim
    wget
    which
    wyrd
    xclip
    zathura
    zip
    zsh
  ];
}
