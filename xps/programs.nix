{ pkgs, ... }:

let
  pkgsMaster = import (fetchTarball https://github.com/NixOS/nixpkgs/archive/master.tar.gz) {};
  pkgsFork = import (fetchTarball https://github.com/markus1189/nixpkgs/archive/master.tar.gz) {};
  pkgsRelease19 = import (fetchTarball https://github.com/NixOS/nixpkgs/archive/release-19.03.tar.gz) {};
  pkgsMy = import (fetchTarball https://github.com/markus1189/nixpkgs/archive/staging.tar.gz) {};
in
{
  nixpkgs = {
    config = {
      firefox = {
        enableGoogleTalkPlugin = true;
        enableOfficialBranding = true;
      };
    };
  };

  environment = {
    systemPackages = with pkgs; [
      ack
      actkbd
      alsaOss
      alsaUtils
      ammonite
      arandr
      aspell
      aspellDicts.de
      aspellDicts.en
      at
      bc
      bashmount
      cabal-install
      chromedriver
      chromium
      cloc
      coreutils
      docker_compose
      dmenu
      # dropbox-cli
      ddgr
      deluge
      pkgsRelease19.dragon-drop
      dstat
      emacs
      entr
      evince
      exa
      exfat
      exiftool
      expect
      feh
      figlet
      file
      # filezilla
      firefoxWrapper
      foo2zjs
      fortune
      gcc
      gdb
      ghc
      # ghostscript
      gimp
      glxinfo
      gnumake
      gnuplot
      go-pup
      googleearth
      gparted
      graphviz
      hicolor_icon_theme
      html2text
      httpie
      i3lock
      jetbrains.idea-community
      inetutils
      imagemagick
      imv
      inkscape
      insomnia
      jo
      jq
      pkgsMaster.k9s
      libnotify
      # libreoffice
      libxml2
      lsb-release
      lsof
      mpc_cli
      mr
      mplayer
      mtools
      mu
      netcat-openbsd
      nethogs
      nixUnstable
      nix-index
      # nixops
      nmap
      openssl
      openvpn
      pandoc
      parallel
      parcellite
      patchelf
      pavucontrol
      pciutils
      pdfgrep
      pdftk
      pdfpc
      playerctl
      pmutils
      powertop
      psmisc
      pv
      pythonFull
      rlwrap
      rofi
      # (rWrapper.override {
      #   packages = with rPackages; [
      #     RColorBrewer
      #     ggplot2
      #     plyr
      #     sqldf
      #     xtable
      #     extrafont
      #   ];
      # })
      rsync
      rxvt_unicode-with-plugins
      sbt
      scala
      scalafmt
      scrot
      shellcheck
      spotify
      sshfs
      sqlite
      # slack
      stack
      sysdig
      system-config-printer
      tcpdump
      tdesktop
      terminator
      testdisk
      tigervnc
      (texlive.combine {
        inherit (texlive)
        scheme-medium
        beamer
        listings
        minted
        cleveref
        microtype
        babel
        todonotes
        chngcntr
        excludeonly
        upquote
        ifplatform
        xstring
        enumitem;
      })
      trayer
      tree
      unetbootin
      units
      unrar
      unzip
      usbutils
      up
      update-resolv-conf
      vim
      w3m
      wget
      # wine
      which
      xclip
      x11vnc
      xvfb_run
      zathura
      zip
      zlib
      # zoom-us
      zsh
    ] ++ (with pkgs.pythonPackages; [
      ipython
      youtube-dl
      pygments
    ]) ++ (with pkgs.haskellPackages; [
      # cabal2nix
      # hasktags
      hindent
      hlint
      # idris
      structured-haskell-mode
      xmobar
    ]) ++ (with pkgs.xorg; [
      xbacklight
      xev
      xkill
    ]) ++ (with pkgs.nodePackages; [
      js-yaml
    ]) ++ [ # photography related stuff
      darktable
      # rawtherapee
      hugin
      geeqie
    ] ++ [ # evaluation phase
      bat
      lnav
      pdd
      dateutils
      # cachix
      rclone
      bind
      pwgen
      keynav
    ] ++ (with pkgs.myScripts; [
      browserHistory
      gnuplot-quick
      isVpnActive
      multihead4k
      tmx
      ts
    ]) ++ [
      go
      glide
      go2nix
      dep2nix
      godef
    ];
  };
}
