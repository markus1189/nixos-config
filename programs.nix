{ pkgs, ... }:

let
  pkgsMaster = import (fetchTarball https://github.com/NixOS/nixpkgs/archive/master.tar.gz) {};
  pkgsFork = import (fetchTarball https://github.com/markus1189/nixpkgs/archive/master.tar.gz) {};
  pkgsRelease17 = import (fetchTarball https://github.com/NixOS/nixpkgs/archive/release-17.09.tar.gz) {};
in
{
  nixpkgs = {
    config = {
      packageOverrides = pkgs: {
        ncmpcpp = pkgs.ncmpcpp.override { clockSupport = true; };
        dwarf-fortress = pkgs.dwarf-fortress.override {
          theme = pkgs.dwarf-fortress-packages.phoebus-theme;
          enableDFHack = true;
        };
      };

      firefox = {
        enableGoogleTalkPlugin = true;
        enableOfficialBranding = true;
      };

      chromium = {
        enableWideVine = false;
        enablePepperFlash = true;
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
      filezilla
      firefoxWrapper
      foo2zjs
      fortune
      gcc
      gdb
      ghc
      # ghostscript
      gimp
      gitFull
      (gitAndTools.git-extras)
      glxinfo
      gnumake
      gnupg
      gnupg1
      gnuplot
      go-pup
      gparted
      graphviz
      hicolor_icon_theme
      html2text
      htop
      httpie
      http-prompt
      i3lock
      jetbrains.idea-community
      inetutils
      imagemagick
      (builtins.trace "[INFO] using pinned imv version" pkgsRelease17.imv)
      inkscape
      insomnia
      jo
      jq
      libnotify
      # libreoffice
      libxml2
      lsb-release
      lsof
      mpc_cli
      mplayer
      mtools
      netcat-openbsd
      nethogs
      nixUnstable
      nix-index
      nixops
      nmap
      openssl
      openvpn
      oraclejdk
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
      spotify
      sqlite
      stack
      # sysdig
      system-config-printer
      tcpdump
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
      update-resolv-conf
      vim
      w3m
      wget
      wine
      which
      xclip
      x11vnc
      xvfb_run
      zathura
      zip
      zlib
      zoom-us
      zsh
    ] ++ (with pkgs.pythonPackages; [
      ipython
      youtube-dl
      pygments
    ]) ++ (with pkgs.haskellPackages; [
      cabal2nix
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
      rawtherapee
      hugin
    ] ++ [ # evaluation phase
      bat
      lnav
      pdd
    ];
  };
}
