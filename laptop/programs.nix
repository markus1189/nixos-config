{ pkgs, ... }:

{
  nixpkgs = {
    config = {
      firefox = {
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
      buku
      cabal-install
      chromedriver
      chromium
      cloc
      coreutils
      discord
      docker-compose
      dmenu
      ddgr
      deluge
      xdragon
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
      foo2zjs
      fortune
      gcc
      gdb
      ghc
      gimp
      glxinfo
      gnumake
      gnuplot
      pup
      google-chrome
      gparted
      graphviz
      hicolor-icon-theme
      hledger
      hledger-ui
      html2text
      httpie
      i3lock
      inotify-tools
      jetbrains.idea-community
      inetutils
      imagemagick
      imv
      inkscape
      insomnia
      keynav
      k9s
      libnotify
      # libreoffice
      libxml2
      lsb-release
      lsof
      mitmproxy
      mpc_cli
      mr
      mplayer
      mtools
      mu
      libressl.nc
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
      rclone
      remind
      wyrd
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
      slack
      sysdig
      system-config-printer
      tcpdump
      tdesktop
      terminator
      testdisk
      #tigervnc
      # (texlive.combine {
      #   inherit (texlive)
      #   scheme-medium
      #   beamer
      #   listings
      #   minted
      #   cleveref
      #   microtype
      #   babel
      #   todonotes
      #   chngcntr
      #   excludeonly
      #   upquote
      #   ifplatform
      #   xstring
      #   enumitem;
      # })
      trayer
      tree
      # unetbootin
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
      xdotool
      x11vnc
      xvfb_run
      yt-dlp
      youtube-dl
      zathura
      zip
      zlib
      zoom-us
      zsh
    ] ++ (with pkgs.pythonPackages; [
      pygments
    ]) ++ (with pkgs.haskellPackages; [
      cabal2nix
      # hasktags
      hlint
      # idris
      # pkgs.stack
      structured-haskell-mode
      (pkgs.ormolu)
      xmobar
    ]) ++ (with pkgs.xorg; [
      xbacklight
      xev
      xkill
    ]) ++ (with pkgs.nodePackages; [
      js-yaml
    ]) ++ [ # photography related stuff
      darktable
      hugin
      geeqie
    ] ++ [ # evaluation phase
      lnav # log file viewer
      pdd
      dateutils
      bind
      wxhexeditor
    ] ++ (with pkgs.myScripts; [
      browserHistory
      gnuplot-quick
      isVpnActive
      multihead4k
      tmx
      ts
    ]) ++ [ # Go related stuff
      go
      glide
      go2nix
      dep2nix
      godef
    ];
  };
}
