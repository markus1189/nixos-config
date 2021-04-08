{ pkgs, ... }:

let
  web2nix = { pkgs ? import <nixpkgs> {}, url, name ? url, ... }:
    pkgs.writeScriptBin name ''
      #!${pkgs.runtimeShell}
      export LIBGL_DRI3_DISABLE=1
      exec ${pkgs.chromium}/bin/chromium --app=${pkgs.lib.escapeShellArg url}
    '';
  teamboard-abschluss = web2nix { name = "teamboard-abschluss"; url = "https://miro.com/app/board/o9J_ks-8k-s=/?moveToWidget=3074457348190738188&cot=13"; };
  gmail = account: web2nix { name = "gmail${toString account}"; url = "https://mail.google.com/mail/u/${toString account}/#inbox"; };
in
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
      docker_compose
      dmenu
      ddgr
      deluge
      dragon-drop
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
      go-pup
      googleearth
      google-chrome
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
      rclone
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
      x11vnc
      xvfb_run
      zathura
      zip
      zlib
      zoom-us
      zsh
    ] ++ (with pkgs.pythonPackages; [
      youtube-dl
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
