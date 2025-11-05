{ pkgs, ... }:

let
  ndtSources = import ../ndt/sources.nix { };
in
{
  nixpkgs = { };

  environment = {
    systemPackages = (
      with pkgs;
      [
        #
        ack
        actkbd
        aider-chat-full
        alsa-oss
        alsa-utils
        ammonite
        arandr
        aspell
        aspellDicts.de
        aspellDicts.en
        ast-grep
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
        dool
        emacs
        entr
        envsubst
        evince
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
        gemini-cli-bin
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
        i3lock
        inotify-tools
        inetutils
        imagemagick
        imv
        inkscape
        insomnia
        jqp # interactive jq queries
        keynav
        k9s
        libnotify
        # libreoffice
        llm
        libxml2
        lsb-release
        lsof
        mitmproxy
        mpc_cli
        mr
        mtools
        mu
        libressl.nc
        nethogs
        nixVersions.git
        nix-index
        nmap
        openssl
        openvpn
        pandoc
        parallel
        parcellite
        patchelf
        pavucontrol
        pwvucontrol
        pciutils
        pdfgrep
        pdftk
        pdfpc
        playerctl
        pmutils
        powertop
        prettier
        psmisc
        pv
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
        rxvt-unicode
        sbt
        scala
        scalafmt
        scrot
        shellcheck
        spotify
        src-cli
        sshfs
        sqlite
        slack
        # sysdig
        system-config-printer
        tcpdump
        tdesktop
        terminator
        testdisk
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
        html-tidy
        trayer
        tree
        units
        unrar
        unzip
        usbutils
        up
        update-resolv-conf
        vim
        warpd
        w3m
        wget
        # wine
        which
        xclip
        xdotool
        x11vnc
        xvfb-run
        yt-dlp
        zathura
        zip
        zlib
        zoom-us
        zsh
      ]
      ++ (with pkgs.pythonPackages; [
        # pygments
      ])
      ++ (with pkgs.haskellPackages; [
        cabal2nix
        # hasktags
        hlint
        # idris
        # pkgs.stack
        (pkgs.ormolu)
        xmobar
      ])
      ++ (with pkgs.xorg; [
        xbacklight
        xev
        xkill
      ])
      ++ (with pkgs.nodePackages; [ js-yaml ])
      ++ [
        # photography related stuff
        darktable
        hugin
        # geeqie # libsoup, but pr open
      ]
      ++ [
        # evaluation phase
        lnav # log file viewer
        pdd
        dateutils
        bind
        wxhexeditor
      ]
      ++ (with pkgs.myScripts; [
        gnuplot-quick
        isVpnActive
        multihead4k
        tmx
        ts
      ])
      ++ [
        # Go related stuff
        go
      ]
    );
  };
}
