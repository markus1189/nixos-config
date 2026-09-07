{ config, pkgs, ... }:

{
  nixpkgs = { };

  programs.i3lock = {
    enable = true;
    package = pkgs.i3lock-color;
  };

  environment = {
    systemPackages =
      with pkgs;
      [
        ack
        actkbd
        agent-browser
        alsa-oss
        alsa-utils
        ammonite
        arandr
        aspell
        aspellDicts.de
        aspellDicts.en
        ast-grep
        awscli2
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
        dragon-drop
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
        ghc
        gimp
        mesa-demos
        gnumake
        gnuplot
        pup
        google-chrome
        gparted
        graphviz
        hicolor-icon-theme
        hledger
        hledger-ui
        hocket
        html2text
        inotify-tools
        inetutils
        imagemagick
        inkscape
        insomnia
        jqp # interactive jq queries
        k9s
        koreader
        libnotify
        llm
        libxml2
        lsb-release
        lsof
        marginal
        mitmproxy
        mpc
        mr
        mtools
        libressl.nc
        nethogs
        nixVersions.git
        nmap
        opencode
        openssl
        openvpn
        pandoc
        parallel
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
        rsync
        sbt
        scala
        scalafmt
        scrot
        shellcheck
        yamllint
        statix
        ffmpegthumbnailer
        mediainfo
        epub-thumbnailer
        vips # vipsthumbnail, for dirvish image previews
        p7zip
        signal-desktop
        spotify
        src-cli
        sshfs
        sqlite
        slack
        system-config-printer
        tcpdump
        telegram-desktop
        terminator
        testdisk
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
        which
        xclip
        xdotool
        xev
        xkill
        xvfb-run
        yt-dlp
        zathura
        zip
        zlib
        zoom-us
        zsh
      ]
      ++ (with pkgs.haskellPackages; [
        cabal2nix
        hlint
        pkgs.ormolu
        xmobar
      ])
      ++ [ pkgs.brightnessctl ]
      ++ [
        # photography related stuff
        darktable
        hugin
      ]
      ++ [
        # evaluation phase
        lnav # log file viewer
        pdd
        dateutils
        bind
      ]
      ++ (with pkgs.myScripts; [
        gnuplot-quick
        isVpnActive
        tmx
        ts
      ])
      ++ [
        # Go related stuff
        go
      ];
  };
}
