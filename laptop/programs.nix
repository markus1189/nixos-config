{ config, pkgs, ... }:

{
  nixpkgs = { };

  programs.i3lock = {
    enable = true;
    package = pkgs.i3lock-color;
  };

  # nh reimplements nixos-rebuild in Rust: a nix-output-monitor build tree, a
  # closure diff, then a confirmation prompt before activation -- the
  # `nixos-rebuild build` / `nix store diff-closures` / `switch` sequence that
  # docs/derivation-diffing.md spells out by hand, as one command.
  #
  # The diff comes from dix linked as a *library crate* (crates/nh-diff), not
  # from the `dix` binary, so the standalone CLI in common-packages.nix is a
  # separate tool for ad-hoc `dix <genA> <genB>` -- not a dependency of this.
  #
  # Laptop-only on purpose: `flake` is a local path, and nuc rebuilds from
  # github: via system.autoUpgrade instead.
  programs.nh = {
    enable = true;
    # Default target for a bare `nh os switch`, so it resolves from any
    # directory. The host attr still comes from the hostname, as with
    # activate.sh -- p1's nixos-p1 alias in flake.nix keeps working.
    flake = "/home/${config.my.userName}/repos/nixos-config";

    # Deliberately off, matching `nix.gc.automatic = false` in laptop.nix:
    # collection here stays a manual decision. Run it by hand when wanted:
    #   nh clean all --keep-since 5d --keep 3
    # It is gcroot- and direnv-aware, which plain nix-collect-garbage is not.
    clean.enable = false;
  };

  environment = {
    systemPackages =
      with pkgs;
      [
        #
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
        imv
        inkscape
        insomnia
        jqp # interactive jq queries
        k9s
        keynav
        koreader
        libnotify
        # libreoffice
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
        yamllint
        statix
        ffmpegthumbnailer
        mediainfo
        epub-thumbnailer
        p7zip
        signal-desktop
        spotify
        src-cli
        sshfs
        sqlite
        slack
        # sysdig
        system-config-printer
        tcpdump
        telegram-desktop
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
      ++ (with pkgs.pythonPackages; [
        # pygments
      ])
      ++ (with pkgs.haskellPackages; [
        cabal2nix
        # hasktags
        hlint
        # idris
        # pkgs.stack
        pkgs.ormolu
        xmobar
      ])
      ++ [ pkgs.brightnessctl ]
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
        # wxhexeditor  # commented out due to build failure with mhash dependency
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
      ];
  };
}
