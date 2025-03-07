{ pkgs, ... }:

let
  ndtSources = import ../ndt/sources.nix { };
  nixpkgs2305 = import ndtSources.nixpkgs-2305 {
    config = {
      allowUnfreePredicate = pkg:
        builtins.elem (pkgs.lib.getName pkg) [ "zoom" ];
    };
  };
  my-llm = rec {
    pyWithPackages = (pkgs.python3.withPackages (ps: [
      (ps.llm.overridePythonAttrs (old: { doCheck = false; }))
      (pkgs.callPackage ../nixos-shared/llm-packages/llm-bedrock-anthropic { source = ndtSources.llm-bedrock-anthropic.outPath;})
      # (pkgs.callPackage ../nixos-shared/llm-packages/llm-gemini { source = ndtSources.llm-gemini.outPath;})
      (pkgs.callPackage ../nixos-shared/llm-packages/llm-perplexity { source = ndtSources.llm-perplexity.outPath;})
    ]));
    llm = pkgs.runCommandNoCCLocal "llm" { } ''
      mkdir -p $out/bin
      ln -s ${pyWithPackages}/bin/llm $out/bin/llm
    '';
  }.llm;
in {
  nixpkgs = { config = { firefox = { enableOfficialBranding = true; }; }; };

  environment = {
    systemPackages = with pkgs;
      [
        my-llm
        #
        ack
        actkbd
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
      ] ++ (with pkgs.pythonPackages;
        [
          # pygments
        ]) ++ (with pkgs.haskellPackages; [
          cabal2nix
          # hasktags
          hlint
          # idris
          # pkgs.stack
          (pkgs.ormolu)
          xmobar
        ]) ++ (with pkgs.xorg; [ xbacklight xev xkill ])
      ++ (with pkgs.nodePackages; [ js-yaml ]) ++ [
        # photography related stuff
        darktable
        hugin
        geeqie
      ] ++ [
        # evaluation phase
        lnav # log file viewer
        pdd
        dateutils
        bind
        wxhexeditor
      ]
      ++ (with pkgs.myScripts; [ gnuplot-quick isVpnActive multihead4k tmx ts ])
      ++ [
        # Go related stuff
        go
      ];
  };
}
