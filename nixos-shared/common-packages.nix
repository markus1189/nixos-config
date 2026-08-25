{
  config,
  inputs,
  pkgs,
  ...
}:

{
  imports = [ ./python-env.nix ];

  environment = {
    systemPackages =
      (with pkgs.masterPkgs; [ claude-code ])
      # The agenix NixOS module comes from flake.nix; this is just the CLI.
      ++ [ inputs.agenix.packages.${pkgs.stdenv.hostPlatform.system}.default ]
      ++ (with pkgs; [
        aws-vault
        bat
        binutils
        cachix
        dnsutils
        ed
        ffmpeg-full
        gnupg
        gnupg1
        gitFull
        gh
        git-extras
        git-open
        hub
        adwaita-icon-theme
        gron
        htop
        httpie
        iotop
        jo
        jq
        jless
        moreutils
        ncdu
        nix-prefetch-docker
        nix-prefetch-git
        nix-prefetch-github
        # Interactive closure browser: per-node NAR/closure/added size, and `w`
        # for why-depends (which path drags a store path into the system).
        nix-tree
        # Standalone closure differ -- reads Nix's SQLite db directly instead of
        # shelling out, so it is ~16x nvd on this tree and also reports the size
        # delta per package. nh links the same engine as a crate for rebuilds;
        # this binary is for comparing two arbitrary paths or generations.
        # Reading the db directly races a concurrent writer (a build or GC);
        # `--force-correctness` trades the speed for a full re-check, e.g. mid-
        # autoUpgrade on nuc.
        dix
        nixfmt
        nixpkgs-lint
        notifySendPb
        notifySendTelegram
        notifySendTelegramHtml
        notifySendTelegramMd
        poppler-utils
        sendTelegramPoll
        viessmannOutsideTemperature
        telegramPhotosLastYear
        pwgen
        yq
        viddy
        visidata
        watchexec
        wormhole-william
      ])
      ++ (with pkgs; [
        myScripts.logArgs
        myScripts.recordMeeting
        myScripts.tmuxPollPane
        myScripts.chronic-file
        myScripts.gemini-vision
      ]);
  };
}
