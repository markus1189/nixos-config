{ config, pkgs, inputs, ... }:

let
  # nixpkgs master as a flake input, pinned via flake.lock (bump with
  # `nix flake update nixpkgs-master`) — the old unpinned fetchTarball
  # cannot be evaluated in pure flake mode.
  nixpkgsMaster = import inputs.nixpkgs-master {
    system = pkgs.stdenv.hostPlatform.system;
    config = {
      allowUnfreePredicate = pkg: builtins.elem (pkgs.lib.getName pkg) [ "claude-code" ];
      firefox = {
        enableOfficialBranding = true;
      };
    };
  };
in

{
  environment = {
    systemPackages =
      (with nixpkgsMaster; [ claude-code ])
      ++ (with pkgs; [
        aws-vault
        bat
        binutils
        cachix
        comma
        dnsutils
        ed
        ffmpeg-full
        # edbrowse
        gnupg
        gnupg1
        gitFull
        gh
        git-extras
        git-open
        hub
        git-secret
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
        nix-index
        nixfmt
        nixpkgs-lint
        nixpkgs-fmt
        niv
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
        (myScripts.logArgs)
        (myScripts.recordMeeting)
        (myScripts.tmuxPollPane)
        (myScripts.chronic-file)
        (myScripts.gemini-vision)
      ]);
  };
}
