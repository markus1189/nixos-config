{ config, pkgs, ... }:

let
  nixpkgsMasterSrc = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/master.tar.gz";
  };
  nixpkgsMaster = import nixpkgsMasterSrc {
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
      (with nixpkgsMaster; [ claude-code opencode ])
      ++ (with pkgs; [
        aws-vault
        bat
        binutils
        cachix
        comma
        dnsutils
        ed
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
        nixfmt-rfc-style
        nixpkgs-lint
        nixpkgs-fmt
        niv
        notifySendPb
        notifySendTelegram
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
      ]);
  };
}
