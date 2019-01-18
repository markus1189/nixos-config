{ config, pkgs, ... }:

let
  wirelessInterface = pkgs.lib.head config.networking.wireless.interfaces;
  myScripts = import ./scripts.nix { inherit pkgs; };
in
{
  environment = with myScripts; {
    systemPackages = [

      acConnected
      acDisconnected
      center-mouse
      (chooseNetwork wirelessInterface)
      git-pretty-log
      gnuplot-quick
      lock-screen
      nix-frun
      multihead-left
      multihead-right
      multihead-4k
      singlehead
      select-spotify
      current-spotify-song
      sysdig-trace-in
      sysdig-trace-out
      toggleSoundMute
      take-screenshot
      ts
      (wpa-our-fritz-box wirelessInterface)
      (wpa-wanna-cry wirelessInterface)
      tmx
      xmonad-reset

    ];
    shellAliases = {
      "gl" = "${git-pretty-log}/bin/git-pretty-log";
    };
  };
}
