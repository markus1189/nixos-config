{ config, pkgs, ... }:

let
  dunstLogger = pkgs.mutate ./dunst-logger.sh {
    inherit (pkgs) jo systemd;
  };
  dunstrc = pkgs.mutate ./dunstrc {
    inherit (pkgs) rofi;
    inherit dunstLogger;
    sourceCodePro = pkgs.source-code-pro;
  };
in
{
  services = {
    xserver = {
      displayManager = {
        sessionCommands = ''
          ${pkgs.procps}/bin/pkill dunst || true
          ${pkgs.dunst}/bin/dunst -config ${dunstrc} &
        '';
      };
    };
  };

  environment = {
    systemPackages = with pkgs; [
      dunst
    ];
  };
}
