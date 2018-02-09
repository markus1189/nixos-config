{ config, pkgs, ... }:

let
  dunstrc = pkgs.mutate ./dunstrc { sourceCodePro = pkgs.source-code-pro; };
in
{
  services = {
    xserver = {
      displayManager = {
        sessionCommands = ''
          ${pkgs.psmisc}/bin/killall dunst || true
          ${pkgs.dunst}/bin/dunst -config ${dunstrc} -follow mouse &
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
