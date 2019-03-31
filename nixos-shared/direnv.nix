{ config, pkgs, ... }:

let
  direnvHook = pkgs.runCommand "direnv-zsh-hook" {} ''
    mkdir $out
    ${pkgs.direnv}/bin/direnv hook zsh > $out/direnv-zsh-hook
  '';
in
{
  environment = {
    systemPackages = with pkgs; [ direnv ];
  };

  programs = {
    zsh = {
      interactiveShellInit = pkgs.lib.readFile "${direnvHook}/direnv-zsh-hook";
    };
  };
}
