{ config, pkgs, ...}:

{
  environment = {
    systemPackages = with pkgs; [ fasd ];
    interactiveShellInit = ''
      eval "$(${pkgs.fasd}/bin/fasd --init auto)"
    '';
  };
}
