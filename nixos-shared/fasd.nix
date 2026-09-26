{ config, pkgs, ... }:

{
  environment = {
    systemPackages = with pkgs; [ fasd ];
    interactiveShellInit = ''
      # The whjvenyl fork records a repo's root instead of $PWD by default
      export _FASD_ONLY_VCS=0
      eval "$(${pkgs.fasd}/bin/fasd --init auto)"
    '';
  };
}
