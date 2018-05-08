{ config, pkgs, ...}:

{
  hardware = {
    bluetooth = {
      enable = true;
      extraConfig = ''
        [General]
        ControllerMode = bredr
      '';
    };

    pulseaudio = {
      enable = true;
      package = pkgs.pulseaudioFull;
    };
  };
}
