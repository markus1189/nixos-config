{ config, pkgs, ...}:

{
  hardware = {
    bluetooth = {
      enable = true;
    };

    pulseaudio = {
      enable = true;
      package = pkgs.pulseaudioFull;
    };
  };
}
