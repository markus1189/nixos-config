{ config, pkgs, ...}:

let
in
{
  hardware = {
    bluetooth = {
      enable = true;
      package = pkgs.bluez;
      powerOnBoot = true;
      settings = {
        General = {
          ControllerMode = "bredr";
          Enable = "Source,Sink,Media,Socket";
          Experimental = true;
        };
        Policy = {
          AutoEnable = true;
        };
      };
    };
  };

  services.blueman.enable = true;
}
