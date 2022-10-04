{ config, pkgs, ...}:

let
in
{
  hardware = {
    bluetooth = {
      enable = true;
      package = pkgs.bluez;
      settings = {
        General = {
          ControllerMode = "bredr";
          Enable = "Source,Sink,Media,Socket";
        };
      };
    };
  };
}
