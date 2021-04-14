{ config, pkgs, ...}:

let
in
{
  hardware = {
    bluetooth = {
      enable = true;
      package = pkgs.bluezFull;
      settings = {
        General = {
          ControllerMode = "bredr";
          Enable = "Source,Sink,Media,Socket";
        };
      };
    };
  };
}
