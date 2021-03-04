{ config, pkgs, ...}:

{
  hardware = {
    bluetooth = {
      enable = true;
      settings = {
        General = {
          ControllerMode = "bredr";
          Enable = "Source,Sink,Media,Socket";
        };
      };
    };
  };
}
