{ config, pkgs, ...}:

{
  hardware = {
    bluetooth = {
      enable = true;
      config = {
        General = {
          ControllerMode = "bredr";
          Enable = "Source,Sink,Media,Socket";
        };
      };
    };
  };
}
