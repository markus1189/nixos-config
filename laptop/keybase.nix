{ config, pkgs, ... }:

{
  services = {
    keybase.enable = false;
  };

  environment = {
    systemPackages = with pkgs; [
      keybase
      kbfs
    ];
  };
}
