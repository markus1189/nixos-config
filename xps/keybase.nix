{ config, pkgs, ... }:

{
  services = {
    keybase.enable = true;
  };

  environment = {
    systemPackages = with pkgs; [
      keybase
      kbfs
    ];
  };
}