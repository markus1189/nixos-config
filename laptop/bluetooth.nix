{ config, pkgs, ...}:

let
  # Fix bluez disconnects from staging, until it's reached the system channel...
  bluezPkg = import (builtins.fetchurl "https://raw.githubusercontent.com/NixOS/nixpkgs/3e0b9f5/pkgs/os-specific/linux/bluez/default.nix");
  bluez58 = pkgs.callPackage bluezPkg {};
in
{
  hardware = {
    bluetooth = {
      enable = true;
      package = bluez58;
      settings = {
        General = {
          ControllerMode = "bredr";
          Enable = "Source,Sink,Media,Socket";
        };
      };
    };
  };
}
