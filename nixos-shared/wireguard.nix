host:
{ config, pkgs, ... }:

let
  secrets = import ./secrets.nix;
in
{
  networking = {
    wg-quick = {
      interfaces = {
        wg-nyc301 = secrets.wireguard.mozilla.${host}.wg-nyc301;
      };
    };
  };
}
