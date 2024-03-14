host:
{ config, pkgs, ... }:

let
  secrets = import ./secrets.nix;
in
{
  networking = {
    wg-quick = {
      interfaces = {
        wg-nyc = secrets.wireguard.mozilla.${host}.wg-nyc501;
      };
    };
  };
}
