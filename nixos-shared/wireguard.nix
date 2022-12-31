host:
{ config, pkgs, ... }:

let
  secrets = import ./secrets.nix;
in
{
  networking = {
    wg-quick = {
      interfaces = {
        wg-us118 = secrets.wireguard.mozilla.${host}.wg-us118;
      };
    };
  };
}
