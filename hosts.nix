{ config, pkgs, ... }:

let
  hostsCommit = "0d5fd167a8dc7b8e91475e07d831857a1d1602c1";
  hostsFile = pkgs.fetchurl {
    url = "https://raw.githubusercontent.com/StevenBlack/hosts/${hostsCommit}/alternates/fakenews-gambling-porn/hosts";
    sha256 = "1zpgmdwz5vrkxdab4xqi8na0298v164lll74307w3qr2z1disp6y";
  };
in
{
  networking = {
    extraHosts = builtins.readFile hostsFile;
  };
}
