{ config, pkgs, ... }:

let
  hostsCommit = "d0af61d0ef92117b35342c934a94812657517664";
  hostsFile = pkgs.fetchurl {
    url = "https://raw.githubusercontent.com/StevenBlack/hosts/${hostsCommit}/alternates/fakenews-gambling-porn/hosts";
    sha256 = "19sn7mc4bg5h2km8yvpr3rx9nw90srxp8qxjgxvbj2db8rzd5l9v";
  };
in
{
  networking = {
    extraHosts = builtins.readFile hostsFile;
  };
}
