{ config, pkgs, ... }:

let
  hostsCommit = "b6346cbf7a88e2607476048fe43a22d52a05bbee";
  hostsFile = pkgs.fetchurl {
    url = "https://raw.githubusercontent.com/StevenBlack/hosts/${hostsCommit}/alternates/fakenews-gambling-porn/hosts";
    sha256 = "0755vpzhy0wcgc0lpfcmbkmbdrm42x6w5znlyjbdfxr6lkbpqp0s";
  };
in
{
  networking = {
    extraHosts = builtins.readFile hostsFile;
  };
}
