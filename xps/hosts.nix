{ config, pkgs, ... }:

let
  hostsCommit = "5386cec268756eaeb2348c1bdffbe961a8097898";
  hostsFile = pkgs.fetchurl {
    url = "https://raw.githubusercontent.com/StevenBlack/hosts/${hostsCommit}/alternates/fakenews-gambling-porn/hosts";
    sha256 = "05kiwmn4nn45v8k3d3hi2nn2rjhdx057plx5r5yarwiqw2m4x7x1";
  };
  modifiedHosts = pkgs.runCommand "filtered-hosts" {} ''
    echo "# Modifed version of ${hostsFile}" > $out
    # Accept this because of farnam street links...
    ${pkgs.gnused}/bin/sed '/link.m.convertkit/d' ${hostsFile} > $out
  '';
in
{
  networking = {
    extraHosts = builtins.readFile modifiedHosts;
  };
}
