{ config, pkgs, ... }:

let
  hostsCommit = "b7d496e9649df080f3bdb88b8c4afa3faf2c5237";
  hostsFile = pkgs.fetchurl {
    url = "https://raw.githubusercontent.com/StevenBlack/hosts/${hostsCommit}/alternates/fakenews-gambling-porn/hosts";
    sha256 = "1ni9hj9g0v3hssbfcgwavvb4za0rvvzp5cdjj10x2mxqcd6lnv6n";
  };
  modifiedHosts = pkgs.runCommand "filtered-hosts" {} ''
    echo "# Modifed version of ${hostsFile}" > $out
    # Accept this because of farnam street links...
    ${pkgs.gnused}/bin/sed -i '/link.m.convertkit/d' $out
    # For breuninger development
    ${pkgs.gnused}/bin/sed -i '/richrelevance.com/d' $out
  '';
in
{
  networking = {
    extraHosts = builtins.readFile modifiedHosts;
  };
}
