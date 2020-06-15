{ config, pkgs, ... }:

let
  hostsFile = "${pkgs.nivSources.hosts}/alternates/fakenews-gambling-porn/hosts";
  modifiedHosts = pkgs.runCommand "filtered-hosts" {} ''
    # Accept this because of farnam street links...
    ${pkgs.gnused}/bin/sed -e '/link.m.convertkit/d' -e '/scrolller.com/d' ${hostsFile} > $out
  '';
in
{
  networking = {
    extraHosts = builtins.readFile modifiedHosts;
  };
}
