{ config, pkgs, ... }:

let
  pinnedHostsVersion = pkgs.lib.importJSON ../pinned-versions/hosts-version.json;
  hostsRepo = pkgs.fetchgit {
    inherit (pinnedHostsVersion) url rev sha256 fetchSubmodules;
  };
  modifiedHosts = pkgs.runCommand "filtered-hosts" {} ''
    # Accept this because of farnam street links...
    ${pkgs.gnused}/bin/sed -e '/link.m.convertkit/d' -e '/scrolller.com/d' ${hostsRepo}/alternates/fakenews-gambling-porn/hosts > $out
  '';
in
{
  networking = {
    extraHosts = builtins.readFile modifiedHosts;
  };
}
