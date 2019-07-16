{ config, pkgs, ... }:

let
  pinnedHostsVersion = pkgs.lib.importJSON ../pinned-versions/hosts-version.json;
  hostsCommit = "5386cec268756eaeb2348c1bdffbe961a8097898";
  hostsRepo = pkgs.fetchgit {
    inherit (pinnedHostsVersion) url rev sha256 fetchSubmodules;
  };
  modifiedHosts = pkgs.runCommand "filtered-hosts" {} ''
    # Accept this because of farnam street links...
    ${pkgs.gnused}/bin/sed '/link.m.convertkit/d' ${hostsRepo}/alternates/fakenews-gambling-porn/hosts > $out
  '';
in
{
  networking = {
    extraHosts = builtins.readFile modifiedHosts;
  };
}
