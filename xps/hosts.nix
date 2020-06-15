{ config, pkgs, ... }:

let
  modifiedHosts = pkgs.runCommand "filtered-hosts" {} ''
    # Accept this because of farnam street links...
    ${pkgs.gnused}/bin/sed -e '/link.m.convertkit/d' -e '/scrolller.com/d' ${pkgs.nivSources.hosts} > $out
  '';
in
{
  networking = {
    extraHosts = builtins.readFile modifiedHosts;
  };
}
