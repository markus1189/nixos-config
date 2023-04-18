{ config, pkgs, ... }:

let
  hostsFile = "${pkgs.ndtSources.hosts}/alternates/fakenews-gambling-porn/hosts";
  modifiedHosts = pkgs.runCommand "filtered-hosts" {} ''
    # Some exceptions
    ${pkgs.gnused}/bin/sed \
      -e '/link.m.convertkit/d' \
      -e '/\.strava.com/d' \
      -e '/\.split\.io/d' \
      -e '/wl.spotify.com/d' \
      -e '/fvs.io/d' \
      -e '/scrolller.com/d' \
      ${hostsFile} > $out
  '';
in
{
  networking = {
    extraHosts = builtins.readFile modifiedHosts;
  };
}
