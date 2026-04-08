{ config, pkgs, ... }:

let
  hostsFile = "${pkgs.ndtSources.hosts}/alternates/fakenews-gambling-porn/hosts";
  modifiedHosts = pkgs.runCommand "filtered-hosts" {} ''
    # Domain allowlist exceptions
    ${pkgs.gnused}/bin/sed \
      -e '/link.m.convertkit/d' \
      -e '/\.strava.com/d' \
      -e '/\.split\.io/d' \
      -e '/wl.spotify.com/d' \
      -e '/fvs.io/d' \
      -e '/scrolller.com/d' \
      ${hostsFile} \
    | ${pkgs.gawk}/bin/awk '/^[^#]/ && length($2) >= 50 { next } { print }' \
    > $out  # dnsmasq 2.92 IDN bug: hostnames >= 50 chars cause buffer overflow in read_hostsfile
  '';
in
{
  networking = {
    extraHosts = builtins.readFile modifiedHosts;
  };
}
