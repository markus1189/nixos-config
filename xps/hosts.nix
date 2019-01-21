{ config, pkgs, ... }:

let
  hostsCommit = "4d9b648";
  hostsFile = pkgs.fetchurl {
    url = "https://raw.githubusercontent.com/StevenBlack/hosts/${hostsCommit}/alternates/fakenews-gambling-porn/hosts";
    sha256 = "19dqbiyhi76ri384qc7aymj7j0v2i92lkyqyplyyzzb1xpl4gbpc";
  };
  filteredHosts = pkgs.runCommand "filtered-hosts" {} ''
    # Accept this because of farnam street links...
    echo "# Filtered version of ${hostsFile}" > $out
    ${pkgs.gnugrep}/bin/grep -Fv "link.m.convertkit" ${hostsFile} >> $out
  '';
in
{
  networking = {
    extraHosts = builtins.readFile filteredHosts;
  };
}
