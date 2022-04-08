{ config, pkgs, ... }:

let ndtSources = import ../ndt/sources.nix { };
in {
  imports = [ "${ndtSources.agenix}/modules/age.nix" ];

  environment = {
    systemPackages =
      [ (pkgs.callPackage "${ndtSources.agenix}/pkgs/agenix.nix" {}) ];
  };
}
