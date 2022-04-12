{ pkgs ? import <nixpkgs> {
  overlays = [
    (
      self: super: {
        ndt = import (builtins.fetchTarball https://github.com/markus1189/ndt/archive/master.tar.gz) {
          nixpkgs = self;
          ghc = "ghc8107";
        };
      }
    )
  ];
}
, sourcesFile ? ./sources.json
}:

let
  sourcesDrv = pkgs.runCommand "init-sources" {} "${pkgs.ndt}/bin/ndt -s ${sourcesFile} print > $out";
  sources = import sourcesDrv { inherit pkgs sourcesFile; } ;
in
sources
