{ pkgs ? import <nixpkgs> {
  overlays = [
    (
      self: super: {
        ndt = import (builtins.fetchGit {
        name = "ndt-fetchgit";
        url = "https://github.com/markus1189/ndt/";
        rev = "47670efb0ec13df6710ea394d2b305740c9685b8";
      }) {
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
