{ pkgs ? import <nixpkgs> {
  overlays = [
    (
      self: super: {
        ndt = import (builtins.fetchGit {
        name = "ndt-fetchgit";
        url = "https://github.com/markus1189/ndt/";
        rev = "9b2ad45bd46675e8e7ceb62a90b86e3f54dd5cab";
      }) {
          nixpkgs = self;
          ghc = "ghc902";
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
