{ pkgs ? import <nixpkgs> {
  overlays = [
    (
      self: super: {
        ndt = import (builtins.fetchGit {
        name = "ndt-fetchgit";
        url = "https://github.com/markus1189/ndt/";
        rev = "844e1da99390fb4c95dad0d8931f5d147c8895eb";
      }) {
          nixpkgs = self;
          ghc = "ghc912";
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
