{ pkgs ? import <nixpkgs> {
  overlays = [
    (self: super: {
      ndt = import ../nixos-shared/ndt-pinned.nix { nixpkgs = self; };
    })
  ];
}
, sourcesFile ? ./sources.json
}:

let
  sourcesDrv = pkgs.runCommand "init-sources" {} "${pkgs.ndt}/bin/ndt -s ${sourcesFile} print > $out";
  sources = import sourcesDrv { inherit pkgs sourcesFile; } ;
in
sources
