rec {
  ndtOverlay = self: super:
    let
      pinnedPkgs = import (builtins.fetchGit {
        name = "nixpkgs-pinned-for-ndt";
        url = "https://github.com/nixos/nixpkgs/";
        rev = "4d60081494259c0785f7e228518fee74e0792c1b";
      }) {};
    in {
      ndt = import (builtins.fetchTarball
        "https://github.com/markus1189/ndt/archive/master.tar.gz") {
          nixpkgs = pinnedPkgs;
          ghc = "ghc884";
        };
    };

  ndtSourcesOverlay = self: super: {
    ndtSources = import ../ndt/sources.nix { };
  };

  wallpapersOverlay = self: super: {
    markus-wallpapers = rec {
      cc = super.fetchurl {
        url = "https://www.dropbox.com/s/hoy6xdgnudwy64g/codecentric.jpg?dl=1";
        sha256 = "0z8ia5gv1r3brr7a2cc3sv0ffi0vqdmwj85b404702v7sb7f2d1k";
      };

      library = super.fetchurl {

      };

      shrike-rape-10x8 = cc;
      shrike-rape-10x8-flipped = cc;

      shrike-rape-21x9 = cc;
    };
  };

  visidataOverlay = self: super:
    let pkgs = with self.python3Packages; [ requests sh pytimeparse ];
    in {
      visidata =
        builtins.trace "INFO: Using visidata overlay for more python packages"
        super.visidata.overridePythonAttrs
        (old: { propagatedBuildInputs = old.propagatedBuildInputs ++ pkgs; });
    };

  overlays = [ ndtOverlay ndtSourcesOverlay wallpapersOverlay visidataOverlay ];
}
