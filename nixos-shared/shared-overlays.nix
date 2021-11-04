rec {
  ndtOverlay = self: super: {
    ndt = import (builtins.fetchTarball
      "https://github.com/markus1189/ndt/archive/master.tar.gz") {
        nixpkgs = self;
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

  visidataOverlay = self: super: {
    visidata = builtins.trace "INFO: Using visidata overlay"
      super.visidata.overridePythonAttrs (old: {
        propagatedBuildInputs = old.propagatedBuildInputs
          ++ (with self.python3Packages; [ sh ]);
      });
  };

  overlays = [ ndtOverlay ndtSourcesOverlay wallpapersOverlay visidataOverlay ];
}
