rec {
  ndtOverlay = self: super: {
    ndt = import (builtins.fetchTarball
      "https://github.com/markus1189/ndt/archive/master.tar.gz") {
        nixpkgs = self;
        ghc = "ghc948";
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
    let
      pypkgs = with self.python3Packages;
        [
          requests
          sh
          pytimeparse
          tomli
          # plugins that will soon already be included
          importlib-metadata
          faker
          pdfminer
          praw
          psutil
          s3fs
        ];
    in {
      visidata = builtins.trace
        "INFO: Using visidata overlay for more python packages and develop branch [${self.ndtSources.visidata.date} @ ${self.ndtSources.visidata.rev}]"
        super.visidata.overridePythonAttrs (old: {
          propagatedBuildInputs = old.propagatedBuildInputs ++ pypkgs;
          src = self.ndtSources.visidata.outPath;
          doCheck = false;
          patches = [ ];
        });
    };

  xclipOverlay = self: super: {
    xclip = builtins.trace "INFO: Using xclip overlay for newer version"
      super.xclip.overrideAttrs (old: {
        version = self.ndtSources.xclip.rev;
        src = self.ndtSources.xclip.outPath;
      });
  };

  overlays = [
    ndtOverlay
    ndtSourcesOverlay
    wallpapersOverlay
    visidataOverlay
    xclipOverlay
  ];
}
