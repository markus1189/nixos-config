rec {
  ndtOverlay = self: super: {
    ndt = import (builtins.fetchTarball
      "https://github.com/markus1189/ndt/archive/master.tar.gz") {
        nixpkgs = self;
        ghc = "ghc912";
      };
  };

  ndtSourcesOverlay = self: super: {
    ndtSources = import ../ndt/sources.nix { };
  };

  wallpapersOverlay = self: super: {
    markus-wallpapers = {
      orange-cube-left     = ./assets/wallpapers/orange-cube-6x5-left.png;
      orange-cube-right    = ./assets/wallpapers/orange-cube-6x5-right.png;
      orange-cube-internal = ./assets/wallpapers/orange-cube-16x9.png;
    };
  };

  visidataOverlay = self: super:
    let
      pypkgs = with self.python3Packages; [
        requests
        sh
        pytimeparse
        tomli
        # plugins that will soon already be included
        importlib-metadata
        faker
        pdfminer-six
        praw
        psutil
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
