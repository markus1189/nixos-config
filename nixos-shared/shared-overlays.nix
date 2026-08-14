inputs: rec {
  wallpapersOverlay = self: super: {
    markus-wallpapers = {
      orange-cube-left = ./assets/wallpapers/orange-cube-6x5-left.png;
      orange-cube-right = ./assets/wallpapers/orange-cube-6x5-right.png;
      orange-cube-internal = ./assets/wallpapers/orange-cube-16x9.png;
    };
  };

  visidataOverlay =
    self: super:
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
    in
    {
      visidata =
        builtins.trace
          "INFO: Using visidata overlay for more python packages and develop branch [${inputs.visidata.lastModifiedDate} @ ${inputs.visidata.shortRev}]"
          super.visidata.overridePythonAttrs
          (old: {
            propagatedBuildInputs = old.propagatedBuildInputs ++ pypkgs;
            src = inputs.visidata;
            doCheck = false;
            patches = [ ];
          });
    };

  xclipOverlay = self: super: {
    xclip =
      builtins.trace "INFO: Using xclip overlay for newer version" super.xclip.overrideAttrs
        (old: {
          version = inputs.xclip.shortRev;
          src = inputs.xclip;
        });
  };

  overlays = [
    wallpapersOverlay
    visidataOverlay
    xclipOverlay
  ];
}
