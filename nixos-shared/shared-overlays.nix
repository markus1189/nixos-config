rec {
  rxvtOverlay = self: super: {
    rxvt-unicode = builtins.trace "INFO: Using patched urxvt"
      super.rxvt-unicode.overrideAttrs (old: {
        patches = [
          (self.writeText "urxvt.patch" ''
            OSC commands returning the colour sequence must be terminated by either
            of ST, ESC \, or BEL. rxvt-unicode tries to use the same type of termination
            as was in the query but doesn't correctly handle the multiple-char sequence
            used for 7-bit queries. Force to using ESC \ instead for now.

            Index: src/command.C
            --- src/command.C.orig
            +++ src/command.C
            @@ -3426,9 +3426,9 @@ rxvt_term::process_color_seq (int report, int color, c
                     snprintf (rgba_str, sizeof (rgba_str), "rgb:%04x/%04x/%04x", c.r, c.g, c.b);

                   if (IN_RANGE_INC (color, minCOLOR, maxTermCOLOR))
            -        tt_printf ("\033]%d;%d;%s%c", report, color - minCOLOR, rgba_str, resp);
            +        tt_printf ("\033]%d;%d;%s\033\\", report, color - minCOLOR, rgba_str);
                   else
            -        tt_printf ("\033]%d;%s%c", report, rgba_str, resp);
            +        tt_printf ("\033]%d;%s\033\\", report, rgba_str, resp);
                 }
               else
                 set_window_color (color, str);
          '')
        ];
      });

  };
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
      pypkgs = with self.python3Packages; [
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
    rxvtOverlay
  ];
}
