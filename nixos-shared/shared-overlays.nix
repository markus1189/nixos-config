inputs:
let
  wallpapersOverlay = _: _: {
    markus-wallpapers = {
      orange-cube-left = ./assets/wallpapers/orange-cube-6x5-left.png;
      orange-cube-right = ./assets/wallpapers/orange-cube-6x5-right.png;
      orange-cube-internal = ./assets/wallpapers/orange-cube-16x9.png;
    };
  };

  visidataOverlay =
    self: super:
    let
      # everything else visidata needs is already in nixpkgs' `dependencies`
      pypkgs = with self.python3Packages; [
        pytimeparse
        tomli
      ];
    in
    {
      visidata =
        builtins.trace
          "INFO: Using visidata overlay for more python packages and develop branch [${inputs.visidata.lastModifiedDate} @ ${inputs.visidata.shortRev}]"
          super.visidata.overridePythonAttrs
          (old: {
            dependencies = old.dependencies ++ pypkgs;
            src = inputs.visidata;
            doCheck = false;
          });
    };

  xclipOverlay = _: super: {
    xclip = builtins.trace "INFO: Using xclip overlay for newer version" super.xclip.overrideAttrs (_: {
      version = inputs.xclip.shortRev;
      src = inputs.xclip;
    });
  };

  # rustc 1.99 removed the no_mangle_generic_items lint; clipcat 0.26 denies it
  # and renamed_and_removed_lints together, so the build hard-errors.
  clipcatOverlay = _: super: {
    clipcat = super.clipcat.overrideAttrs (old: {
      postPatch = (old.postPatch or "") + ''
        grep -q '^no_mangle_generic_items ' Cargo.toml \
          || { echo "clipcat no longer denies no_mangle_generic_items: drop clipcatOverlay" >&2; exit 1; }
        sed -i '/^no_mangle_generic_items /d' Cargo.toml
      '';
    });
  };
in
[
  wallpapersOverlay
  visidataOverlay
  xclipOverlay
  clipcatOverlay
]
