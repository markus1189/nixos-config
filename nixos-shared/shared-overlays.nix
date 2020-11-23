rec {
  stableNixpkgsOverlay = self: super: {
    stableNixpkgs = import self.ndtSources.nixpkgs-2003 { };
  };

  nixpkgs1703Overlay = self: super: {
    nixpkgs1703 = import self.ndtSources.nixpkgs-1703 { };
  };

  ndtOverlay = self: super: {
    ndt = import (builtins.fetchTarball
      "https://github.com/markus1189/ndt/archive/master.tar.gz") {
        nixpkgs = self;
      };
  };

  ndtSourcesOverlay = self: super: {
    ndtSources = import ../ndt/sources.nix { };
  };

  dunstWithDunstify = self: super: {
    dunst = super.dunst.override { dunstify = true; };
  };

  wallpapersOverlay = self: super: {
    markus-wallpapers = rec {
      cc = super.fetchurl {
        url = "https://www.dropbox.com/s/hoy6xdgnudwy64g/codecentric.jpg?dl=1";
        sha256 = "0z8ia5gv1r3brr7a2cc3sv0ffi0vqdmwj85b404702v7sb7f2d1k";
      };

      shrike-rape-10x8 = super.fetchurl {
        url =
          "https://lh3.googleusercontent.com/otenqGE9piNoIxqFESloBFaLYROVugEwhUEVKgTDAoBzNX-ximHZuuclY3_85Yhl1VMdr2oItMis8Qgo6F4LKWHmRt3VhxUWeut3E-XS63Z_twtsMIrqXk1VxA9nuyBsZ7mj-lcfMuJn5hYFGYWsHW4-x3qxXArG?authuser=0";
        sha256 = "1xm3c46zx1n5y5rkfbclrpj77x2qzdc0jj5sbbgnlkm5nm5f7y42";
      };

      shrike-rape-10x8-flipped =
        super.runCommandNoCCLocal "shrike-rape-10x8-flipped" { } ''
          ${super.imagemagick}/bin/convert -flop ${shrike-rape-10x8} $out
        '';

      shrike-rape-21x9 = super.fetchurl {
        url =
          "https://lh3.googleusercontent.com/e256LaGHjyBewkL4lKov5FGUT3wgdumtM7n0RSp_2yPc7MlP8iNV19Wcuv1ZThYjV_39u5Cfpi2RFaYGLQ7ODMYH4BLjZwiCiNeb6m1vuCs4sIbgdoqeXl1WZEfDPMq_OsMSLC779E_whTURrTXl_vB3ofPupDYT?authuser=0";
        name = "shrike-rape-21x9.jpg";
        sha256 = "0560f6bl7lzfi39xz63iads88cavpmxpgyidyxnljhz2r937k9fs";
      };
    };
  };

  overlays = [
    ndtOverlay
    ndtSourcesOverlay
    stableNixpkgsOverlay
    nixpkgs1703Overlay
    dunstWithDunstify
    wallpapersOverlay
  ];
}
