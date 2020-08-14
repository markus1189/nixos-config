rec {
  stableNixpkgsOverlay = self: super: {
    stableNixpkgs = import self.ndtSources.nixpkgs-2003 { };
  };

  nixpkgs1703Overlay = self: super: {
    nixpkgs1703 = import self.ndtSources.nixpkgs-1703 { };
  };

  bukuOverlay = self: super: {
    buku =
      builtins.trace "INFO: using pinned buku version" self.stableNixpkgs.buku;
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

  temporaryDdgrFix = self: super: {
    ddgr = builtins.trace
      "INFO: using overlay for ddgr until https://github.com/NixOS/nixpkgs/pull/93928"
      (self.callPackage (builtins.fetchurl
        "https://raw.githubusercontent.com/markus1189/nixpkgs/ddgr-1-9/pkgs/applications/misc/ddgr/default.nix")
        { });
  };

  wallpapersOverlay = self: super: {
    markus-wallpapers = rec {
      cc = super.fetchurl {
        url = "https://www.dropbox.com/s/hoy6xdgnudwy64g/codecentric.jpg?dl=1";
        sha256 = "0z8ia5gv1r3brr7a2cc3sv0ffi0vqdmwj85b404702v7sb7f2d1k";
      };

      shrike-rape-10x8 = super.fetchurl {
        url = "https://lh3.googleusercontent.com/otenqGE9piNoIxqFESloBFaLYROVugEwhUEVKgTDAoBzNX-ximHZuuclY3_85Yhl1VMdr2oItMis8Qgo6F4LKWHmRt3VhxUWeut3E-XS63Z_twtsMIrqXk1VxA9nuyBsZ7mj-lcfMuJn5hYFGYWsHW4-x3qxXArG?authuser=0";
        sha256 = "1xm3c46zx1n5y5rkfbclrpj77x2qzdc0jj5sbbgnlkm5nm5f7y42";
      };

      shrike-rape-10x8-flipped =
        super.runCommandNoCCLocal "shrike-rape-10x8-flipped" { } ''
          ${super.imagemagick}/bin/convert -flop ${shrike-rape-10x8} $out
        '';

      shrike-rape-21x9 = super.fetchurl {
        url = "https://lh3.googleusercontent.com/yHPqcWQ2x51R6jQQIpMEsgFy06-TP741Yn93OPT908G2OZ2IoUOuejEovnnN4Rq5hjrjdH28oU2aSWVhZ5pGn40fFWDxQxB4bKzYLWX5GIf-hCAPqlqExKz0dx7yRHtADmCEBI7EQq_YzCFGQqVZMv-U0BV0IYQ7rLIXjQVJnjdDR5mmFfxRwUkSyzepJb658b8h46S4uMjuK_5xYxgYDHeSUPpTbSmtuG4XMaAHxPP-40XcDJGLlcUS9YfPtrVRULQXviYwEmRDU7vwxEigkOjb7I5Bteg1wub-jIuhMe4y3xu246YRoDMTGw7ilcN_QS40sP7QFh1TS5AlGvpCNlev2ZgQobFuSumtmT18ZCK5fS8p-Jc-8ZPtEk3BqERyoSWMiU0UrLjhEglEtOs_rUzfw-2rpBQi5TRGuE_bbKR-FgQ0e5F8Tj0z9RYajHbWPQpiYf19KXKuLya3YyzKPTK1VpMI3jBajxJ9sdbez1tn6oDy6opW5-DU2dYB0E5iXG9a_q4JWftK5yp52v0m7V6xZHha8r-poZ_2JgipeXMSFVrbGHbuJ_KrRg7l_DjCFiO5NiKoPl2rl1q2AkePKZdb7x13WG1rMPFjXLYFA9tbmvGALm3_1vxqO9GSpHM59zS6x_xV-1W0L9cqBomEZ89dgHoV4p6G4ZUWfEROWTTlJi9PxE3SeeANfHMP_pqrqDefBkUDnKzM8klnHkmb?authuser=0";
        name = "shrike-rape-21x9.jpg";
        sha256 = "05lscmldamrkgq0cjklwfnw5rdgx5343k2zd889nb0ihw90lm6dq";
      };
    };
  };

  overlays = [
    bukuOverlay
    ndtOverlay
    ndtSourcesOverlay
    stableNixpkgsOverlay
    nixpkgs1703Overlay
    dunstWithDunstify
    temporaryDdgrFix
    wallpapersOverlay
  ];
}
