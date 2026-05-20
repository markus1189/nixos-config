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

  # Workaround: nixpkgs HEAD pins spotify linux to a snap rev that's no longer
  # mirrored by snapcraft, and api.snapcraft.io doesn't resolve from this host.
  # Pivot to Spotify's official Debian repo (repository.spotify.com) and
  # unpack the .deb with `dpkg` instead of `unsquashfs`.
  # The eval will THROW once upstream nixpkgs moves past the broken pin — a
  # forced reminder to verify the original derivation works and delete this
  # overlay.
  # To bump (if the new upstream is *also* broken):
  #   1. curl -sL http://repository.spotify.com/pool/non-free/s/spotify-client/ \
  #        | grep -oE 'spotify-client_[^"]+_amd64\.deb' | sort -uV | tail -1
  #   2. nix store prefetch-file --hash-type sha256 \
  #        http://repository.spotify.com/pool/non-free/s/spotify-client/<filename>
  #   3. Update brokenUpstream to the new upstream (version, rev).
  spotifyOverlay = self: super:
    let
      debVersion = "1.2.86.502.g8cd7fb22";
      debHash = "sha256-brbtv0VAeNstmUWEIdfd5Vgx0ue0xjO7suD6Hx0KdDs=";
      brokenUpstream = {
        version = "1.2.84.475.ga1a748ff";
        rev = "93";
      };
      upstreamVersion = super.spotify.version or "<unknown>";
      upstreamRev = super.spotify.rev or "<unknown>";
    in {
      spotify =
        if upstreamVersion != brokenUpstream.version
           || upstreamRev != brokenUpstream.rev
        then
          throw ''
            spotifyOverlay: upstream nixpkgs has moved past the broken pin.
              upstream now: ${upstreamVersion}-rev${upstreamRev}
              overlay knew: ${brokenUpstream.version}-rev${brokenUpstream.rev}
            Verify the upstream snap URL downloads, then DELETE spotifyOverlay
            from nixos-shared/shared-overlays.nix. If the new upstream is also
            broken, bump `brokenUpstream` in the overlay to silence this.
          ''
        else
          builtins.trace
            "INFO: Using spotify overlay pinned to deb ${debVersion} from repository.spotify.com (workaround for snapcraft mirror rotation)"
            (super.spotify.overrideAttrs (finalAttrs: prev: {
              version = debVersion;
              src = super.fetchurl {
                name = "spotify-client_${debVersion}_amd64.deb";
                url = "http://repository.spotify.com/pool/non-free/s/spotify-client/spotify-client_${debVersion}_amd64.deb";
                hash = debHash;
              };
              nativeBuildInputs =
                (super.lib.remove super.squashfsTools (prev.nativeBuildInputs or []))
                ++ [ super.dpkg ];
              unpackPhase = ''
                runHook preUnpack
                mkdir squashfs-root
                dpkg-deb -x "$src" squashfs-root
                cd squashfs-root
                runHook postUnpack
              '';
            }));
    };

  overlays = [
    ndtOverlay
    ndtSourcesOverlay
    wallpapersOverlay
    visidataOverlay
    xclipOverlay
    spotifyOverlay
  ];
}
