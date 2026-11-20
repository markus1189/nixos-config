# Hourly by the friction-digest user timer (../../home-manager/stuff); the test
# suite runs in checkPhase, so a parser regression fails the build, not the hourly run.
{ haskell, haskellPackages }:
haskell.lib.justStaticExecutables (
  haskellPackages.callPackage (
    {
      mkDerivation,
      lib,
      aeson,
      bytestring,
      containers,
      directory,
      filepath,
      hedgehog,
      hostname,
      lens,
      lens-aeson,
      pandoc,
      tasty,
      tasty-golden,
      tasty-hedgehog,
      tasty-hunit,
      text,
      time,
    }:
    mkDerivation {
      pname = "friction-digest";
      version = "0.1.0";
      src = lib.cleanSource ./.;
      isLibrary = true;
      isExecutable = true;
      libraryHaskellDepends = [
        aeson
        bytestring
        containers
        directory
        filepath
        lens
        lens-aeson
        pandoc
        text
        time
      ];
      executableHaskellDepends = [
        hostname
        lens-aeson
      ];
      testHaskellDepends = [
        hedgehog
        tasty
        tasty-golden
        tasty-hedgehog
        tasty-hunit
      ];
      license = lib.licenses.mit;
      mainProgram = "friction-digest";
    }
  ) { }
)
