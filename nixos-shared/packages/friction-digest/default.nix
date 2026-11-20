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
      deepseq,
      directory,
      filepath,
      foldl,
      hedgehog,
      hostname,
      lens,
      lens-aeson,
      pandoc,
      pandoc-types,
      streaming,
      streaming-bytestring,
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
        deepseq
        directory
        filepath
        foldl
        lens
        lens-aeson
        pandoc
        pandoc-types
        streaming
        streaming-bytestring
        text
        time
      ];
      executableHaskellDepends = [ hostname ];
      testHaskellDepends = [
        hedgehog
        streaming
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
