# Vendored after nixpkgs removed fasd (upstream clvv/fasd was archived).
# src is the whjvenyl fork (inputs.fasd), 62+ commits of fixes past the
# last nixpkgs pin; drop the input and this file if nixpkgs ever readds it.
{
  lib,
  stdenv,
  src,
}:

stdenv.mkDerivation {
  pname = "fasd";
  version = "1.0.2-${src.shortRev}";

  inherit src;

  installPhase = ''
    PREFIX=$out make install
  '';

  meta = {
    homepage = "https://github.com/whjvenyl/fasd";
    description = "Quick command-line access to files and directories for POSIX shells";
    license = lib.licenses.mit;
    platforms = lib.platforms.all;
    mainProgram = "fasd";
  };
}
