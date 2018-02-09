# adapted from https://github.com/grahamc/nixos-config/
{ stdenvNoCC }:
file: args:
(stdenvNoCC.mkDerivation (args // rec {
  name = baseNameOf file;

  phases = [ "installPhase" "checkPhase" ];

  installPhase = ''
    cp ${file} $out
    substituteAllInPlace $out
  '';

  doCheck = true;

  checkPhase = ''
    if grep -q '@[[:alnum:]]\+@' $out; then
      echo Found non interpolated pattern in $out, failing.  Matches are:
      grep -Hn '@[[:alpha:][:digit:]]\+@' $out
      echo "####################"
      return 1
    fi
  '';
}))
