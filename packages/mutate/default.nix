# adapted from https://github.com/grahamc/nixos-config/
{ stdenvNoCC, gnugrep }:
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
    if ${gnugrep}/bin/grep -Hn '@[[:alpha:][:digit:]-]\+@' $out; then
      echo "[ERROR] Found non interpolated pattern in $out, failing.  Matches are:"
      echo "$MATCHES"
      exit 1
    fi
  '';
}))
