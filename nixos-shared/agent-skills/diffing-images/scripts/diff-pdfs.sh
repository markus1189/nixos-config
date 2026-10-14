#!/usr/bin/env nix
#! nix shell nixpkgs#bash nixpkgs#coreutils nixpkgs#gawk nixpkgs#imagemagick nixpkgs#odiff nixpkgs#poppler-utils --command bash
# Page-by-page visual diff of two versions of a PDF: renders both, then runs
# diff-images.sh on every page pair.
set -euo pipefail
readonly SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

usage() {
  cat <<'EOF'
Usage: diff-pdfs.sh [--dpi N] [diff-images options] A.pdf B.pdf

Renders every page of both PDFs and compares each page pair with
diff-images.sh. Catches what text extraction misses: ticked boxes, stamps,
signatures, layout. Both files must come from the same generator; a re-export
by another program changes glyph edges on every page.

Options:
  -r, --dpi N        render resolution (default 150: ticks and digits stay
                     legible, an A4 page renders at 1241x1754)
  -t, --threshold T, --aa, -i/--ignore R, --max-crops N
                     passed to diff-images.sh for every page
  -h, --help         show this help

Output: "page count differs: A=N B=M" if they do, then one block per page:
  pN identical (threshold T)
  pN differ: ...   followed by indented "region ..." lines with crop paths
  pN size differs: ...
  pN only in A | pN only in B
Exit: 0 all pages identical, 1 any difference, 2 error.
EOF
}

die() { echo "diff-pdfs: $*" >&2; exit 2; }

dpi=150 pass=() files=()
while (( $# )); do
  case "$1" in
    -r|--dpi) (( $# >= 2 )) || die "$1 needs a value"; dpi=$2; shift 2 ;;
    -t|--threshold|-i|--ignore|--max-crops)
      (( $# >= 2 )) || die "$1 needs a value"; pass+=("$1" "$2"); shift 2 ;;
    --aa|--threshold=*) pass+=("$1"); shift ;;
    -h|--help) usage; exit 0 ;;
    -*) die "unknown option $1 (see --help)" ;;
    *) files+=("$1"); shift ;;
  esac
done
(( ${#files[@]} == 2 )) || { usage >&2; exit 2; }
[[ $dpi =~ ^[0-9]+$ ]] && (( dpi > 0 )) || die "--dpi needs a positive whole number"
A=${files[0]} B=${files[1]}
[[ -f $A ]] || die "no such file: $A"
[[ -f $B ]] || die "no such file: $B"

# An unreadable file must reach the check below, not trip set -e as exit 1.
pages() { pdfinfo "$1" 2>/dev/null | awk '/^Pages:/ { print $2 }' || true; }
na=$(pages "$A") nb=$(pages "$B")
[[ -n $na ]] || die "pdfinfo cannot read $A"
[[ -n $nb ]] || die "pdfinfo cannot read $B"

out=$(mktemp -d -t claude-code.diff-pdfs.XXXXXX)
pdftoppm -png -r "$dpi" "$A" "$out/a" || die "pdftoppm failed on $A"
pdftoppm -png -r "$dpi" "$B" "$out/b" || die "pdftoppm failed on $B"
# pdftoppm pads page numbers to the width of the page count (a-01.png in a
# 12-page file, b-1.png in a 9-page one): match pages by number.
for f in "$out"/[ab]-*.png; do
  n=${f##*-} n=${n%.png}
  mv "$f" "${f%-*}-p$(( 10#$n )).png"
done

(( na == nb )) || echo "page count differs: A=$na B=$nb"
status=0
for (( p = 1; p <= (na > nb ? na : nb); p++ )); do
  if (( p > na )); then echo "p$p only in B"; status=1; continue; fi
  if (( p > nb )); then echo "p$p only in A"; status=1; continue; fi
  res=$(bash "$SCRIPT_DIR/diff-images.sh" -o "$out/p$p" "${pass[@]}" "$out/a-p$p.png" "$out/b-p$p.png") \
    && rc=0 || rc=$?
  (( rc <= 1 )) || die "diff-images.sh failed on page $p (exit $rc)"
  (( rc == 0 )) || status=1
  awk -v p="p$p" 'NR == 1 { print p, $0; next } { print "  " $0 }' <<<"$res"
done
exit $status
