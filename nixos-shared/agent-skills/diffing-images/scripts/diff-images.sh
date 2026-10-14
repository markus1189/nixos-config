#!/usr/bin/env nix
#! nix shell nixpkgs#bash nixpkgs#coreutils nixpkgs#gawk nixpkgs#imagemagick nixpkgs#odiff --command bash
# Where do two pixel-aligned images differ? odiff finds the changed pixels;
# each cluster of them becomes one A|B|diff strip, small enough to Read at
# native resolution.
set -euo pipefail

usage() {
  cat <<'EOF'
Usage: diff-images.sh [options] A B

Compares two pixel-aligned images with odiff and writes one strip per changed
region: A, B and diff (changed pixels red over A), side by side, or stacked
for wide regions.

Options:
  -t, --threshold T  colour tolerance 0..1. Default 0 (exact); JPEG input
                     defaults to 0.1, which absorbs compression noise and
                     uniform grey steps up to 26/255 alike
      --aa           ignore anti-aliasing, for renders from different
                     engines, fonts or machines; also shrinks real edits
  -i, --ignore R     ignore region x1:y1-x2:y2 in image pixels, repeatable;
                     fractional or swapped corners are fine
  -o, --out DIR      directory for the crops (default: a new temp dir)
      --max-crops N  crop only the N largest regions (default 8)
  -h, --help         show this help

First output line:
  identical (threshold T)
  differ: N px (P%) in K regions, threshold T; crops show A, B, diff ...
  size differs: A WxH, B WxH; compared by position, the first R rows match ...
After "differ", one line per region, largest first:
  region I: WxH+X+Y crop PATH (SCALE, e.g. 4x) colour #RRGGBB -> #RRGGBB (N px)
where colour is the most frequent colour under the region's changed pixels
in A and in B: for a recolour, the old and the new value.
Exit: 0 identical, 1 different, 2 error.
EOF
}

die() { echo "diff-images: $*" >&2; exit 2; }

# Each 8x8 block holding a changed pixel becomes one pixel of a small image;
# dilating that by one block puts changes up to 24 px apart into one region
# (32 px or more never) and leaves an 8-15 px margin around it. Clustering at
# full resolution instead took 17 s on a 37-megapixel screenshot; this, 1.3 s.
readonly BLOCK=8
# Read shows images at most 2000 px on the long side, and small ones at their
# own size, where a changed pixel is a speck: strips are shrunk to fit, or
# magnified by a whole factor (pixel-exact) towards 512 px, at most 8x.
readonly MAX_STRIP=2000 MAGNIFY_TO=512 MAX_MAGNIFY=8
# Region lines beyond this flood the context without helping.
readonly MAX_LISTED=30

threshold="" out="" max_crops=8 aa=() ignores=() files=()
while (( $# )); do
  case "$1" in
    -t|--threshold) (( $# >= 2 )) || die "$1 needs a value"; threshold=$2; shift 2 ;;
    --threshold=*) threshold=${1#*=}; shift ;;
    --aa) aa=(--aa); shift ;;
    -i|--ignore) (( $# >= 2 )) || die "$1 needs a value"; ignores+=("$2"); shift 2 ;;
    -o|--out) (( $# >= 2 )) || die "$1 needs a value"; out=$2; shift 2 ;;
    --max-crops) (( $# >= 2 )) || die "$1 needs a value"; max_crops=$2; shift 2 ;;
    -h|--help) usage; exit 0 ;;
    -*) die "unknown option $1 (see --help)" ;;
    *) files+=("$1"); shift ;;
  esac
done
(( ${#files[@]} == 2 )) || { usage >&2; exit 2; }
A=${files[0]} B=${files[1]}
[[ -f $A ]] || die "no such file: $A"
[[ -f $B ]] || die "no such file: $B"
[[ $max_crops =~ ^[0-9]+$ ]] || die "--max-crops needs a whole number"

note=""
if [[ -z $threshold ]]; then
  threshold=0
  shopt -s nocasematch
  if [[ $A == *.jp*g || $B == *.jp*g ]]; then
    threshold=0.1 note=" (JPEG input; -t 0 for exact)"
  fi
  shopt -u nocasematch
fi
awk -v t="$threshold" 'BEGIN { exit !(t ~ /^([0-9]*\.)?[0-9]+$/ && t <= 1) }' ||
  die "--threshold must be between 0 and 1"

if [[ -n $out ]]; then
  mkdir -p "$out" || die "cannot create $out"
  rm -f "$out"/crop-*.png "$out"/hist-*.txt "$out"/mask.png "$out"/a.png "$out"/b.png
else
  out=$(mktemp -d -t claude-code.diff-images.XXXXXX)
fi

# odiff picks its decoder by lowercase extension, rejects .PNG, .tif and .gif,
# and swaps red and blue in BMPs (4.5.0). Everything else goes through magick,
# first frame only.
readable() {
  case "$1" in
    *.png | *.jpg | *.jpeg | *.webp | *.tiff) printf '%s\n' "$1" ;;
    *) magick "${1}[0]" "$2" && printf '%s\n' "$2" ;;
  esac
}
a=$(readable "$A" "$out/a.png") || die "cannot read $A"
b=$(readable "$B" "$out/b.png") || die "cannot read $B"
read -r wa ha < <(magick identify -ping -format '%w %h\n' "${a}[0]") || die "cannot read $A"
read -r wb hb < <(magick identify -ping -format '%w %h\n' "${b}[0]") || die "cannot read $B"

opts=(-t "$threshold" "${aa[@]}")
if (( ${#ignores[@]} )); then
  # odiff 4.5.0 takes integer corners only, panics on swapped ones, and lets a
  # region wider than the image spill into the next row: round outward, order
  # and clamp here.
  regions=$(printf '%s\n' "${ignores[@]}" | awk -F'[:-]' -v w="$wa" -v h="$ha" '
    NF != 4 || $0 !~ /^[0-9.]+:[0-9.]+-[0-9.]+:[0-9.]+$/ { print "bad --ignore " $0 > "/dev/stderr"; bad = 1; exit 1 }
    {
      lx = $1 < $3 ? $1 : $3; hx = $1 < $3 ? $3 : $1
      ly = $2 < $4 ? $2 : $4; hy = $2 < $4 ? $4 : $2
      lx = int(lx); ly = int(ly)
      hx = hx == int(hx) ? hx : int(hx) + 1; hy = hy == int(hy) ? hy : int(hy) + 1
      if (lx > w - 1 || ly > h - 1) next
      if (hx > w - 1) hx = w - 1
      if (hy > h - 1) hy = h - 1
      list = list (list == "" ? "" : ",") lx ":" ly "-" hx ":" hy
    }
    END { if (!bad && list != "") print list }') || die "regions must look like x1:y1-x2:y2"
  [[ -z $regions ]] || opts+=(-i "$regions")
fi

if [[ "$wa $ha" != "$wb $hb" ]]; then
  res=$(odiff "$a" "$b" --parsable-stdout --output-diff-lines "${opts[@]}") && rc=0 || rc=$?
  (( rc == 0 || rc == 22 )) || die "odiff failed (exit $rc)"
  same=$(awk -F';' -v m=$(( ha < hb ? ha : hb )) '{ n = split($3, r, ","); print n ? r[1] : m }' <<<"$res")
  echo "size differs: A ${wa}x${ha}, B ${wb}x${hb}; compared by position, the first $same rows match." \
    "Realign (cut the inserted band) or compare one region cut from both"
  exit 1
fi

res=$(odiff "$a" "$b" "$out/mask.png" --diff-mask --parsable-stdout "${opts[@]}") && rc=0 || rc=$?
case $rc in
  0) echo "identical (threshold $threshold)$note"; exit 0 ;;
  22) ;;
  *) die "odiff failed (exit $rc)" ;;
esac
count=${res%%;*}

pw=$(( (wa + BLOCK - 1) / BLOCK * BLOCK )) ph=$(( (ha + BLOCK - 1) / BLOCK * BLOCK ))
mapfile -t boxes < <(
  magick "$out/mask.png" -background none -extent "${pw}x${ph}" -alpha extract -threshold 0 \
    -scale "$(( pw / BLOCK ))x$(( ph / BLOCK ))!" -threshold 0 -morphology Dilate Square:1 \
    -define connected-components:verbose=true -connected-components 8 null: |
    awk -v b="$BLOCK" -v w="$wa" -v h="$ha" '
      $NF ~ /255/ {
        split($2, g, /[x+]/)
        x = g[3] * b; y = g[4] * b; cw = g[1] * b; ch = g[2] * b
        if (x + cw > w) cw = w - x
        if (y + ch > h) ch = h - y
        print $4, cw "x" ch "+" x "+" y
      }' | sort -k1,1nr | cut -d' ' -f2)
n=${#boxes[@]}
(( n > 0 )) || die "odiff reported $count changed pixels but the mask is empty"

awk -v c="$count" -v px=$(( wa * ha )) -v n="$n" -v t="$threshold" -v note="$note" 'BEGIN {
  printf "differ: %d px (%.4f%%) in %d region%s, threshold %s%s; crops show A, B, diff (changed pixels red)\n",
    c, 100 * c / px, n, n == 1 ? "" : "s", t, note }'

# One magick run: the three full-size images are decoded once, not per region.
args=("$a" -write mpr:A +delete "$b" -write mpr:B +delete "$out/mask.png" -write mpr:M +delete)
lines=()
for (( i = 0; i < n && i < MAX_LISTED; i++ )); do
  g=${boxes[i]}
  line="region $(( i + 1 )): $g"
  if (( i < max_crops )); then
    w=${g%%x*} h=${g#*x}; h=${h%%+*}
    if (( w > h )); then join=-append sw=$(( w + 4 )) sh=$(( 3 * (h + 4) ))
    else join=+append sw=$(( 3 * (w + 4) )) sh=$(( h + 4 )); fi
    crop="$out/crop-$(( i + 1 )).png"
    long=$(( sw > sh ? sw : sh ))
    zoom=$(( MAGNIFY_TO / long ))
    (( zoom <= MAX_MAGNIFY )) || zoom=$MAX_MAGNIFY
    if (( long > MAX_STRIP )); then
      fit=(-resize "${MAX_STRIP}x${MAX_STRIP}")
      shown=$(awk -v l="$long" -v m="$MAX_STRIP" 'BEGIN { printf "%.2fx", m / l }')
    elif (( zoom >= 2 )); then
      fit=(-scale "$(( zoom * 100 ))%") shown="${zoom}x"
    else
      fit=() shown="1x"
    fi
    args+=('(' mpr:A -crop "$g" +repage ')' '(' mpr:B -crop "$g" +repage ')'
           '(' '(' mpr:A -crop "$g" +repage ')' '(' mpr:M -crop "$g" +repage ')' -composite ')'
           -bordercolor '#888' -border 2 "$join" "${fit[@]}" -write "$crop" +delete)
    for s in A B; do
      args+=('(' '(' "mpr:$s" -crop "$g" +repage ')' '(' mpr:M -crop "$g" +repage -alpha extract ')'
             -alpha off -compose CopyOpacity -composite -compose Over -depth 8 -format %c
             -write "histogram:info:$out/hist-$s-$(( i + 1 )).txt" ')' +delete)
    done
    line+=" crop $crop ($shown)"
  fi
  lines+=("$line")
done
if (( max_crops > 0 )); then
  magick "${args[@]}" xc:none null: || die "cropping failed"
fi
# The most frequent colour under a region's changed pixels, in A and in B: for
# a recolour that is the old and the new value (#333333 -> #3A3A3A).
top() { awk '$2 ~ /,255\)$/ && $1 + 0 > best { best = $1 + 0; hex = substr($3, 1, 7) }
             END { if (best) print hex, best }' "$1"; }
for (( i = 0; i < n && i < max_crops && i < MAX_LISTED; i++ )); do
  read -r ca na < <(top "$out/hist-A-$(( i + 1 )).txt") || continue
  read -r cb _ < <(top "$out/hist-B-$(( i + 1 )).txt") || continue
  lines[i]+=" colour $ca -> $cb ($na px)"
done
printf '%s\n' "${lines[@]}"
(( n <= MAX_LISTED )) || echo "... $(( n - MAX_LISTED )) more regions"
exit 1
