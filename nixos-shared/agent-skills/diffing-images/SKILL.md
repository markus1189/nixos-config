---
name: diffing-images
description: "Finds where two images, screenshots or PDF versions differ, pixel-exactly, and crops each changed region into a small A/B/diff strip to look at, using odiff. Use when asked what changed between two images, screenshots, renders, charts or versions of a PDF; for visual regression checks before and after a CSS, theme, layout or code change, including verifying one's own UI edits; to prove that a refactor, regeneration, re-export or image optimization changed nothing visible; when odiff, pixel diff, image diff, screenshot diff or spot the difference come up; and before claiming two images or pages look the same. Also for PDF versions whose text diff cannot show ticked boxes, stamps, signatures or layout. Not for inputs that are not pixel-aligned: two scans, photos taken separately, or PDFs exported by different programs."
---

# Diffing images

Small changes are invisible in the images themselves: Read shows a 1280×2807
screenshot at 912×2000, and a heading recoloured from `#333333` to `#3a3a3a`
looks identical even at full size. Diff first, then Read only the crops: their
third panel paints every changed pixel red.

## Scripts

```bash
./scripts/diff-images.sh [options] A B             # two images, any format magick reads
./scripts/diff-pdfs.sh [--dpi N] [options] A.pdf B.pdf
```

```
differ: 1023 px (0.0285%) in 1 region, threshold 0; crops show A, B, diff (changed pixels red)
region 1: 128x40+32+248 crop /tmp/claude-code.diff-images.yNOze0/crop-1.png (3x) colour #333333 -> #3A3A3A (522 px)
```

- The other first lines are `identical (threshold T)` and `size differs: …, the
  first R rows match` (no crops then).
- Regions are `WxH+X+Y`, largest first; changes up to 24 px apart share one.
  Coordinates are pixels of the input, for PDFs of the render (× 72/DPI gives
  PDF points). The factor in parentheses is how far the strip was magnified
  (small regions) or shrunk to Read's 2000 px limit. Read every listed crop
  before describing a change.
- `colour` is the most frequent colour under the region's changed pixels, in A
  and in B: for a recolour, the old and the new value.
- `diff-pdfs.sh` prints the same per page (`p2 differ: …`, region lines
  indented), plus `pN only in A` / `pN only in B` and a `page count differs`
  line.
- Crops land in a new directory under `$TMPDIR`. Exit 0 identical, 1 different,
  2 error. Both scripts take `--help`.
- Count pixels with these scripts, not `magick compare -metric AE`: in
  ImageMagick 7.1.2 that metric is no pixel count (50 for a 10×10 change).

## Threshold

- Exact (`-t 0`) is the default: screenshots, PDF renders and plots from one
  pipeline are deterministic, so every changed pixel is real.
- JPEG input switches to `-t 0.1`, which absorbs compression noise; pass it for
  other lossy sources (AI image edits, lossy WebP) too. At 0.1, uniform grey
  steps up to 26/255 (`#333` vs `#444` text) count as identical, so 0.1 never
  proves "unchanged".
- `--aa` for renders from different engines, fonts or machines: it removed 73%
  of edge noise in a test, and two thirds of a real one-glyph edit.

## Alignment

Pixels are compared by position. Content inserted above a change shifts
everything below it, and the script reports how many top rows match instead of
cropping. Realign and compare again: cut the inserted band
(`magick b.png -crop +0+40 +repage b2.png`), or cut the same element from both
images. Nothing rescues two scans, separately taken photos, or a PDF
re-exported by another program (compare `pdfinfo` Creator and Producer): font
changes left 3,600–10,000 noisy pixels per page against 12–81 px of real edits,
with or without `--aa`.

## Web pages (agent-browser)

- Take both shots with the same `set viewport W H [scale]`, the same scrollbar
  setting, and animations frozen:
  `agent-browser eval "(()=>{const s=document.createElement('style');s.textContent='*,*::before,*::after{animation:none!important;transition:none!important;caret-color:transparent!important}';document.head.appendChild(s)})()"`
- Run `agent-browser get url` before the baseline shot: right after a close and
  reopen, `screenshot` can capture `about:blank` and still report success.
- An element screenshot that crosses the viewport edge comes back with a blank
  band. Cut the element out of full-page shots instead, at its `get box`
  coordinates plus the scroll offset, times the scale.
- Mask dynamic content (clocks, counters) with `-i x1:y1-x2:y2`; fractional or
  swapped corners from `get box` are fine.
- `agent-browser diff screenshot` exits 0 even when the shots differ.

## Calling odiff directly

Only when the scripts don't fit (bulk loops, custom output). It is not on PATH
(`nix shell nixpkgs#odiff`), and its CLI has traps: stale diff files,
case-sensitive extensions, a BMP colour swap, an option that swallows the
output path. Read [references/odiff-cli.md](references/odiff-cli.md) first.

**Script Execution:** Always invoke scripts by absolute path: resolve
`./scripts/` against this SKILL.md's directory. All scripts use Nix shebangs,
so no dependency installation is needed.
