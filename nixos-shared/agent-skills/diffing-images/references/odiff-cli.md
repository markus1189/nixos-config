# odiff CLI (4.5.0)

For direct calls when the scripts don't fit. Not on PATH:
`nix shell nixpkgs#odiff --command odiff …`. Upstream:
https://github.com/dmtrKovalenko/odiff

```bash
odiff A.png B.png diff.png -t 0 --parsable-stdout --output-diff-lines --output-diff-cols
```

## Output

- `--parsable-stdout`: stdout is `count;percent[;rows][;cols]`, `0` when
  identical, `layout` for a size mismatch under `--fail-on-layout`. Without it
  the verdict is prose on stderr.
- `percent` has two decimals, so small changes print `0.00`: use the count.
- `--output-diff-lines` / `--output-diff-cols` list every changed row / column
  index, thousands of them on big changes: reduce to min/max before printing.
- Exit 0 identical, 22 pixels differ, 21 sizes differ (only with
  `--fail-on-layout`; without it, sizes are compared by position and the extra
  area counts as changed), 1 error, 134 panic.
- The diff image is A with changed pixels red; `--diff-mask` keeps only the
  changed pixels, over transparency.

## Traps

- On a match no diff image is written, and one from an earlier run survives.
  Delete it before each run.
- The decoder follows the lowercase extension: png, jpg/jpeg, webp, tiff, bmp.
  `.PNG`, `.tif`, `.gif` and extensionless files fail with "Unsupported image
  format". The diff output must be `.png`; anything else exits 1 and leaves a
  0-byte file.
- BMPs load with red and blue swapped: a red BMP matches a blue PNG. Convert
  BMPs with magick first.
- `--diff-overlay` followed by the output path swallows the path: exit 22, no
  file. Give the factor (`--diff-overlay 0.5`) or put the flag after all paths.
- `-i x1:y1-x2:y2[,…]` takes integers, both corners inclusive. An `x2` past the
  right edge spills into the next row; swapped corners crash with
  `panic: integer overflow`.
- Transparent pixels are composited onto white: a fully transparent image
  matches opaque white even at `-t 0`.
- A pixel counts when its YIQ distance exceeds 35215·t²; for a uniform grey step
  d that means d > 264·t. JPEG decodes exactly only at `-t 0`.
- `--reduce-ram-usage` does nothing (parsed, never read); `--enable-asm` needs
  AVX-512.

## Server mode

`odiff --server` prints `{"ready":true}`, then answers one JSON request per
stdin line, failures included, without exiting:

```
{"requestId":1,"base":"a.png","compare":"b.png","output":"d.png","options":{"threshold":0}}
{"requestId":1,"match":false,"reason":"pixel-diff","diffCount":100,"diffPercentage":0.02}
```

A CLI call costs about 8 ms on small images, so this pays off only for
thousands of pairs.
