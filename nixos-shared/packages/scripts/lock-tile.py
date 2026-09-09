"""Generate the seamlessly tiling isometric-cube background for lockScreen.

i3lock's --tiling draws the image at native size and repeats it, so one small
tile covers any root window: the 3840x1600 dual-monitor desktop, the 1920x1080
internal panel, and anything else, with no per-host asset and no scaling.

Why a tile at all: --blur=15 recomputes a screenshot blur on every lock, which
measured 1.79s on a 3840x1600 root (blur.c repeats a 7-tap kernel (sigma/2)^2
times, so cost is quadratic). Tiling a pregenerated image measured 0.06s -- the
same cost as a solid --color -- and reveals nothing of the screen at all.

Exact periodicity is the whole trick. Picking a hexagon radius and deriving the
lattice pitch as sqrt(3)*R rounds to an integer and drifts a third of a pixel
per column, which accumulates into a visible break at the tile edge. So the
integer tile size is chosen FIRST and the lattice derived to divide it exactly:
WIDTH/NCOLS and HEIGHT/NROWS are the pitches by construction. The hexagons come
out 0.05% off regular, which is invisible; the drift would not have been.

Colours are the wallpaper's own palette (magick -colors 6 on
assets/wallpapers/orange-cube-6x5-left.png).
"""

import random
import sys

from PIL import Image, ImageDraw

# 12x16 cubes rather than the smallest possible cell: a bigger period means
# the accent cubes repeat 4 times across the desktop instead of 8, which is
# the difference between a texture and a visible grid.
WIDTH, HEIGHT = 960, 1108
NCOLS, NROWS = 12, 16

BACKGROUND = "#15171A"
NORMAL_FACES = ("#4C535A", "#3D3F45", "#15171A")  # top, left, right
ACCENT_FACES = ("#DE5905", "#8A3703", "#2A1712")
ACCENT_CHANCE = 0.035
SEED = 1189

PITCH_X = WIDTH / NCOLS
ROW_Y = HEIGHT / NROWS
HALF_W = PITCH_X / 2
RADIUS = ROW_Y / 1.5  # so that 1.5 * RADIUS == ROW_Y exactly


def draw_cube(draw, cx, cy, top, left, right):
    """One hexagon split into three rhombi, read as a cube lit from above."""
    v = [
        (cx, cy - RADIUS),
        (cx + HALF_W, cy - RADIUS / 2),
        (cx + HALF_W, cy + RADIUS / 2),
        (cx, cy + RADIUS),
        (cx - HALF_W, cy + RADIUS / 2),
        (cx - HALF_W, cy - RADIUS / 2),
    ]
    centre = (cx, cy)
    draw.polygon([v[5], v[0], v[1], centre], fill=top)
    draw.polygon([v[1], v[2], v[3], centre], fill=right)
    draw.polygon([v[3], v[4], v[5], centre], fill=left)


def main():
    if len(sys.argv) != 2:
        sys.exit("usage: lock-tile.py OUTPUT.png")

    image = Image.new("RGB", (WIDTH, HEIGHT), BACKGROUND)
    draw = ImageDraw.Draw(image)
    rng = random.Random(SEED)

    for row in range(NROWS):
        for col in range(NCOLS):
            cx = col * PITCH_X + (row % 2) * HALF_W
            cy = row * ROW_Y
            faces = ACCENT_FACES if rng.random() < ACCENT_CHANCE else NORMAL_FACES
            # Every cube is drawn at all nine wrapped offsets, so a shape
            # crossing an edge reappears on the opposite one and the tile is
            # periodic rather than merely close to it.
            for dx in (-WIDTH, 0, WIDTH):
                for dy in (-HEIGHT, 0, HEIGHT):
                    draw_cube(draw, cx + dx, cy + dy, *faces)

    image.save(sys.argv[1], optimize=True)


if __name__ == "__main__":
    main()
