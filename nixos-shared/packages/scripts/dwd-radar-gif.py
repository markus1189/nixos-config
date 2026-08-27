#!/usr/bin/env python3
# Build an animated rain-radar loop (WebP + GIF) that looks like the DWD WarnWetter app.
#
# Fetches the live DWD precipitation radar (animation_overview_v3.json + the per-range
# animated PRECIPITATION_V4 webp frames), colorizes each frame through the app's own
# legend LUT (precip_scale_17.png), composites it over a basemap built from the app's own
# vector tiles (borders, water, urban areas, place names), labels each frame with its
# timestamp, marks the latest measured frame as "JETZT" (now), and writes an animated,
# looping WebP (lossless) and a GIF.
#
# Reverse-engineered from de.dwd.warnapp 5.4.2; see ../26-scratch/warnwetter-re/WarnWetter-API.org.
# No authentication required for any of the read endpoints used here.
#
# Usage:
#   ./dwd-radar-gif                       # ~2h of past radar ending now, -> radar.webp + radar.gif
#   ./dwd-radar-gif --past 90 --out regen # 90 min of past, -> regen.webp + regen.gif
#   ./dwd-radar-gif --crop full           # whole radar square instead of the Germany crop
#   ./dwd-radar-gif --center Frankfurt --span-km 400   # zoom in around a place (or lon,lat)
#   ./dwd-radar-gif --mark Lorsbach                    # highlight a position on the map
#   ./dwd-radar-gif --mark 'Lorsbach;Niederlauken;Hasselbach;Esch' --crop marks --margin-km 25
#   ./dwd-radar-gif --no-labels           # pristine radar, no basemap/text/legend
#   ./dwd-radar-gif --gif-only            # skip the webp
#   ./dwd-radar-gif --frame-ms 500        # slower playback (ms per frame)
#
# ── Container semantics (verified) ───────────────────────────────────────────
# A PRECIPITATION_V4 file is NOT a single snapshot. The animated WebP holds the whole window at
# 5-min cadence — frame k = winstart + 5k min, (winend-winstart)/5min frames. Frames up to the
# file's {validtime} are measurements, byte-identical across every later-issued file of that
# window; frames past it are nowcast, re-issued every 5 min. Proven by a 12x12 hash matrix over
# one window (perfectly lower-triangular) and by continuity of precipitation counts across
# window seams. Two traps follow:
#   * ImageSequence.Iterator yields the SAME live Image object each step, so
#     `list(Iterator(im))[0]` exhausts the iterator and then reads the LAST frame. Convert
#     inside the loop instead.
#   * Taking frame 0 everywhere is equally wrong: it freezes the whole window on its first
#     step. The measurement for time t is the frame at index (t - winstart)/5.
#
# ── Intensity encoding ────────────────────────────────────────────────────────
# Each webp pixel carries the intensity in G as G = 4 + 8*k for classes k = 0..16 (G = 0 is
# "no precipitation", R = 255 marks the no-data margin outside radar coverage). That is exactly
# 17 classes, matching the 17 colour bands in the app's precip_scale_17.png (85 opaque LUT rows,
# 5 per class). B ∈ {0, 5, 6} on precipitating pixels and most likely encodes the hydrometeor
# type (the app knows Regen/Schneeregen/Schnee/Graupel/Hagel/Gewitter) — UNVERIFIED, unused here.
#
# ── Geo-referencing (calibrated, see --calibrate) ─────────────────────────────
# The 1200x1200 frame is linear in longitude and linear in Web Mercator y, with NON-square
# pixels (≈1.0 km E-W, ≈1.2 km N-S) — i.e. exactly what a MapLibre raster source placed by a
# Mercator bbox looks like. Calibrated against the radar network itself: on an archived
# measurement frame the no-data margin (R = 255) traces the union of the 17 DWD radar sites'
# 150 km coverage discs, and the site coordinates are published (koordinaten-radarverbund.pdf,
# Stand 25.04.2018, reproduced in RADAR_SITES below). Longitude comes from the union's east and
# west tangents; the row scale from a least-squares fit over the six disc tangents that provably
# lie on the union boundary (ASB/BOO/ROS north, FBG/ISN/MEM south) — rms residual 0.32 px.
#   Validation:  coverage-union IoU 0.9975 against the observed no-data mask, and 99.4% of the
#                germany.geojson outline vertices land on radar data (the network is designed to
#                cover Germany). Re-check any time with --calibrate.
#   Previously:  a 2100 km Web-Mercator square at 8.5°E/50.5°N sampled linearly in latitude —
#                IoU 0.778 / 89.4%, i.e. overlays sat 45–90 km off. Override with --bbox.
#   Aspect:      one source pixel is ≈0.99 km E-W but ≈1.22 km N-S, so a 1:1 render stretches
#                the map east-west by 23%. The view scales the axes independently (pixel_aspect)
#                and the output is square in Mercator units, like the app's own display.
# Calibrate on ARCHIVED windows only (`aktuell: false`): those 11+ files agree pixel-for-pixel
# and hold German radar data alone. The live window and the forecast ranges carry extra coverage
# beyond the German discs (foreign radars / extrapolation), which is fine to render but would
# corrupt a fit. Only the .DE product exists — animation_overview_v3.json contains no .EU files,
# so the old --region eu flag never had data behind it and is gone.

import argparse
import io
import json
import math
import os
import subprocess
import sys
import time
import urllib.error
import urllib.parse
import urllib.request
from concurrent.futures import ThreadPoolExecutor
from datetime import datetime, timezone, timedelta
from zoneinfo import ZoneInfo

import numpy as np
from PIL import Image, ImageDraw, ImageFont, ImageSequence

STATIC = "https://app-prod-static.warnwetter.de"
UA = "okhttp/4.12.0"  # the app's OkHttp UA; the host 403s a bare urllib default
LOCAL_TZ = ZoneInfo("Europe/Berlin")  # DWD radar filenames are UTC; labels are local (CET/CEST)
WD = ["Mo", "Di", "Mi", "Do", "Fr", "Sa", "So"]

# ── the radar image's geo-referencing (see header) ───────────────────────────
SRC = 1200  # native frame size, px
GRID = dict(lon0=0.006762, dlon=0.01415669,         # column -> longitude, linear
            y0=1.21655248, dy=0.0003047340)          # row -> Web Mercator y, linear

# DWD Radarverbund, WGS84. Source: dwd.de .../koordinaten-radarverbund.pdf (Stand 25.04.2018).
# Used for the coverage-disc calibration documented above; kept for --calibrate.
RADAR_SITES = {
    "ASB": (6.748292, 53.564011), "BOO": (10.046899, 54.004381), "DRS": (13.768639, 51.124639),
    "EIS": (12.402788, 49.540667), "ESS": (6.967111, 51.405649), "FBG": (8.003611, 47.873611),
    "FLD": (8.801998, 51.311197), "HNR": (9.694533, 52.460083), "ISN": (12.101779, 48.174705),
    "MEM": (10.219222, 48.042145), "NEU": (11.135034, 50.500114), "NHB": (6.548328, 50.109656),
    "OFT": (8.712933, 49.984745), "PRO": (13.858212, 52.648667), "ROS": (12.058076, 54.175660),
    "TUR": (9.782675, 48.585379), "UMD": (11.176091, 52.160096),
}
SITE_RANGE_KM = 150.0


def merc_y(lat):
    return math.log(math.tan(math.pi / 4 + math.radians(lat) / 2))


def inv_merc(y):
    return math.degrees(2 * math.atan(math.exp(y)) - math.pi / 2)


def src_px(lon, lat):
    """(lon, lat) -> floating pixel position in the native 1200x1200 radar frame."""
    return (lon - GRID["lon0"]) / GRID["dlon"], (GRID["y0"] - merc_y(lat)) / GRID["dy"]


def src_lonlat(x, y):
    return GRID["lon0"] + x * GRID["dlon"], inv_merc(GRID["y0"] - y * GRID["dy"])

# ── HTTP ─────────────────────────────────────────────────────────────────────


def fetch(url, timeout=30, retries=2):
    last = None
    for attempt in range(retries + 1):
        try:
            req = urllib.request.Request(url, headers={"User-Agent": UA, "Accept": "*/*"})
            return urllib.request.urlopen(req, timeout=timeout).read()
        except urllib.error.HTTPError:
            raise                       # 403/404 are answers, not failures to retry
        except Exception as e:          # transient: DNS, reset, timeout
            last = e
            if attempt < retries:
                time.sleep(0.4 * (attempt + 1))
    raise last


def fetch_or_none(url):
    """Fetch URL, return bytes, or None on 404/403 (a missing/old valid-time snapshot)."""
    try:
        return fetch(url)
    except urllib.error.HTTPError as e:
        if e.code in (403, 404):
            return None
        raise


def get_overview():
    # /v16/animation_overview_v3.json — the live product with `now`,
    # lastPrecipitationMeasurement, firstPrecipitationForecast, and ~46 data ranges.
    return json.loads(fetch(f"{STATIC}/v16/animation_overview_v3.json"))

# ── minimal Mapbox-Vector-Tile reader (no third-party deps) ──────────────────
# Only what the DWD base tiles need: layers -> features -> (type, props, rings).


def _varint(b, i):
    r = s = 0
    while True:
        x = b[i]
        i += 1
        r |= (x & 0x7F) << s
        if not x & 0x80:
            return r, i
        s += 7


def _fields(b):
    i, n = 0, len(b)
    while i < n:
        key, i = _varint(b, i)
        fn, wt = key >> 3, key & 7
        if wt == 0:
            v, i = _varint(b, i)
        elif wt == 2:
            ln, i = _varint(b, i)
            v = b[i:i + ln]
            i += ln
        elif wt == 5:
            v = b[i:i + 4]
            i += 4
        elif wt == 1:
            v = b[i:i + 8]
            i += 8
        else:
            return
        yield fn, v


def _packed(b):
    out, i = [], 0
    while i < len(b):
        v, i = _varint(b, i)
        out.append(v)
    return out


def _zz(v):
    return (v >> 1) ^ -(v & 1)


def _rings(geom):
    """Decode MVT command/parameter integers into rings of tile-local coordinates."""
    out, cur, x, y, i = [], [], 0, 0, 0
    while i < len(geom):
        cmd = geom[i]
        i += 1
        cid, cnt = cmd & 7, cmd >> 3
        if cid == 1:                       # MoveTo -> starts a new ring/point
            for _ in range(cnt):
                x += _zz(geom[i])
                y += _zz(geom[i + 1])
                i += 2
                if cur:
                    out.append(cur)
                cur = [(x, y)]
        elif cid == 2:                     # LineTo
            for _ in range(cnt):
                x += _zz(geom[i])
                y += _zz(geom[i + 1])
                i += 2
                cur.append((x, y))
        elif cid == 7:                     # ClosePath
            if cur:
                cur.append(cur[0])
                out.append(cur)
                cur = []
        else:
            break
    if cur:
        out.append(cur)
    return out


def parse_mvt(buf):
    layers = {}
    for fn, v in _fields(buf):
        if fn != 3:
            continue
        name, keys, vals, feats, extent = None, [], [], [], 4096
        for f2, v2 in _fields(v):
            if f2 == 1:
                name = v2.decode()
            elif f2 == 3:
                keys.append(v2.decode())
            elif f2 == 5:
                extent = v2
            elif f2 == 4:
                val = None
                for f3, v3 in _fields(v2):
                    val = v3.decode() if f3 == 1 else (bool(v3) if f3 == 7 else v3)
                    break
                vals.append(val)
            elif f2 == 2:
                tags, typ, geom = [], 0, []
                for f3, v3 in _fields(v2):
                    if f3 == 2:
                        tags = _packed(v3)
                    elif f3 == 3:
                        typ = v3
                    elif f3 == 4:
                        geom = _packed(v3)
                feats.append((typ, tags, geom))
        props = []
        for typ, tags, geom in feats:
            p = {}
            for j in range(0, len(tags) - 1, 2):
                if tags[j] < len(keys) and tags[j + 1] < len(vals):
                    p[keys[tags[j]]] = vals[tags[j + 1]]
            props.append((typ, p, _rings(geom)))
        layers[name] = (extent, props)
    return layers


def tile_xy(lon, lat, z):
    n = 2 ** z
    x = (lon + 180.0) / 360.0 * n
    y = (1 - math.log(math.tan(math.radians(lat)) + 1 / math.cos(math.radians(lat))) / math.pi) / 2 * n
    return x, y


def tile_lonlat(tx, ty, gx, gy, extent, z):
    n = 2 ** z
    lon = (tx + gx / extent) / n * 360.0 - 180.0
    lat = math.degrees(math.atan(math.sinh(math.pi * (1 - 2 * (ty + gy / extent) / n))))
    return lon, lat


# ── basemap rendering from the app's own vector tiles ────────────────────────
# Colours lifted from the app's own light-mode style (assets/map/styles/germany/style.json).
C_LAND = (227, 227, 227)   # background
C_URBAN = (230, 230, 230)   # city_area_fill
C_URBANL = (207, 207, 205)   # city_area_line
C_WATER = (195, 204, 211)   # water_fill / water_river_line
C_SEA = (181, 202, 223)   # ocean_fill
C_COAST = (175, 184, 191)   # ocean_coastline
C_BORDER = (88, 106, 121)    # boundary_germany_country
C_STATE = (86, 91, 107)     # boundary_germany_federal_states
C_FOREIGN = (143, 152, 159)   # boundary_other_countries
C_TEXT = (25, 27, 30)      # place labels
C_HALO = (255, 255, 255)


def _adm(props):
    v = props.get("adminLevel")
    try:
        return int(v)
    except (TypeError, ValueError):
        return None


def _boundary_style(props, w):
    """Reproduce the style.json filters for the three boundary layers."""
    lvl, src = _adm(props), props.get("source")
    if lvl == 2 and src == "bkg":                       # boundary_germany_country
        return C_BORDER, max(2, round(w * 2.2))
    if lvl == 4 and src == "bkg":                       # boundary_germany_federal_states
        return C_STATE, w
    if lvl == 2 and not props.get("isGermany"):         # boundary_other_countries
        return C_FOREIGN, max(1, round(w * 1.4))
    return None, 0


def build_basemap(view, zoom, workers, want_places, max_places, font, font_small, marks=()):
    """Render everything that isn't radar: (under, over, note).

    `under` goes below the radar layer (land, urban areas, water), `over` above it
    (borders, place dots and labels). Both are RGBA at the output size.
    """
    W, H = view.size
    lon0, lat0 = view.lonlat(0, 0)
    lon1, lat1 = view.lonlat(W, H)
    x0, y0 = tile_xy(lon0, lat0, zoom)
    x1, y1 = tile_xy(lon1, lat1, zoom)
    txs = range(int(math.floor(min(x0, x1))), int(math.floor(max(x0, x1))) + 1)
    tys = range(int(math.floor(min(y0, y1))), int(math.floor(max(y0, y1))) + 1)
    coords = [(tx, ty) for tx in txs for ty in tys]

    def grab(t):
        tx, ty = t
        try:
            return t, fetch(f"{STATIC}/map/v2/germany/base/{zoom}/{tx}/{ty}.pbf")
        except Exception:
            return t, None

    with ThreadPoolExecutor(max_workers=workers) as ex:
        tiles = list(ex.map(grab, coords))

    under = Image.new("RGBA", (W, H), C_LAND + (255,))
    over = Image.new("RGBA", (W, H), (0, 0, 0, 0))
    du, do = ImageDraw.Draw(under), ImageDraw.Draw(over)
    places, ok = [], 0
    line_w = max(1, round(W / 700))

    def to_px(rings, extent, tx, ty):
        """-> [(points, raw_tile_coords)] so callers can drop segments on the tile clip edge."""
        out = []
        for ring in rings:
            pts = [view.px(*tile_lonlat(tx, ty, gx, gy, extent, zoom)) for gx, gy in ring]
            out.append((pts, ring))
        return out

    def stroke(draw, pts, raw, extent, colour, width):
        """Draw a ring/line, skipping the synthetic segments introduced by tile clipping."""
        run = []
        for i in range(len(pts) - 1):
            (ax, ay), (bx, by) = raw[i], raw[i + 1]
            clipped = ((ax <= 0 and bx <= 0) or (ax >= extent and bx >= extent)
                       or (ay <= 0 and by <= 0) or (ay >= extent and by >= extent))
            if clipped:
                if len(run) >= 2:
                    draw.line(run, fill=colour, width=width)
                run = []
            else:
                run = (run or [pts[i]]) + [pts[i + 1]]
        if len(run) >= 2:
            draw.line(run, fill=colour, width=width)

    for (tx, ty), raw in tiles:
        if raw is None:
            continue
        ok += 1
        try:
            layers = parse_mvt(raw)
        except Exception:
            continue
        # draw order follows style.json: urban areas, inland water, then the sea on top
        for lname, fill, edge in (("city_polygons", C_URBAN, C_URBANL),
                                  ("water", C_WATER, C_WATER),
                                  ("sea", C_SEA, C_COAST)):
            extent, feats = layers.get(lname, (4096, []))
            for typ, props, rings in feats:
                shapes = to_px(rings, extent, tx, ty)
                for pts, raw in shapes:
                    if typ == 3 and len(pts) >= 3:
                        du.polygon(pts, fill=fill)
                for pts, raw in shapes:                # coastlines/riverbanks after the fills
                    if len(pts) >= 2:
                        stroke(du, pts, raw, extent, edge, line_w)
        extent, feats = layers.get("boundary", (4096, []))
        for typ, props, rings in feats:
            col, wid = _boundary_style(props, line_w)
            if col is None:
                continue
            for pts, raw in to_px(rings, extent, tx, ty):
                if len(pts) >= 2:
                    stroke(do, pts, raw, extent, col, wid)
        if want_places:
            extent, feats = layers.get("place", (4096, []))
            for typ, props, rings in feats:
                kind = props.get("type")
                if kind not in ("city", "town"):
                    continue
                name = props.get("name_de") or props.get("name")
                if not name or not rings or not rings[0]:
                    continue
                pop = props.get("population")
                pop = int(pop) if isinstance(pop, (int, float)) else 0
                gx, gy = rings[0][0]
                lon, lat = tile_lonlat(tx, ty, gx, gy, extent, zoom)
                places.append((0 if kind == "city" else 1, -pop, name, view.px(lon, lat)))

    taken = draw_marks(do, view, marks, font, W, H)
    taken.append((0, H - chrome_bar(H) - 2, W, H))     # keep labels out of the status strip
    drawn = 0
    if want_places and places:
        places.sort(key=lambda p: p[:2])          # cities before towns, then by population
        dot = max(2, round(W / 260))
        for _, _, name, (x, y) in places:
            if drawn >= max_places:
                break
            if not (dot < x < W - dot and dot < y < H - dot):
                continue
            box = do.textbbox((x + dot + 3, y - dot - 2), name, font=font_small)
            box = (box[0] - 3, box[1] - 3, box[2] + 3, box[3] + 3)
            if box[0] < 0 or box[1] < 0 or box[2] > W or box[3] > H:
                continue                                   # label would run off the edge
            if any(box[0] < t[2] and t[0] < box[2] and box[1] < t[3] and t[1] < box[3] for t in taken):
                continue
            taken.append(box)
            do.ellipse((x - dot, y - dot, x + dot, y + dot), fill=(55, 55, 60), outline=C_HALO)
            do.text((x + dot + 3, y - dot - 2), name, font=font_small, fill=C_TEXT,
                    stroke_width=2, stroke_fill=C_HALO)
            drawn += 1
    return under, over, f"{ok}/{len(coords)} tiles z{zoom}, {drawn} places, {len(marks)} marks"

# ── outline fallback (used with --no-basemap, or if the tile server is down) ─


def load_outline():
    here = os.path.dirname(os.path.abspath(__file__))
    p = os.path.join(here, "germany.geojson")
    if not os.path.exists(p):
        return []
    d = json.load(open(p))
    rings = []
    for f in d.get("features", []):
        g = f.get("geometry") or f
        t, c = g.get("type"), g.get("coordinates")
        for poly in ([c] if t == "Polygon" else (c if t == "MultiPolygon" else [])):
            rings.append(poly[0] if isinstance(poly[0][0], list) else poly)
    return rings


# ── colourisation (the app's exact legend) ───────────────────────────────────
NCLASS = 17

# The 17 opaque bands of the app's precip_scale_17.png, sampled at the middle of each 5-row
# band. Inlined so the script needs no asset from the APK; --scale re-reads the PNG instead.
PRECIP_LUT = [
    (255, 255, 255), (51, 255, 255), (26, 204, 154), (1, 153, 52), (77, 179, 27),
    (153, 204, 1), (204, 230, 1), (255, 255, 1), (255, 196, 1), (255, 137, 1),
    (255, 69, 1), (254, 0, 0), (229, 0, 76), (204, 0, 152), (102, 0, 203),
    (0, 0, 254), (0, 0, 254),
]


def load_lut(path=None):
    if path is None:
        here = os.path.dirname(os.path.abspath(__file__))
        cand = os.path.join(here, "shader_scales", "precip_scale_17.png")
        if not os.path.exists(cand):
            return PRECIP_LUT
        path = cand
    arr = Image.open(path).convert("RGBA")
    col = [arr.getpixel((0, y)) for y in range(arr.size[1])]
    # 128 RGBA rows; rows 0..84 are the 17 opaque classes at 5 rows each, rest transparent.
    return [col[min(k * 5 + 2, 84)][:3] for k in range(NCLASS)]


def colourise(arr, lut_rgb):
    """HxWx4 uint8 radar frame -> HxWx4 RGBA overlay (transparent where dry / no data)."""
    r, g = arr[:, :, 0], arr[:, :, 1]
    rain = (g > 0) & (r < 255)
    k = np.clip((g.astype(np.int16) - 4) // 8, 0, NCLASS - 1)   # G = 4 + 8*k, 17 classes
    rgb = np.asarray(lut_rgb, dtype=np.uint8)[k]
    return np.dstack([rgb, np.where(rain, 255, 0).astype(np.uint8)])

# ── output geometry ──────────────────────────────────────────────────────────
# The source pixels are NOT square: one column step is math.radians(dlon) in Mercator units,
# one row step is dy. Rendering 1:1 would stretch the map east-west by this factor (~1.23), so
# the view scales the axes independently and the output ends up square in Mercator units —
# the same shapes the app shows.


def pixel_aspect():
    return GRID["dy"] / math.radians(GRID["dlon"])


class View:
    """Maps lon/lat to output pixels for a crop of the native frame."""

    def __init__(self, x0, y0, x1, y1, size):
        self.x0, self.y0, self.x1, self.y1 = x0, y0, x1, y1
        self.sx = 1.0
        self.sy = pixel_aspect()
        if size:
            self.sx = size / max((x1 - x0) * self.sx, (y1 - y0) * self.sy)
            self.sy *= self.sx
        self.size = (round((x1 - x0) * self.sx), round((y1 - y0) * self.sy))

    def px(self, lon, lat):
        x, y = src_px(lon, lat)
        return ((x - self.x0) * self.sx, (y - self.y0) * self.sy)

    def lonlat(self, px, py):
        return src_lonlat(self.x0 + px / self.sx, self.y0 + py / self.sy)


def km_per_px(lat):
    """Ground size of one source pixel at `lat`: (east-west, north-south) in km."""
    g = 6371.0 * math.cos(math.radians(lat))
    return math.radians(GRID["dlon"]) * g, GRID["dy"] * g


def _haversine_km(a, b):
    (lo1, la1), (lo2, la2) = a, b
    p1, p2 = math.radians(la1), math.radians(la2)
    return 6371.0 * math.acos(max(-1.0, min(1.0, math.sin(p1) * math.sin(p2)
                              + math.cos(p1) * math.cos(p2) * math.cos(math.radians(lo2 - lo1)))))


_GEO_CACHE = {}


def geocode(name, near=None):
    """Resolve a place name through the app's own GeoSearch (search-prod.warnwetter.de).

    Village names repeat all over Germany — "Hasselbach" alone scores highest in
    Baden-Württemberg and "Esch" in Luxembourg. With a `near` reference point we prefer
    candidates whose name matches exactly (otherwise "Esch" resolves to nearby *Eschborn*)
    and, among those, the closest one.
    """
    key = (name.casefold(), near)
    if key in _GEO_CACHE:
        return _GEO_CACHE[key]
    q = urllib.parse.urlencode({"query": name, "limit": 25,
                                "placeTypes": "CITY,TOWN,VILLAGE,HAMLET"})
    try:
        req = urllib.request.Request(f"https://search-prod.warnwetter.de/v1/search?{q}",
                                     headers={"User-Agent": UA, "Accept-Language": "de"})
        res = json.loads(urllib.request.urlopen(req, timeout=20).read()).get("results", [])
    except Exception as e:
        sys.exit(f"place lookup for {name!r} failed: {e}")
    if not res:
        sys.exit(f"no place found for {name!r}")
    if near:
        exact = [r for r in res if r["title"].casefold() == name.casefold()]
        pool = exact or res
        pool = sorted(pool, key=lambda r: _haversine_km(
            near, (r["coordinate"]["longitude"], r["coordinate"]["latitude"])))
        hit = pool[0]
    else:
        hit = res[0]
    c = hit["coordinate"]
    out = (c["longitude"], c["latitude"], hit["title"], hit["subtitle"])
    dist = f", {_haversine_km(near, (out[0], out[1])):.0f} km entfernt" if near else ""
    print(f"      {name!r} -> {hit['title']} ({hit['subtitle']}) "
          f"{out[0]:.4f},{out[1]:.4f}{dist}")
    _GEO_CACHE[key] = out
    return out


def resolve_point(spec, near=None):
    """'lon,lat[,label]' or 'PlaceName', either optionally suffixed '|Label'.

    -> (lon, lat, label)
    """
    label = None
    if "|" in spec:
        spec, label = spec.split("|", 1)
    parts = spec.split(",")
    if len(parts) >= 2:
        try:
            lon, lat = float(parts[0]), float(parts[1])
        except ValueError:
            pass
        else:
            if len(parts) > 2 and label is None:
                label = ",".join(parts[2:])
            return lon, lat, (label or f"{lon:.3f},{lat:.3f}")
    lon, lat, title, _sub = geocode(spec.strip(), near)
    return lon, lat, (label or title)


def resolve_marks(specs, near_spec):
    """Resolve --mark specs. The reference for disambiguating later names is --near if
    given, else the first mark resolved (which is usually the unambiguous home village)."""
    ref = None
    if near_spec:
        lon, lat, _ = resolve_point(near_spec)
        ref = (lon, lat)
    marks = []
    for spec in specs or []:
        for part in spec.split(";"):
            part = part.strip()
            if not part:
                continue
            lon, lat, label = resolve_point(part, ref)
            marks.append((lon, lat, label))
            ref = ref or (lon, lat)
    return marks, ref


C_MARK = (0, 82, 204)      # a blue that survives being drawn over the radar palette


def draw_marks(dr, view, marks, font, W, H):
    """Draw highlighted positions; returns their bounding boxes so place labels dodge them.

    Two passes: every marker first, so label placement can see all of them. Villages a few
    km apart (Niederlauken/Hasselbach) otherwise let whoever is drawn first take the space.
    """
    r = max(5, round(W / 85))
    ring = max(2, r // 3)
    boxes, placed = [], []
    for lon, lat, label in marks:
        x, y = view.px(lon, lat)
        if not (0 <= x < W and 0 <= y < H):
            print(f"      note: {label} is outside the view", file=sys.stderr)
            continue
        dr.ellipse((x - r - ring, y - r - ring, x + r + ring, y + r + ring),
                   outline=(255, 255, 255, 235), width=ring)
        dr.ellipse((x - r, y - r, x + r, y + r), fill=(255, 255, 255, 235),
                   outline=C_MARK + (255,), width=ring)
        dr.ellipse((x - r / 3, y - r / 3, x + r / 3, y + r / 3), fill=C_MARK + (255,))
        own = (x - r - ring - 2, y - r - ring - 2, x + r + ring + 2, y + r + ring + 2)
        boxes.append(own)
        if label:
            placed.append((x, y, label, own))
    gap, h = r + ring + 3, font.size
    for x, y, label, own in placed:
        w = dr.textlength(label, font=font)
        spot = None
        for tx, ty in ((x + gap, y - h / 2), (x - gap - w, y - h / 2),          # beside
                       (x + gap, y - gap - h), (x - gap - w, y - gap - h),      # upper corners
                       (x + gap, y + gap), (x - gap - w, y + gap),              # lower corners
                       (x - w / 2, y + gap + 2), (x - w / 2, y - gap - h - 2)):  # under / over
            box = (tx - 3, ty - 3, tx + w + 3, ty + h + 5)
            if box[0] < 0 or box[2] > W or box[1] < 0 or box[3] > H:
                continue
            if any(box[0] < t[2] and t[0] < box[2] and box[1] < t[3] and t[1] < box[3]
                   for t in boxes if t is not own):     # its own marker never blocks it
                continue
            spot = (tx, ty, box)
            break
        if spot is None:
            print(f"      note: no room for the label {label!r}", file=sys.stderr)
            continue
        tx, ty, box = spot
        dr.text((tx, ty), label, font=font, fill=C_MARK + (255,),
                stroke_width=3, stroke_fill=(255, 255, 255, 235))
        boxes.append(box)
    return boxes


def centred_window(lon, lat, span_km):
    """Pixel window of a `span_km` square of ground centred on (lon, lat)."""
    cx, cy = src_px(lon, lat)
    kx, ky = km_per_px(lat)
    hw, hh = span_km / 2 / kx, span_km / 2 / ky
    return (max(0, int(cx - hw)), max(0, int(cy - hh)),
            min(SRC, int(math.ceil(cx + hw))), min(SRC, int(math.ceil(cy + hh))))


def marks_window(marks, margin_km):
    """Pixel window covering every mark plus a margin."""
    los = [m[0] for m in marks]
    las = [m[1] for m in marks]
    lat_mid = sum(las) / len(las)
    kx, ky = km_per_px(lat_mid)
    xs = [src_px(lo, la)[0] for lo, la in zip(los, las)]
    ys = [src_px(lo, la)[1] for lo, la in zip(los, las)]
    mx, my = margin_km / kx, margin_km / ky
    return (max(0, int(min(xs) - mx)), max(0, int(min(ys) - my)),
            min(SRC, int(math.ceil(max(xs) + mx))), min(SRC, int(math.ceil(max(ys) + my))))


# Bounding box of germany.geojson (2753 vertices, needed for exactly these four numbers).
GERMANY_BBOX = (5.86600, 15.04153, 47.27036, 55.05706)   # lon0, lon1, latS, latN


def germany_window(margin_km):
    lon0, lon1, latS, latN = GERMANY_BBOX
    rings = load_outline()
    if rings:                       # prefer the real outline when the geojson is available
        los = [p[0] for r in rings for p in r]
        las = [p[1] for r in rings for p in r]
        lon0, lon1, latS, latN = min(los), max(los), min(las), max(las)
    dlat = margin_km / 111.195
    dlon = margin_km / (111.195 * math.cos(math.radians((latS + latN) / 2)))
    xa, ya = src_px(lon0 - dlon, latN + dlat)
    xb, yb = src_px(lon1 + dlon, latS - dlat)
    return (max(0, int(xa)), max(0, int(ya)),
            min(SRC, int(math.ceil(xb))), min(SRC, int(math.ceil(yb))))

# ── fonts ────────────────────────────────────────────────────────────────────


def font_paths(explicit=None):
    """Regular/bold candidates. An explicit path (--font / DWD_RADAR_FONT) wins, which is how
    a Nix-packaged copy gets a font at all: the store has neither fc-match nor system fonts."""
    out = []
    for p in (explicit, os.environ.get("DWD_RADAR_FONT")):
        if p and os.path.exists(p):
            out.append(p)
    for spec in ("DejaVu Sans", "DejaVu Sans:style=Bold", "sans-serif", "sans-serif:style=Bold"):
        try:
            p = subprocess.run(["fc-match", "-f", "%{file}", spec], capture_output=True,
                               text=True, timeout=10).stdout.strip()
            if p and os.path.exists(p):
                out.append(p)
        except Exception:
            pass
    out += ["/run/current-system/sw/share/fonts/truetype/DejaVuSans.ttf",
            "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf"]
    return out


def pick_fonts(H, regular=None, bold=None):
    """(regular, small, bold) sized relative to the output; falls back to the bitmap font."""
    paths = font_paths(regular)
    bold_paths = font_paths(bold or os.environ.get("DWD_RADAR_FONT_BOLD"))
    reg = [p for p in paths if "Bold" not in p and "bold" not in p] or paths
    bld = [p for p in bold_paths if "Bold" in p or "bold" in p] or reg
    base = max(11, round(H / 42))

    def load(cands, size):
        for p in cands:
            try:
                return ImageFont.truetype(p, size)
            except Exception:
                continue
        return ImageFont.load_default()
    return load(reg, base), load(reg, max(9, round(base * 0.72))), load(bld, base)

# ── chrome: timestamp panel, legend, progress ────────────────────────────────


def chrome_bar(H):
    """Height of the bottom status strip; place labels keep clear of it."""
    return max(28, round(H / 16))


def draw_chrome(im, ts, is_last, idx, total, lut_rgb, font, font_small, font_bold, want_legend):
    W, H = im.size
    ov = Image.new("RGBA", (W, H), (0, 0, 0, 0))
    d = ImageDraw.Draw(ov)
    loc = ts.astimezone(LOCAL_TZ)
    stamp = f"{WD[loc.weekday()]} {loc:%d.%m.} {loc:%H:%M}"
    tz = "MESZ" if loc.utcoffset() == timedelta(hours=2) else "MEZ"
    pad = max(6, round(W / 130))
    bar = chrome_bar(H)
    d.rectangle((0, H - bar, W, H), fill=(255, 255, 255, 214))
    d.line((0, H - bar, W, H - bar), fill=(0, 0, 0, 40), width=1)
    ty = H - bar + (bar - font.size) / 2 - 1
    d.text((pad, ty), stamp, font=font_bold, fill=C_TEXT + (255,))
    wtext = d.textlength(stamp, font=font_bold)
    d.text((pad + wtext + 5, ty + font.size * 0.22), tz, font=font_small, fill=(120, 120, 126, 255))

    if want_legend:
        lw = min(round(W * 0.42), max(120, round(W / 3)))
        lh = max(7, round(bar * 0.30))
        lx, ly = W - pad - lw, H - bar + (bar - lh) / 2 + font_small.size * 0.45
        for k, c in enumerate(lut_rgb):
            a = lx + lw * k / len(lut_rgb)
            b = lx + lw * (k + 1) / len(lut_rgb)
            d.rectangle((a, ly, b + 1, ly + lh), fill=c + (255,))
        d.rectangle((lx, ly, lx + lw, ly + lh), outline=(150, 150, 155, 255))
        d.text((lx, ly - font_small.size - 3), "leicht", font=font_small, fill=(90, 90, 96, 255))
        rt = "stark"
        d.text((lx + lw - d.textlength(rt, font=font_small), ly - font_small.size - 3), rt,
               font=font_small, fill=(90, 90, 96, 255))

    if total > 1:                       # thin progress bar along the very bottom
        d.rectangle((0, H - 3, W * (idx + 1) / total, H), fill=(70, 130, 200, 235))

    if is_last:
        tag, fnt = "JETZT", font_bold
        tw = d.textlength(tag, font=fnt)
        bx, by = W - pad - tw - 10, pad
        d.rounded_rectangle((bx, by, bx + tw + 10, by + fnt.size + 8), radius=4,
                            fill=(205, 30, 30, 235))
        d.text((bx + 5, by + 3), tag, font=fnt, fill=(255, 255, 255, 255))
    im.alpha_composite(ov)

# ── calibration self-check (--calibrate) ─────────────────────────────────────


def calibrate_report(masks):
    mask = np.zeros_like(masks[0])
    for m in masks:
        mask |= m
    mask[:, mask.sum(0) <= 10] = False
    mask[mask.sum(1) <= 10, :] = False
    H, W = mask.shape
    lim = math.cos(SITE_RANGE_KM / 6371.0)
    step = 4
    px = np.arange(0, W, step) + 0.5
    lon = np.radians(GRID["lon0"] + px * GRID["dlon"])
    lat = np.radians([inv_merc(GRID["y0"] - v * GRID["dy"]) for v in px])
    syn = np.zeros((len(px), len(px)), bool)
    for lo, la in RADAR_SITES.values():
        sp, cp, l1 = math.sin(math.radians(la)), math.cos(math.radians(la)), math.radians(lo)
        syn |= (sp * np.sin(lat)[:, None] + cp * np.cos(lat)[:, None] * np.cos(lon[None, :] - l1)) >= lim
    obs = mask[::step, ::step]
    iou = (syn & obs).sum() / max((syn | obs).sum(), 1)
    ins = tot = 0
    for ring in load_outline():
        for lo, la in ring:
            x, y = src_px(lo, la)
            if 0 <= x < W and 0 <= y < H:
                tot += 1
                ins += bool(mask[int(y), int(x)])
    print(f"      coverage-disc IoU {iou:.4f} (17 sites x {SITE_RANGE_KM:.0f} km, {len(masks)} archived frames)")
    print(f"      germany.geojson vertices on radar data: {ins}/{tot} = {100 * ins / max(tot, 1):.2f}%")

# ── main ─────────────────────────────────────────────────────────────────────


def main():
    ap = argparse.ArgumentParser(description="DWD WarnWetter rain-radar -> animated WebP/GIF")
    ap.add_argument("--past", type=int, default=120, help="minutes of past radar to include (default 120)")
    ap.add_argument("--out", default="radar", help="output base name (-> .webp / .gif)")
    ap.add_argument("--frame-ms", type=int, default=350, help="ms per frame in the animation")
    ap.add_argument("--hold-ms", type=int, default=0, help="extra hold on the last frame (default: 3x frame-ms)")
    ap.add_argument("--crop", default="germany", metavar="germany|full|marks|lon0,lon1,latS,latN",
                    help="output framing (default: Germany plus --margin-km); "
                         "'marks' fits the view to every --mark")
    ap.add_argument("--center", "--centre", dest="center", metavar="PLACE|lon,lat",
                    help="zoom in on a place (name looked up via DWD GeoSearch) or lon,lat")
    ap.add_argument("--span-km", type=float, default=300.0,
                    help="width of the --center view in km (default 300)")
    ap.add_argument("--mark", action="append", metavar="PLACE|lon,lat[|Label]",
                    help="highlight a position (repeatable; several per flag with ';')")
    ap.add_argument("--near", metavar="PLACE|lon,lat",
                    help="reference point for disambiguating --mark/--center names "
                         "(default: the first --mark resolved)")
    ap.add_argument("--margin-km", type=float, default=45.0, help="margin around Germany for --crop germany")
    ap.add_argument("--size", type=int, default=1000, help="long edge of the output in px (0 = native)")
    ap.add_argument("--no-labels", action="store_true", help="no basemap, text, places or legend")
    ap.add_argument("--no-basemap", action="store_true", help="skip the vector tiles, draw only the outline")
    ap.add_argument("--no-places", "--no-cities", dest="no_places", action="store_true")
    ap.add_argument("--no-legend", action="store_true")
    ap.add_argument("--max-places", type=int, default=14)
    ap.add_argument("--tile-zoom", type=int, default=0, help="basemap tile zoom (0 = auto)")
    ap.add_argument("--gif-only", action="store_true")
    ap.add_argument("--webp-only", action="store_true")
    ap.add_argument("--bbox", help="override the radar grid: lon0,lon1,latS,latN")
    ap.add_argument("--scale", help="path to a precip_scale_17.png override")
    ap.add_argument("--workers", type=int, default=8, help="parallel fetch/decode threads (default 8)")
    ap.add_argument("--font", help="path to a TTF for labels (env: DWD_RADAR_FONT)")
    ap.add_argument("--font-bold", help="path to a bold TTF (env: DWD_RADAR_FONT_BOLD)")
    ap.add_argument("--calibrate", action="store_true", help="report geo-referencing quality and exit")
    args = ap.parse_args()

    if args.gif_only and args.webp_only:
        sys.exit("--gif-only and --webp-only are mutually exclusive")
    if args.bbox:
        try:
            lo0, lo1, laS, laN = (float(v) for v in args.bbox.split(","))
        except ValueError:
            sys.exit("--bbox needs lon0,lon1,latS,latN")
        GRID.update(lon0=lo0, dlon=(lo1 - lo0) / SRC,
                    y0=merc_y(laN), dy=(merc_y(laN) - merc_y(laS)) / SRC)

    # 1) overview
    print("[1/4] fetching radar overview…", flush=True)
    ov = get_overview()
    now = datetime.fromtimestamp(ov["now"] / 1000, tz=timezone.utc)
    last_meas = datetime.fromtimestamp(ov["lastPrecipitationMeasurement"] / 1000, tz=timezone.utc)
    print(f"      now={now:%H:%M:%S UTC}  lastMeasurement={last_meas:%H:%M UTC}")

    if args.calibrate:
        # Archived windows only: they agree pixel-for-pixel and hold German radar data alone,
        # whereas the live/forecast ranges carry extra coverage that would corrupt the check.
        names = [r["files"]["PRECIPITATION_V4"]["file"] for r in ov["data"]
                 if not r.get("aktuell") and r["files"].get("PRECIPITATION_V4", {}).get("timeStep") == 300_000]

        def _mask(fn):
            # The container's MEASUREMENT frame is the one at its valid time, i.e. index
            # (validtime - winstart)/5 — the last one for an archived window. Nowcast frames
            # carry wider-than-German coverage and would corrupt the disc fit.
            raw = fetch_or_none(f"{STATIC}/v16/{fn}")
            if raw is None:
                return None
            parts = fn.rsplit(".", 4)          # …{winstart}.{winend}.{validtime}.webp
            ws = datetime.strptime(parts[1], "%Y%m%d%H%M")
            vt = datetime.strptime(parts[3], "%Y%m%d%H%M")
            want = int((vt - ws).total_seconds() // 300)
            im = Image.open(io.BytesIO(raw))
            for k, fr in enumerate(ImageSequence.Iterator(im)):
                if k == want:
                    return np.array(fr.convert("RGBA"))[:, :, 0] < 255
            return None
        with ThreadPoolExecutor(max_workers=args.workers) as ex:
            masks = [m for m in ex.map(_mask, names) if m is not None]
        if not masks:
            sys.exit("no archived radar windows available to calibrate against")
        print("[--] calibration check against the DWD radar network:")
        calibrate_report(masks)
        return

    # 2) pick the window files to fetch.
    #    PRECIPITATION_V4.DE.WEB_MERCATOR.{winstart}.{winend}.{validtime}.webp is NOT one
    #    snapshot: the animated container holds the WHOLE window at 5-min cadence, frame k =
    #    winstart + 5k min ((winend-winstart)/5min frames, so 12 for an hourly window, 24 for a
    #    two-hour one). Frames up to {validtime} are measurements and are byte-identical across
    #    every later-issued file of that window; frames beyond it are nowcast and get overwritten
    #    as measurements arrive. So one fetch per WINDOW yields every 5-min step — and archived
    #    windows only serve their canonical valid time anyway, so per-step fetching degraded to
    #    one frame per two hours once a window aged out.
    horizon = last_meas - timedelta(minutes=args.past)
    windows = []
    for r in ov["data"]:
        files = r["files"]
        key = "PRECIPITATION_V4" if "PRECIPITATION_V4" in files else "PRECIPITATION"
        f = files[key]
        if f["timeStep"] != 300_000:      # only the 5-min (observed) cadence
            continue
        ws = datetime.fromtimestamp(r["start"] / 1000, tz=timezone.utc)
        we = datetime.fromtimestamp(r["end"] / 1000, tz=timezone.utc)
        if we <= horizon or ws > last_meas:
            continue
        name = f["file"]
        vt = datetime.strptime(name.rsplit(".", 2)[1], "%Y%m%d%H%M").replace(tzinfo=timezone.utc)
        windows.append((ws, we, name, vt))
    windows.sort(key=lambda w: w[0])
    if not windows:
        sys.exit("no radar windows cover the requested past period")
    print(f"      {len(windows)} window file(s) covering {horizon:%H:%M} to {last_meas:%H:%M} UTC")

    # 3) fetch the windows and unpack their container frames.
    print(f"[2/4] downloading radar windows ({args.workers} threads)…", flush=True)

    def fetch_window(w):
        ws, we, name, vt = w
        data = fetch_or_none(f"{STATIC}/v16/{name}")
        if data is None:
            return []                     # not yet generated / purged
        im = Image.open(io.BytesIO(data))
        out = []
        # Iterate eagerly and convert INSIDE the loop. ImageSequence.Iterator yields the same
        # live Image object every step, so `list(Iterator(im))[0]` exhausts the iterator first
        # and then reads the LAST frame — twelve references to one object parked at frame 11.
        for k, fr in enumerate(ImageSequence.Iterator(im)):
            t = ws + timedelta(minutes=5 * k)
            if t < horizon or t > last_meas or t > vt:
                continue                  # outside the window, or still a nowcast
            out.append((t, np.array(fr.convert("RGBA"))))
        return out

    with ThreadPoolExecutor(max_workers=args.workers) as ex:
        frames = [f for batch in ex.map(fetch_window, windows) for f in batch]
    if not frames:
        sys.exit("no measured radar snapshots available in the requested past window")
    frames.sort(key=lambda f: f[0])
    seen = set()
    frames = [f for f in frames if not (f[0] in seen or seen.add(f[0]))]
    gaps = [(a[0], b[0]) for a, b in zip(frames, frames[1:])
            if (b[0] - a[0]) != timedelta(minutes=5)]
    print(f"      {len(frames)} snapshots from {frames[0][0]:%H:%M} to {frames[-1][0]:%H:%M} UTC")
    for a, b in gaps:
        print(f"      gap: {a:%H:%M} -> {b:%H:%M} ({(b - a).total_seconds() / 60:.0f} min)")

    # 4) crop, colourise, composite, label
    print("[3/4] rendering frames…", flush=True)
    marks, ref = resolve_marks(args.mark, args.near)
    if args.center:
        clon, clat, _ = resolve_point(args.center, ref)
        win = centred_window(clon, clat, args.span_km)
    elif args.crop == "marks":
        if not marks:
            sys.exit("--crop marks needs at least one --mark")
        win = marks_window(marks, args.margin_km)
    elif args.crop == "full":
        win = (0, 0, SRC, SRC)
    elif args.crop == "germany":
        win = germany_window(args.margin_km)
    else:
        try:
            lo0, lo1, laS, laN = (float(v) for v in args.crop.split(","))
        except ValueError:
            sys.exit("--crop needs 'germany', 'full', or lon0,lon1,latS,latN")
        xa, ya = src_px(lo0, laN)
        xb, yb = src_px(lo1, laS)
        win = (max(0, int(xa)), max(0, int(ya)), min(SRC, int(xb)), min(SRC, int(yb)))
    cw, ch = win[2] - win[0], win[3] - win[1]
    if cw < 2 or ch < 2:
        sys.exit("the requested view is empty — check --center / --crop")
    view = View(*win, args.size)
    W, H = view.size
    lat_mid = view.lonlat(W / 2, H / 2)[1]
    kx, ky = km_per_px(lat_mid)
    print(f"      crop {cw}x{ch} px of {SRC}x{SRC} ({cw * kx:.0f}x{ch * ky:.0f} km)  ->  output {W}x{H}")

    lut = load_lut(args.scale)
    font, font_small, font_bold = pick_fonts(H, args.font, args.font_bold)
    labels = not args.no_labels

    under = Image.new("RGBA", (W, H), C_LAND + (255,))
    over = Image.new("RGBA", (W, H), (0, 0, 0, 0))
    if labels and not args.no_basemap:
        zoom = args.tile_zoom or max(4, min(8, round(math.log2(360.0 * W / (abs(view.lonlat(W, 0)[0] - view.lonlat(0, 0)[0]) * 256)))))
        under, over, note = build_basemap(view, zoom, args.workers, not args.no_places,
                                          args.max_places, font, font_small, marks)
        print(f"      basemap: {note}")
    elif labels:
        d = ImageDraw.Draw(over)
        for ring in load_outline():
            pts = [view.px(lo, la) for lo, la in ring]
            d.line(pts + [pts[0]], fill=C_BORDER + (255,), width=max(1, round(W / 700)))
        draw_marks(d, view, marks, font, W, H)
    elif marks:                                   # --no-labels: markers only, no basemap
        draw_marks(ImageDraw.Draw(over), view, marks, font, W, H)

    rendered = []
    for i, (ts, fr) in enumerate(frames):
        radar = Image.fromarray(colourise(fr, lut), "RGBA").crop(win)
        if radar.size != (W, H):
            radar = radar.resize((W, H), Image.NEAREST)   # keep the discrete intensity classes
        im = under.copy()
        im.alpha_composite(radar)
        im.alpha_composite(over)
        if labels:
            draw_chrome(im, ts, i == len(frames) - 1, i, len(frames), lut,
                        font, font_small, font_bold, not args.no_legend)
        rendered.append(im.convert("RGB"))
    print(f"      {len(rendered)} frames @ {W}x{H}")

    # 5) save
    print("[4/4] writing output…", flush=True)
    hold = args.hold_ms or args.frame_ms * 3
    # Steps the server never served (a 403 leaves a 10- or 15-minute gap) get proportionally
    # more screen time, so the loop advances at a constant rate in *weather* time.
    durations = []
    for a, b in zip(frames, frames[1:]):
        step = (b[0] - a[0]).total_seconds() / 300.0
        durations.append(int(round(args.frame_ms * min(step, 4.0))))
    durations.append(args.frame_ms + hold)
    if not args.gif_only:
        p = args.out + ".webp"
        rendered[0].save(p, save_all=True, append_images=rendered[1:], duration=durations,
                         loop=0, lossless=True, method=6)
        print(f"      {p}  ({os.path.getsize(p) // 1024} KiB)")
    if not args.webp_only:
        p = args.out + ".gif"
        rendered[0].save(p, save_all=True, append_images=rendered[1:], duration=durations,
                         loop=0, disposal=2, optimize=True)
        print(f"      {p}  ({os.path.getsize(p) // 1024} KiB)")
    print("done.")


if __name__ == "__main__":
    main()
