"""kb-pages ROOT: HTML gallery of every page below a _sync-all-files/ dir.

Prints to stdout; kb-index writes it to ROOT/_sync-all-files/pages.html.
The output is a function of synced content only (no mtimes, no clock):
every host renders the same bytes, so Syncthing never sees a conflict.
"""
import fnmatch
import html
import os
import re
import sys
from html.parser import HTMLParser
from urllib.parse import quote

SYNC = "_sync-all-files"
OUT = SYNC + "/pages.html"
CHUNK = 1 << 16
# BotC companion: 5 MB, meta in the first KB; the cap bounds pages
# without a <p> that would otherwise be parsed to the end.
CAP = 2 << 20
DATED = re.compile(r"^(\d{4}-\d{2})/(\d{2})-([^/]+)/")
# stuff-patterns.nix junkDirs, substituted by the Nix wrapper: the gallery
# must skip exactly what Syncthing skips, or hosts would disagree.
JUNK = ["@junkDirs@"]


class Meta(HTMLParser):
    def __init__(self):
        super().__init__(convert_charrefs=True)
        self.title = self.h1 = self.desc = self.para = ""
        self._in = None
        self._buf = []
        self._skip = 0

    def handle_starttag(self, tag, attrs):
        a = dict(attrs)
        if tag in ("script", "style", "template", "svg"):
            self._skip += 1
        elif tag == "meta" and not self.desc:
            name = (a.get("name") or a.get("property") or "").lower()
            if name in ("description", "og:description"):
                self.desc = norm(a.get("content") or "")
        elif tag in ("title", "h1", "p") and self._in is None:
            self._in, self._buf = tag, []

    def handle_endtag(self, tag):
        if tag in ("script", "style", "template", "svg"):
            self._skip = max(0, self._skip - 1)
        elif tag == self._in:
            text = norm("".join(self._buf))
            self._in = None
            if tag == "title" and not self.title:
                self.title = text
            elif tag == "h1" and not self.h1:
                self.h1 = text
            # Short <p>s are bylines and button labels, not summaries.
            elif tag == "p" and not self.para and len(text) >= 40:
                self.para = text

    def handle_data(self, data):
        if self._in and not self._skip:
            self._buf.append(data)

    def done(self):
        return self.title and (self.desc or self.para)


def norm(s):
    return re.sub(r"\s+", " ", s).strip()


def clip(s, n=220):
    if len(s) <= n:
        return s
    return s[:n].rsplit(" ", 1)[0] + " …"


def read_meta(path):
    p = Meta()
    with open(path, "rb") as f:
        while f.tell() < CAP and not p.done():
            chunk = f.read(CHUNK)
            if not chunk:
                break
            p.feed(chunk.decode("utf-8", "replace"))
    return p


def junk(name):
    return name.startswith(".") or any(
        fnmatch.fnmatch(name, j) for j in JUNK)


def find_pages(root):
    for d, dirs, files in os.walk(root):
        dirs[:] = sorted(x for x in dirs if not junk(x))
        rel = os.path.relpath(d, root)
        parts = [] if rel == "." else rel.split(os.sep)
        if SYNC not in parts:
            continue
        for f in sorted(files):
            r = "/".join(parts + [f])
            if f.lower().endswith((".html", ".htm")) and r != OUT:
                yield r


def entries(root):
    """One entry per page; a dir with index.html is one entry whose
    sibling pages become its sub-links."""
    pages = list(find_pages(root))
    hubs = {os.path.dirname(p) for p in pages
            if os.path.basename(p) == "index.html"}
    out = {}
    for p in pages:
        d = os.path.dirname(p)
        if d in hubs and os.path.basename(p) != "index.html":
            continue
        out[p] = {"path": p, "subs": []}
    for p in pages:
        d = os.path.dirname(p)
        if d in hubs and os.path.basename(p) != "index.html":
            out[d + "/index.html"]["subs"].append(p)
    return list(out.values())


def size(n):
    return f"{n / 1e6:.1f} MB" if n >= 1e6 else f"{max(1, n // 1000)} KB"


def href(p):
    return "../" + quote(p)


def card(root, e):
    p = e["path"]
    m = read_meta(os.path.join(root, p))
    title = m.title or m.h1 or os.path.basename(p)
    desc, auto = (m.desc, False) if m.desc else (m.para, True)
    dm = DATED.match(p)
    where = f"{dm.group(1)}-{dm.group(2)} · {dm.group(3)}" if dm else "Stuff"
    nbytes = sum(os.path.getsize(os.path.join(root, x))
                 for x in [p] + e["subs"])
    subs = "".join(
        f'<a href="{href(s)}">{html.escape(read_meta(os.path.join(root, s)).title or os.path.basename(s))}</a>'
        for s in e["subs"])
    d = (f'<p class="{"auto" if auto else ""}">{html.escape(clip(desc))}</p>'
         if desc else '<p class="none">keine Beschreibung</p>')
    hay = norm(f"{title} {desc} {p}").lower()
    nav = f"<nav>{subs}</nav>" if subs else ""
    return (
        f'<article data-q="{html.escape(hay)}">'
        f'<h3><a href="{href(p)}">{html.escape(title)}</a></h3>{d}{nav}'
        f'<footer>{html.escape(where)} · {size(nbytes)}'
        f' · <span class="path">{html.escape(p)}</span></footer></article>'
    )


def month_of(e):
    dm = DATED.match(e["path"])
    return dm.group(1) if dm else "Ohne Datum"


def render(root):
    es = sorted(entries(root), key=lambda e: e["path"], reverse=True)
    body, cur = [], None
    for e in es:
        m = month_of(e)
        if m != cur:
            if cur is not None:
                body.append("</section>")
            body.append(f'<section><h2>{html.escape(m)}</h2>')
            cur = m
        body.append(card(root, e))
    if cur is not None:
        body.append("</section>")
    return PAGE.replace("{{N}}", str(len(es))).replace(
        "{{BODY}}", "\n".join(body) or '<p class="none">Noch keine Seiten.</p>')


PAGE = """<!doctype html>
<html lang="de"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<meta name="description" content="Alle HTML-Seiten unter _sync-all-files/ in ~/Stuff, erzeugt von kb-index.">
<title>Stuff · Seiten</title>
<style>
:root{color-scheme:light dark;--bg:#f6f5f2;--card:#fff;--text:#1d1d1f;--dim:#6b6b70;--line:#e2e0da;--acc:#c2410c}
@media (prefers-color-scheme:dark){:root{--bg:#141416;--card:#1e1e22;--text:#ececef;--dim:#9a9aa2;--line:#2c2c32;--acc:#fb923c}}
*{box-sizing:border-box}body{margin:0;background:var(--bg);color:var(--text);font:16px/1.45 system-ui,sans-serif}
header{position:sticky;top:0;background:var(--bg);padding:16px 16px 10px;border-bottom:1px solid var(--line);z-index:1}
h1{margin:0 0 10px;font-size:1.3rem}h1 small{color:var(--dim);font-weight:400}
input{width:100%;padding:10px 12px;font:inherit;border:1px solid var(--line);border-radius:10px;background:var(--card);color:var(--text)}
main{max-width:1100px;margin:0 auto;padding:8px 16px 40px}
h2{font-size:.85rem;letter-spacing:.08em;text-transform:uppercase;color:var(--dim);margin:22px 0 8px}
section{display:grid;gap:12px;grid-template-columns:repeat(auto-fill,minmax(300px,1fr))}
section>h2{grid-column:1/-1}
article{background:var(--card);border:1px solid var(--line);border-radius:12px;padding:14px 16px;display:flex;flex-direction:column;gap:6px}
article h3{margin:0;font-size:1.05rem}article h3 a{color:var(--text);text-decoration:none}article h3 a:hover{color:var(--acc)}
article p{margin:0;color:var(--text)}p.auto{color:var(--dim)}p.none{color:var(--dim);font-style:italic}
nav{display:flex;flex-wrap:wrap;gap:6px}nav a{font-size:.85rem;color:var(--acc);text-decoration:none;border:1px solid var(--line);border-radius:999px;padding:2px 10px}
footer{margin-top:auto;font-size:.8rem;color:var(--dim)}.path{opacity:.7;word-break:break-all}
[hidden]{display:none!important}
</style></head><body>
<header><h1>Stuff · Seiten <small>({{N}})</small></h1>
<input id="q" type="search" placeholder="Suchen …" autofocus aria-label="Seiten durchsuchen"></header>
<main>
{{BODY}}
</main>
<script>
const q=document.getElementById("q");
q.addEventListener("input",()=>{const t=q.value.toLowerCase().split(/\\s+/).filter(Boolean);
for(const a of document.querySelectorAll("article"))a.hidden=!t.every(w=>a.dataset.q.includes(w));
for(const s of document.querySelectorAll("section"))s.hidden=!s.querySelector("article:not([hidden])");});
</script></body></html>
"""

if __name__ == "__main__":
    sys.stdout.write(render(sys.argv[1] if len(sys.argv) > 1 else "."))
