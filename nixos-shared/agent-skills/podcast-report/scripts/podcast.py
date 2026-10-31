#!/usr/bin/env nix
#! nix shell --impure --expr ``
#! nix with (import (builtins.getFlake "nixpkgs") {});
#! nix python3.withPackages (ps: with ps; [ requests beautifulsoup4 ])
#! nix ``
#! nix --command python3
"""Resolve a podcast episode from Pocket Casts and fetch its transcript.

  podcast.py episode [--show S] [--title T | --uuid U] [--feed URL] -o DIR
                                                  resolve -> DIR/episode.json, DIR/notes.md
  podcast.py transcript DIR                       transcript -> DIR/transcript.txt
  podcast.py audio DIR                            download the enclosure -> DIR/audio.<ext>

Episode uuids come from `pocketcasts.py history` (the pocketcasts skill's client, next to this file).

Exit codes: 0 ok · 1 error (message says why) · 2 no transcript ·
3 needs a choice (candidates are printed; rerun with --uuid or --feed).
"""

import argparse
import json
import re
import sys
import time
import urllib.parse
import xml.etree.ElementTree as ET
from pathlib import Path

import requests
from bs4 import BeautifulSoup

# The checkout links pocketcasts.py in from the pocketcasts skill; the build copies it.
from pocketcasts import PocketCasts, choose, die, hms, norm, pct, seconds, show_notes

# beehiiv's Cloudflare challenges curl/requests default UAs; a browser UA passes.
UA = {"User-Agent": "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 "
                    "(KHTML, like Gecko) Chrome/141.0 Safari/537.36"}
NS = {"content": "http://purl.org/rss/1.0/modules/content/"}
# Older feeds still declare the namespace by its original GitHub URL.
PODCAST_NS = ["https://podcastindex.org/namespace/1.0",
              "https://github.com/Podcastindex-org/podcast-namespace/blob/main/docs/1.0.md"]
TRANSCRIPT_PREF = ["application/json", "text/vtt", "application/x-subrip",
                   "application/srt", "text/html", "text/plain"]
# Tracking redirects wrapped around enclosure URLs; Pocket Casts and the feed
# can disagree on which wrappers are present.
REDIRECT = re.compile(r"(?:www\.)?(?:podtrac\.com/pts/redirect\.mp3|dts\.podtrac\.com/redirect\.mp3"
                      r"|op3\.dev/e|chtbl\.com/track/[^/]+|pdst\.fm/e|pfx\.vpixl\.com/[^/]+)/(?:https?://)?(.*)")

def strip_redirects(u):
    u = re.sub(r"^https?://", "", u or "")
    while m := REDIRECT.match(u):
        u = m.group(1)
    return u


def itunes_lookup(title):
    # iTunes answers rate limiting with a non-JSON body; back off and retry.
    for attempt in range(3):
        r = requests.get("https://itunes.apple.com/search", timeout=30,
                         params={"media": "podcast", "term": title, "limit": 5})
        try:
            results = r.json()["results"]
            break
        except (ValueError, KeyError):
            time.sleep(5 * (attempt + 1))
    else:
        die("iTunes search keeps failing (rate limit?); pass the feed with --feed URL")
    hit = next((x for x in results if norm(x.get("collectionName")) == norm(title)), None)
    if not hit or not hit.get("feedUrl"):
        print(f"no exact iTunes match with a feed for {title!r}; rerun with --feed URL. Candidates:\n"
              + "\n".join(f"  {x.get('collectionName')}  {x.get('feedUrl')}" for x in results), file=sys.stderr)
        sys.exit(3)
    return hit


def match_item(items, ep):
    """Exact URL, then redirect-stripped URL, then title, then unique last path segment.

    Titles alone are not a key: publishers rename episodes after Pocket Casts cached them.
    The last path segment alone is not either: ZEIT/NPR/Art19 keep the ID in the query.
    Items without an enclosure are skipped: there is nothing to listen to or transcribe.
    """
    encs = [(i, i.find("enclosure").get("url", "")) for i in items if i.find("enclosure") is not None]
    for how, pred in (
        ("exact", lambda u, i: u == ep["url"]),
        ("stripped", lambda u, i: strip_redirects(u) == strip_redirects(ep["url"])),
        ("title", lambda u, i: norm(ep["title"]) and norm(i.findtext("title")) == norm(ep["title"])),
    ):
        for i, u in encs:
            if pred(u, i):
                return i, how
    tail = urllib.parse.urlparse(ep["url"]).path.rsplit("/", 1)[-1]
    same = [i for i, u in encs if tail and urllib.parse.urlparse(u).path.rsplit("/", 1)[-1] == tail]
    return (same[0], "tail-unique") if len(same) == 1 else (None, None)


def pi_all(item, tag):
    return [x for ns in PODCAST_NS for x in item.findall(f"{{{ns}}}{tag}")]


def cmd_episode(args):
    pc = PocketCasts()
    ep, why, alternatives = choose(pc.history(), args.show, args.title, args.uuid)
    show = ep["podcastTitle"]
    hit = None if args.feed else itunes_lookup(show)
    feed_url = args.feed or hit["feedUrl"]
    genres = hit.get("genres", []) if hit else []
    feed = requests.get(feed_url, headers=UA, timeout=60)
    feed.raise_for_status()
    item, how = match_item(ET.fromstring(feed.content).findall("./channel/item"), ep)
    if item is None:
        die(f"episode {ep['title']!r} not found in feed {feed_url}")

    bookmarks = {b["time"]: b.get("title") for b in pc.bookmarks(ep["uuid"])}
    # Whether the history entry's own `bookmarks` field ever fills is unverified; merge it in.
    for b in ep.get("bookmarks") or []:
        if isinstance(b, dict) and "time" in b:
            bookmarks.setdefault(b["time"], b.get("title"))
    # Pocket Casts generates transcripts for many shows whose feed has none.
    try:
        generated = show_notes(ep["podcastUuid"], ep["uuid"]).get("pocket_casts_transcripts") or []
    except (requests.RequestException, ValueError, KeyError) as ex:
        print(f"Pocket Casts show notes unavailable, feed transcripts only: {ex}", file=sys.stderr)
        generated = []
    notes_html = item.findtext("content:encoded", namespaces=NS) or item.findtext("description") or ""
    out = Path(args.o)
    out.mkdir(parents=True, exist_ok=True)
    meta = {
        "show": show,
        "title": item.findtext("title"),
        "published": ep.get("published"),
        "duration_s": ep.get("duration"),
        "played_pct": pct(ep),
        "picked_because": why,
        "alternatives": alternatives,
        "genres": genres,
        # null with --feed: no iTunes genres to judge by.
        "fiction": any("Fiction" in g for g in genres) if hit else None,
        "feed_url": feed_url,
        "page_url": item.findtext("link"),
        "guid": (item.findtext("guid") or "").strip(),
        "audio_url": item.find("enclosure").get("url"),
        "matched_by": how,
        "transcripts": [t.attrib for t in pi_all(item, "transcript")]
                       + [{**t, "origin": "pocketcasts"} for t in generated],
        "persons": [{"name": (p.text or "").strip(), **p.attrib} for p in pi_all(item, "person")],
        "chapters_url": next((c.get("url") for c in pi_all(item, "chapters")), None),
        "bookmarks": [{"t": t, "title": bookmarks[t]} for t in sorted(bookmarks)],
        "pocketcasts": {"episode": ep["uuid"], "podcast": ep["podcastUuid"]},
    }
    (out / "episode.json").write_text(json.dumps(meta, indent=2, ensure_ascii=False))
    (out / "notes.md").write_text(BeautifulSoup(notes_html, "html.parser").get_text("\n").strip() + "\n")
    summary = {k: meta[k] for k in ("show", "title", "played_pct", "picked_because", "alternatives",
                                    "fiction", "matched_by", "page_url")}
    print(json.dumps(summary | {"transcripts": [" ".join(filter(None, (t.get("type"), t.get("origin"))))
                                                for t in meta["transcripts"]],
                                "bookmarks": len(meta["bookmarks"])}, ensure_ascii=False, indent=1))


CUE = re.compile(r"((?:\d+:)?\d\d:\d\d[.,]\d+)\s+-->[^\n]*\n(.*?)(?=\n\s*\n|\Z)", re.S)


def parse_cues(text):
    """VTT/SRT -> [(seconds, speaker, text)].

    Speakers come from <v NAME> voice tags (several per cue split into rows) or a
    Buzzsprout-style `SPEAKER_nn:` prefix. Buzzsprout prefixes only a turn's first cue,
    so an unlabelled cue inherits the previous speaker; with no labels at all it's '?'.
    """
    out, spk = [], "?"
    for m in CUE.finditer(text):
        t, body = seconds(m.group(1)), " ".join(m.group(2).split())
        parts = re.split(r"<v(?:\.[^ >]*)? ([^>]+)>", body)
        segments = [(None, parts[0])] + list(zip(parts[1::2], parts[2::2]))
        for voice, txt in segments:
            txt = re.sub(r"<[^>]+>", "", txt).strip()
            if voice:
                spk = voice.strip()
            elif (p := re.match(r"(SPEAKER_\d+):\s*(.*)", txt)):
                spk, txt = p.group(1), p.group(2)
            if txt:
                out.append((t, spk, txt))
    return out


def parse_html(body):
    """Podcasting 2.0 HTML transcript (<cite> speaker, <time> start, <p> text) -> rows."""
    soup = BeautifulSoup(body, "html.parser")
    rows, spk, t = [], "?", None
    for el in soup.find_all(["cite", "time", "p"]):
        txt = " ".join(el.get_text(" ").split())
        if el.name == "cite":
            spk = txt.rstrip(":") or "?"
        elif el.name == "time":
            t = seconds(txt) if re.fullmatch(r"[\d:.,]+", txt) else t
        elif txt and t is not None:
            rows.append((t, spk, txt))
    return rows


def to_turns(kind, body):
    """Returns (turns, has_timestamps). Without timestamps the whole text is one turn."""
    if kind == "application/json":
        rows = [(s["startTime"], s.get("speaker", "?"), s["body"]) for s in json.loads(body)["segments"]]
    elif kind in ("text/html", "text/plain"):
        rows = parse_html(body) if kind == "text/html" else []
        if not rows:
            txt = BeautifulSoup(body, "html.parser").get_text("\n") if kind == "text/html" else body
            return ([[0, "?", " ".join(txt.split())]] if txt.strip() else []), False
    else:
        rows = parse_cues(body)
    # Merge consecutive rows of one speaker. Unlabelled text is cut into ~60 s chunks
    # so cited timestamps stay within a minute of the passage.
    turns = []
    for t, spk, txt in rows:
        if turns and turns[-1][1] == spk and (spk != "?" or t - turns[-1][0] < 60):
            turns[-1][2] += " " + txt
        else:
            turns.append([t, spk, txt])
    return turns, True


def ordered(tags):
    """The publisher's transcripts by TRANSCRIPT_PREF, then Pocket Casts' generated ones, which
    never carry speaker labels."""
    return sorted(tags, key=lambda t: (t.get("origin") == "pocketcasts",
                                       TRANSCRIPT_PREF.index(t["type"]) if t.get("type") in TRANSCRIPT_PREF else 99))


def cmd_transcript(args):
    d = Path(args.dir)
    meta = json.loads((d / "episode.json").read_text())
    tags = ordered(meta["transcripts"])
    if not tags:
        print("no transcript in the feed or from Pocket Casts; try the publisher page, else `audio` + "
              "transcribe-audio", file=sys.stderr)
        sys.exit(2)
    failures = []
    for tag in tags:
        try:
            r = requests.get(tag["url"], headers=UA, timeout=60)
            if r.status_code != 200 or "Just a moment" in r.text[:2000]:
                raise ValueError(f"HTTP {r.status_code}" + (" Cloudflare challenge" if r.status_code == 200 else ""))
            turns, has_ts = to_turns(tag.get("type"), r.text)
            if not turns:
                raise ValueError("parsed to 0 turns")
            break
        except (requests.RequestException, ValueError, KeyError) as ex:
            failures.append(f"{tag.get('type')} {tag.get('url')}: {ex}")
    else:
        die("every feed transcript failed; try the publisher page, else `audio` + transcribe-audio:\n  "
            + "\n  ".join(failures))
    for f in failures:
        print(f"skipped {f}", file=sys.stderr)
    with open(d / "transcript.txt", "w") as f:
        for t, spk, txt in turns:
            f.write(f"[{hms(t)}] {spk}: {txt.strip()}\n")
    print(json.dumps({"source": tag.get("type"), "origin": tag.get("origin", "feed"),
                      "turns": len(turns), "timestamps": has_ts,
                      "words": sum(len(x[2].split()) for x in turns),
                      "speakers": sorted({s for _, s, _ in turns})}))


def cmd_audio(args):
    d = Path(args.dir)
    meta = json.loads((d / "episode.json").read_text())
    ext = Path(urllib.parse.urlparse(meta["audio_url"]).path).suffix or ".mp3"
    dest = d / f"audio{ext}"
    with requests.get(meta["audio_url"], headers=UA, stream=True, timeout=120) as r:
        r.raise_for_status()
        with open(dest, "wb") as f:
            for chunk in r.iter_content(1 << 20):
                f.write(chunk)
    print(dest)


def main():
    p = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    sub = p.add_subparsers(dest="cmd", required=True)
    e = sub.add_parser("episode", help="resolve an episode to episode.json + notes.md",
                       description="Default: the latest started episode, skipping an autoplayed one.")
    e.add_argument("--show", help="substring of the podcast title (narrows every other option)")
    sel = e.add_mutually_exclusive_group()
    sel.add_argument("--title", help="substring of the episode title; several matches exit 3")
    sel.add_argument("--uuid", help="Pocket Casts episode uuid, as printed by `pocketcasts.py history`")
    e.add_argument("--feed", help="RSS feed URL, skipping the iTunes lookup")
    e.add_argument("-o", required=True, help="output dir")
    t = sub.add_parser("transcript", help="fetch the feed transcript into DIR/transcript.txt")
    t.add_argument("dir")
    a = sub.add_parser("audio", help="download the episode audio into DIR")
    a.add_argument("dir")
    args = p.parse_args()
    try:
        {"episode": cmd_episode, "transcript": cmd_transcript, "audio": cmd_audio}[args.cmd](args)
    except requests.RequestException as ex:
        die(f"network: {ex}")
    except ET.ParseError as ex:
        die(f"feed is not valid XML: {ex}")


if __name__ == "__main__":
    main()
