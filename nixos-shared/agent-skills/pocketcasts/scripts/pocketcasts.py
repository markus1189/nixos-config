#!/usr/bin/env nix
#! nix shell --impure --expr ``
#! nix with (import (builtins.getFlake "nixpkgs") {});
#! nix python3.withPackages (ps: with ps; [ requests beautifulsoup4 ])
#! nix ``
#! nix --command python3
"""The user's Pocket Casts account: what was heard, what is queued, the catalog, a few writes.

Reads (episode lists: index, played %, published, show, title, episode uuid)
  pocketcasts.py history [-n N] [--show S] [--year YYYY]   last 100 plays; --year reaches further
  pocketcasts.py pick [--show S] [--title T | --uuid U]    the episode the user means, as JSON
  pocketcasts.py up-next|in-progress|new|starred [-n N]
  pocketcasts.py bookmarks [-n N] [--episode U]
  pocketcasts.py subs                                       subscriptions by folder, podcast uuids
  pocketcasts.py episodes P [-n N]                          a show's episodes with the user's state
  pocketcasts.py episode U                                  one episode with the user's state, JSON
  pocketcasts.py notes U                                    page, transcripts, chapters, show notes
  pocketcasts.py search [--episodes] TERM                   podcasts (* = subscribed), or episodes
  pocketcasts.py stats                                      listening time and time saved

Writes (each re-reads and prints the state it left)
  pocketcasts.py queue U [--next]            end of Up Next, or right after the current episode
  pocketcasts.py unqueue U [U ...]
  pocketcasts.py mark U played|unplayed|POSITION        POSITION: seconds or [HH:]MM:SS
  pocketcasts.py star|unstar|archive|unarchive U
  pocketcasts.py subscribe|unsubscribe P

U = episode uuid, P = podcast uuid, as the reads print them. -n 0 lists everything.
Exit codes: 0 ok · 1 error (message says why) · 2 bad arguments ·
3 needs a choice (candidates are printed; rerun with --uuid).

Login: `pass api/pocketcasts/user` and `pass api/pocketcasts/password`.
"""

import argparse
import functools
import json
import os
import re
import subprocess
import sys

import requests
from bs4 import BeautifulSoup

API = "https://api.pocketcasts.com"
CACHE = "https://podcast-api.pocketcasts.com"
UUID = re.compile(r"[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}")
UNPLAYED, IN_PROGRESS, PLAYED = 1, 2, 3  # playingStatus
# Autoplay starts the next episode when one ends: a barely started newest entry
# above a nearly finished one means the user heard the nearly finished one.
AUTOPLAY_MAX_PCT, FINISHED_MIN_PCT = 10, 80


def die(msg, code=1):
    print(f"error: {msg}", file=sys.stderr)
    sys.exit(code)


def norm(s):
    return re.sub(r"[^a-z0-9]", "", (s or "").lower())


def hms(t):
    t = int(float(t))
    return f"{t // 3600:02d}:{t % 3600 // 60:02d}:{t % 60:02d}"


def seconds(stamp):
    """'1:02:03', '02:03', '00:00:10,240', '75' -> seconds."""
    parts = [float(p.replace(",", ".")) for p in stamp.strip().split(":")]
    return int(sum(p * 60 ** i for i, p in enumerate(reversed(parts))))


def uuid_arg(s):
    # The Up Next endpoints queue a missing uuid as "", after which /up_next/list
    # answers "Invalid UUID string" until it is removed (2026-10-31).
    if not UUID.fullmatch(s):
        raise argparse.ArgumentTypeError(f"not a uuid: {s!r}")
    return s


def pass_get(name):
    # gpg-agent has no pinentry-timeout here: an expired cache would open a desktop
    # dialog and block forever, typically while the user is out running. Fail instead.
    env = {**os.environ, "PASSWORD_STORE_GPG_OPTS":
           (os.environ.get("PASSWORD_STORE_GPG_OPTS", "") + " --pinentry-mode error").strip()}
    try:
        r = subprocess.run(["pass", name], capture_output=True, text=True, env=env,
                           stdin=subprocess.DEVNULL, timeout=30)
    except subprocess.TimeoutExpired:
        die(f"pass {name} timed out after 30 s")
    if r.returncode != 0 or not r.stdout.strip():
        die(f"pass {name} failed (gpg key locked? run `pass {name}` once at the desktop): "
            + r.stderr.strip())
    return r.stdout.splitlines()[0]


@functools.cache
def podcast(uuid):
    """Public catalog entry: title, category, ..., and every episode, newest first. None if unknown."""
    r = requests.get(f"{CACHE}/mobile/podcast/full/{uuid}", timeout=60)
    if r.status_code == 404:
        return None
    r.raise_for_status()
    return r.json()["podcast"]


def show_notes(podcast_uuid, episode_uuid):
    """The episode's entry in Pocket Casts' show notes: show_notes (HTML), link, chapters,
    transcripts (the feed's podcast:transcript tags) and pocket_casts_transcripts (generated
    by Pocket Casts). {} if the episode has none."""
    loc = requests.get(f"{CACHE}/mobile/show_notes/full/{podcast_uuid}", timeout=30,
                       params={"disableredirect": "true"})
    loc.raise_for_status()
    r = requests.get(loc.json()["url"], timeout=60)
    r.raise_for_status()
    eps = (r.json().get("podcast") or {}).get("episodes") or []
    return next((e for e in eps if e["uuid"] == episode_uuid), {})


class PocketCasts:
    """A logged-in session; subscriptions and podcast titles are fetched once per run."""

    def __init__(self):
        r = requests.post(f"{API}/user/login", timeout=30, json={
            "email": pass_get("api/pocketcasts/user"), "password": pass_get("api/pocketcasts/password"),
            "scope": "webplayer"})
        if r.status_code != 200:
            die(f"Pocket Casts login: HTTP {r.status_code} {r.text[:200]}")
        self.auth = {"Authorization": f"Bearer {r.json()['token']}"}
        self._subs = None
        self._titles = {}

    def post(self, path, body=None, missing_ok=False):
        r = requests.post(f"{API}{path}", json=body or {}, timeout=30, headers=self.auth)
        if r.status_code == 404 and missing_ok:
            return None
        if not r.ok:
            die(f"{path}: HTTP {r.status_code} {r.text[:200]}")
        return r.json() if r.content.strip() else {}

    def subscriptions(self, fresh=False):
        if self._subs is None or fresh:
            self._subs = self.post("/user/podcast/list")
        return self._subs

    def title(self, podcast_uuid):
        if not self._titles:
            self._titles = {p["uuid"]: p["title"] for p in self.subscriptions()["podcasts"]}
        if podcast_uuid not in self._titles:
            self._titles[podcast_uuid] = (podcast(podcast_uuid) or {}).get("title", "")
        return self._titles[podcast_uuid]

    def fill(self, eps):
        # Pocket Casts sometimes returns an empty podcastTitle, and some lists carry none.
        for e in eps:
            e["podcastTitle"] = e.get("podcastTitle") or self.title(e["podcastUuid"])
        return eps

    def history(self):
        return self.fill(self.post("/user/history")["episodes"])

    def up_next(self):
        return self.fill(up_next_episodes(self.post("/up_next/list", {
            "version": 2, "model": "webplayer", "serverModified": 0, "showPlayStatus": True})))

    def bookmarks(self, episode=None):
        return [b for b in self.post("/user/bookmark/list")["bookmarks"]
                if not episode or b.get("episodeUuid") == episode]

    def episode(self, uuid):
        ep = self.post("/user/episode", {"uuid": uuid}, missing_ok=True) or die(f"no episode {uuid}")
        return self.fill([ep])[0]


def up_next_episodes(resp):
    """/up_next/list -> history-shaped episodes; progress lives in a separate episodeSync list."""
    sync = {s["uuid"]: s for s in resp.get("episodeSync") or []}
    return [{**e, "podcastUuid": e["podcast"],
             **{k: sync[e["uuid"]][k] for k in ("playedUpTo", "duration") if k in sync.get(e["uuid"], {})}}
            for e in resp["episodes"]]


def pct(ep):
    return (ep.get("playedUpTo") or 0) * 100 // max(ep.get("duration") or 1, 1)


def line(ep):
    played = f"{pct(ep):3d}%" if "playedUpTo" in ep else "   -"
    return (f"{played}  {(ep.get('published') or '')[:10]:10}  {(ep.get('podcastTitle') or '?')[:30]:30}  "
            f"{ep['title'][:60]:60}  {ep['uuid']}")


def state(ep):
    s = {UNPLAYED: "unplayed", IN_PROGRESS: f"at {hms(ep.get('playedUpTo') or 0)}",
         PLAYED: "played"}.get(ep.get("playingStatus"), "?")
    flags = [s] + ["starred"] * bool(ep.get("starred")) + ["archived"] * bool(ep.get("isDeleted"))
    return f"{', '.join(flags)} · {ep.get('podcastTitle')} · {ep['title']}"


def choose(hist, show=None, title=None, uuid=None):
    """Pick the history entry the user means. Returns (episode, why, alternatives)."""
    if show is not None and not norm(show):
        die(f"--show {show!r} has no letters or digits to match on")
    if title is not None and not norm(title):
        die(f"--title {title!r} has no letters or digits to match on")
    pool = [e for e in hist if not show or norm(show) in norm(e.get("podcastTitle"))]
    if uuid:
        ep = next((e for e in hist if e["uuid"] == uuid), None)
        return ep or die(f"uuid {uuid} not in the last {len(hist)} history entries"), "uuid", []
    if title:
        hits = [e for e in pool if norm(title) in norm(e["title"])]
        if not hits:
            die(f"no history entry matches title {title!r}" + (f" in show {show!r}" if show else ""))
        if len(hits) > 1:
            more = f"\n… {len(hits) - 10} more; narrow with --show" if len(hits) > 10 else ""
            print("several episodes match; rerun with --uuid:\n" + "\n".join(map(line, hits[:10])) + more,
                  file=sys.stderr)
            sys.exit(3)
        return hits[0], "title", []
    started = [e for e in pool if (e.get("playedUpTo") or 0) > 0]
    if not started:
        die("no started episode in history" + (f" for show {show!r}" if show else ""))
    ep, why = started[0], "latest started"
    if pct(ep) < AUTOPLAY_MAX_PCT:
        done = next((e for e in started[1:3] if pct(e) >= FINISHED_MIN_PCT), None)
        if done:
            ep, why = done, f"latest started is {pct(started[0])}% played (autoplay?); took the finished one"
    return ep, why, [line(e) for e in started[:3] if e is not ep]


def show(eps, n):
    for i, e in enumerate(eps[:n or None]):
        print(f"{i:2d}  {line(e)}")
    if n and len(eps) > n:
        print(f"… {len(eps) - n} more (-n 0 for all)")


def cmd_history(pc, args):
    if args.year:
        changes = pc.post("/history/year", {"year": args.year})["history"]["changes"]
        eps = pc.fill([{"uuid": c["episode"], "podcastUuid": c["podcast"], "title": c["title"],
                        "published": c["published"]}
                       for c in sorted(changes, key=lambda c: int(c["modifiedAt"]), reverse=True)])
    else:
        eps = pc.history()
    show([e for e in eps if not args.show or norm(args.show) in norm(e["podcastTitle"])], args.n)


def cmd_pick(pc, args):
    ep, why, alternatives = choose(pc.history(), args.show, args.title, args.uuid)
    print(json.dumps({k: ep.get(k) for k in ("uuid", "podcastUuid", "podcastTitle", "title", "published", "duration")}
                     | {"played_pct": pct(ep), "picked_because": why, "alternatives": alternatives},
                     ensure_ascii=False, indent=1))


LISTS = {"in-progress": "/user/in_progress", "new": "/user/new_releases", "starred": "/user/starred"}


def cmd_list(pc, args):
    show(pc.up_next() if args.cmd == "up-next" else pc.fill(pc.post(LISTS[args.cmd])["episodes"]), args.n)


def cmd_bookmarks(pc, args):
    marks = sorted(pc.bookmarks(args.episode), key=lambda b: b.get("createdAt") or "", reverse=True)
    titles = {}
    for b in marks[:args.n or None]:
        u = b["episodeUuid"]
        if u not in titles:
            titles[u] = (pc.post("/user/episode", {"uuid": u}, missing_ok=True) or {}).get("title", "?")
        print(f"{(b.get('createdAt') or '')[:10]:10}  {hms(b['time'])}  {(b.get('title') or '')[:20]:20}  "
              f"{titles[u][:50]:50}  {u}")


def cmd_subs(pc, args):
    subs = pc.subscriptions()
    folders = {f["folderUuid"]: f["name"] for f in subs.get("folders") or []}
    for p in sorted(subs["podcasts"], key=lambda p: (folders.get(p.get("folderUuid"), "￿"), norm(p["title"]))):
        print(f"{folders.get(p.get('folderUuid'), '-')[:14]:14}  {(p.get('lastEpisodePublished') or '')[:10]:10}  "
              f"{p['title'][:50]:50}  {p['uuid']}")


def cmd_episodes(pc, args):
    pod = podcast(args.podcast) or die(f"no podcast {args.podcast}")
    mine = {e["uuid"]: e for e in pc.post("/user/podcast/episodes", {"uuid": args.podcast})["episodes"]}
    category = " / ".join(c.strip() for c in (pod.get("category") or "?").splitlines())
    print(f"{pod['title']} · {category} · {len(pod['episodes'])} episodes")
    show([{"uuid": e["uuid"], "title": e["title"], "published": e["published"], "duration": e.get("duration"),
           "podcastTitle": pod["title"], **mine.get(e["uuid"], {})} for e in pod["episodes"]], args.n)


def cmd_episode(pc, args):
    print(json.dumps(pc.episode(args.uuid), ensure_ascii=False, indent=1))


def cmd_notes(pc, args):
    ep = pc.episode(args.uuid)
    sn = show_notes(ep["podcastUuid"], ep["uuid"])
    print(f"{ep['podcastTitle']} · {ep['title']}")
    if sn.get("link"):
        print(f"page: {sn['link']}")
    for origin, key in (("feed", "transcripts"), ("pocketcasts", "pocket_casts_transcripts")):
        for t in sn.get(key) or []:
            print(f"transcript: {t.get('type')}  {origin}  {t.get('url')}")
    for c in sn.get("chapters") or []:
        print(f"chapter: {hms(c['startTime'])}  {c.get('title') or ''}")
    if sn.get("chapters_url"):
        print(f"chapters: {sn['chapters_url']}")
    print("\n" + BeautifulSoup(sn.get("show_notes") or "", "html.parser").get_text("\n").strip())


def cmd_search(pc, args):
    if args.episodes:
        r = requests.post(f"{CACHE}/episode/search", json={"term": " ".join(args.term)}, timeout=30)
        r.raise_for_status()
        for e in r.json()["episodes"][:args.n or None]:
            print(f"{e['published'][:10]}  {e['podcast_title'][:30]:30}  {e['title'][:60]:60}  {e['uuid']}")
        return
    subs = {p["uuid"] for p in pc.subscriptions()["podcasts"]}
    for p in pc.post("/discover/search", {"term": " ".join(args.term)})["podcasts"][:args.n or None]:
        print(f"{'*' if p['uuid'] in subs else ' '} {p['title'][:45]:45}  {(p.get('author') or '')[:30]:30}  {p['uuid']}")


def cmd_stats(pc, args):
    s = pc.post("/user/stats/summary")

    def days(k):
        t = int(s.get(k) or 0)
        return f"{t // 86400}d {t % 86400 // 3600:2d}h {t % 3600 // 60:2d}m"
    print(f"since {(s.get('timesStartedAt') or '?')[:10]}")
    for k, label in (("timeListened", "listened"), ("timeSilenceRemoval", "saved: trim silence"),
                     ("timeSkipping", "saved: skipping"), ("timeIntroSkipping", "saved: intro skipping"),
                     ("timeVariableSpeed", "saved: playback speed")):
        print(f"{label:22} {days(k)}")


def verify(pc, uuid, ok, failed):
    ep = pc.episode(uuid)
    print(state(ep))
    if not ok(ep):
        die(f"Pocket Casts accepted the request, but the episode is {failed}")


def cmd_mark(pc, args):
    ep = pc.episode(args.uuid)
    dur = ep.get("duration") or 0
    if args.to in ("played", "unplayed"):
        status, pos = (PLAYED, dur) if args.to == "played" else (UNPLAYED, 0)
    elif re.fullmatch(r"\d+(:\d\d){0,2}", args.to):
        status, pos = IN_PROGRESS, seconds(args.to)
    else:
        die(f"mark takes played, unplayed or a position, not {args.to!r}", 2)
    pc.post("/sync/update_episode", {"uuid": ep["uuid"], "podcast": ep["podcastUuid"], "status": status,
                                     "position": pos, "duration": dur})
    verify(pc, ep["uuid"], lambda e: e.get("playingStatus") == status and (status != IN_PROGRESS or e.get("playedUpTo") == pos),
           "not marked")


def cmd_flag(pc, args):
    ep = pc.episode(args.uuid)
    on = not args.cmd.startswith("un")
    if args.cmd.endswith("star"):
        pc.post("/sync/update_episode_star", {"uuid": ep["uuid"], "podcast": ep["podcastUuid"], "star": on})
        key, word = "starred", "starred"
    else:
        pc.post("/sync/update_episodes_archive", {"episodes": [{"uuid": ep["uuid"], "podcast": ep["podcastUuid"]}],
                                                  "archive": on})
        key, word = "isDeleted", "archived"
    verify(pc, ep["uuid"], lambda e: bool(e.get(key)) == on, f"still {'not ' * on}{word}")


def cmd_queue(pc, args):
    ep = pc.episode(args.uuid)
    resp = pc.post("/up_next/play_next" if args.next else "/up_next/play_last", {"version": 2, "episode": {
        "uuid": ep["uuid"], "title": ep["title"], "url": ep["url"], "podcast": ep["podcastUuid"],
        "published": ep["published"]}})
    order = [e["uuid"] for e in resp.get("episodes") or []]
    if ep["uuid"] not in order:
        die("Pocket Casts accepted the request, but the episode is not in Up Next")
    print(f"Up Next {order.index(ep['uuid']) + 1} of {len(order)} (1 = playing now) · {ep['podcastTitle']} · {ep['title']}")


def cmd_unqueue(pc, args):
    before = {e["uuid"] for e in pc.up_next()}
    for u in args.uuids:
        if u not in before:
            print(f"not in Up Next: {u}")
    order = pc.post("/up_next/remove", {"uuids": args.uuids}).get("order") or []
    if left := [u for u in args.uuids if u in order]:
        die(f"Pocket Casts accepted the request, but these are still in Up Next: {' '.join(left)}")
    print(f"Up Next now holds {len(order)}")


def cmd_subscribe(pc, args):
    on = args.cmd == "subscribe"
    pod = podcast(args.podcast) or die(f"no podcast {args.podcast}")
    pc.post(f"/user/podcast/{args.cmd}", {"uuid": args.podcast})
    if (args.podcast in {p["uuid"] for p in pc.subscriptions(fresh=True)["podcasts"]}) != on:
        die(f"Pocket Casts accepted the request, but {pod['title']} is still {'un' * on}subscribed")
    print(f"{'subscribed to' if on else 'unsubscribed from'} {pod['title']}")


def main():
    p = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    sub = p.add_subparsers(dest="cmd", required=True)

    def cmd(name, fn, what, n=None):
        s = sub.add_parser(name, help=what)
        s.set_defaults(fn=fn)
        if n is not None:
            s.add_argument("-n", type=int, default=n, help=f"how many (default {n}, 0 = all)")
        return s

    h = cmd("history", cmd_history, "recent plays, newest first", 10)
    h.add_argument("--show", help="substring of the podcast title")
    h.add_argument("--year", type=int, help="every play of that year instead of the last 100")
    k = cmd("pick", cmd_pick, "the history entry the user means, as JSON")
    k.add_argument("--show", help="substring of the podcast title (narrows every other option)")
    sel = k.add_mutually_exclusive_group()
    sel.add_argument("--title", help="substring of the episode title; several matches exit 3")
    sel.add_argument("--uuid", type=uuid_arg, help="episode uuid from history")
    for name, what in (("up-next", "the Up Next queue, 0 = playing now"), ("in-progress", "started, unfinished"),
                       ("new", "new releases from subscriptions"), ("starred", "starred episodes")):
        cmd(name, cmd_list, what, 25)
    b = cmd("bookmarks", cmd_bookmarks, "bookmarks, newest first", 10)
    b.add_argument("--episode", type=uuid_arg, help="only this episode's")
    cmd("subs", cmd_subs, "subscriptions by folder")
    e = cmd("episodes", cmd_episodes, "a show's episodes, newest first, with played %%", 20)
    e.add_argument("podcast", type=uuid_arg)
    cmd("episode", cmd_episode, "one episode with the user's state, as JSON").add_argument("uuid", type=uuid_arg)
    cmd("notes", cmd_notes, "page, transcript URLs, chapters, show notes").add_argument("uuid", type=uuid_arg)
    s = cmd("search", cmd_search, "podcasts by name, or episodes with --episodes", 10)
    s.add_argument("--episodes", action="store_true")
    s.add_argument("term", nargs="+")
    cmd("stats", cmd_stats, "listening time and time saved")
    q = cmd("queue", cmd_queue, "add to Up Next (end, or --next)")
    q.add_argument("uuid", type=uuid_arg)
    q.add_argument("--next", action="store_true", help="right after the current episode")
    cmd("unqueue", cmd_unqueue, "remove from Up Next").add_argument("uuids", nargs="+", type=uuid_arg)
    m = cmd("mark", cmd_mark, "played, unplayed, or in progress at a position")
    m.add_argument("uuid", type=uuid_arg)
    m.add_argument("to", metavar="played|unplayed|POSITION")
    for name in ("star", "unstar", "archive", "unarchive"):
        cmd(name, cmd_flag, f"{name} an episode").add_argument("uuid", type=uuid_arg)
    for name in ("subscribe", "unsubscribe"):
        cmd(name, cmd_subscribe, f"{name} a podcast").add_argument("podcast", type=uuid_arg)
    args = p.parse_args()
    try:
        args.fn(PocketCasts(), args)
    except requests.RequestException as ex:
        die(f"network: {ex}")


if __name__ == "__main__":
    main()
