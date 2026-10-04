#!/usr/bin/env nix
#! nix shell nixpkgs#python3 --command python
"""Reddit read-only CLI over the official OAuth API.

Auth: app-only (client_credentials) by default; user context (personalised
frontpage, subscriptions, saved/upvoted) when REDDIT_REFRESH_TOKEN is set.
"""

import argparse
import hashlib
import html
import json
import os
import re
import sys
import time
import urllib.error
import urllib.parse
import urllib.request

# Reddit rejects generic user agents; the platform:app:version (by /u/x) form
# is the one they document and rate-limit against.
UA = "claude-code:reddit-skill:v0.2 (by /u/markus1189)"
API = "https://oauth.reddit.com"
CACHE_DIR = os.path.expanduser("~/.cache/claude-reddit")

HTTP_TIMEOUT = 30
# Reddit allows 100 requests/minute. When the remaining budget for the current
# window drops this low, pause rather than spend the last of it and get 429'd
# mid-pagination.
RATELIMIT_FLOOR = 5
# Exponential backoff (1s, 2s, 4s); 429s here are window exhaustion, which
# clears within the minute.
MAX_RETRIES = 4


def die(msg, code=1):
    print(f"error: {msg}", file=sys.stderr)
    sys.exit(code)


# ---------------------------------------------------------------- auth

def _pass(entry):
    """Read the first line of a pass entry; None if it doesn't exist."""
    import subprocess
    for attempt in range(2):
        try:
            r = subprocess.run(["pass", entry], capture_output=True, text=True,
                               timeout=20)
        except (OSError, subprocess.TimeoutExpired):
            return None
        if r.returncode == 0:
            break
        # A missing entry is a normal fallthrough; anything else (locked
        # gpg-agent, pinentry failure) must not masquerade as "no credentials".
        err = r.stderr.strip()
        if not err or "is not in the password store" in err:
            return None
        # gpg-agent sporadically answers "Wrong secret key used" under
        # concurrent decrypts; the identical call succeeds a moment later.
        if attempt == 0:
            time.sleep(1)
            continue
        die(f"pass {entry} failed: {err[:300]}")
    if not r.stdout.strip():
        return None
    return r.stdout.splitlines()[0].strip()


def _creds():
    """Env vars win; otherwise fall back to the pass store."""
    cid = os.environ.get("REDDIT_CLIENT_ID") or _pass("api/reddit/clientId")
    csec = (os.environ.get("REDDIT_CLIENT_SECRET")
            or _pass("api/reddit/clientSecret"))
    if not cid or not csec:
        die("no credentials: set REDDIT_CLIENT_ID / REDDIT_CLIENT_SECRET, "
            "or store them at pass api/reddit/{clientId,clientSecret}")
    return cid, csec


def _post_form(url, data, cid, csec):
    body = urllib.parse.urlencode(data).encode()
    req = urllib.request.Request(url, data=body, method="POST")
    req.add_header("User-Agent", UA)
    import base64
    raw = base64.b64encode(f"{cid}:{csec}".encode()).decode()
    req.add_header("Authorization", f"Basic {raw}")
    try:
        with urllib.request.urlopen(req, timeout=30) as r:
            return json.loads(r.read().decode())
    except urllib.error.HTTPError as e:
        die(f"token request failed ({e.code}): {e.read().decode()[:300]}")


_token = None


def _cache_path():
    """Token cache file, chosen WITHOUT touching pass.

    Each pass read is a gpg decrypt; keying the cache on the pass contents
    cost three of them per API call. Env credentials are free to read, so they
    keep a content-derived key; pass credentials share one fixed slot, at the
    price that a rotated refresh token is noticed only when the cached access
    token expires (≤1h).
    """
    if os.environ.get("REDDIT_CLIENT_ID"):
        ident = (f"{os.environ['REDDIT_CLIENT_ID']}:"
                 f"{os.environ.get('REDDIT_REFRESH_TOKEN') or 'app'}")
        key = hashlib.sha256(ident.encode()).hexdigest()[:16]
    else:
        key = "pass"
    os.makedirs(CACHE_DIR, mode=0o700, exist_ok=True)
    return os.path.join(CACHE_DIR, f"token-{key}.json")


def get_token():
    """Return (access_token, is_user_context). Cached on disk until expiry."""
    global _token
    if _token:
        return _token
    path = _cache_path()

    if os.path.exists(path):
        try:
            with open(path) as f:
                c = json.load(f)
            if c.get("expires_at", 0) > time.time() + 60:
                _token = c["access_token"], c["user"]
                return _token
        except (OSError, ValueError, KeyError):
            pass

    cid, csec = _creds()
    refresh = (os.environ.get("REDDIT_REFRESH_TOKEN")
               or _pass("api/reddit/refreshToken"))
    if refresh:
        data = {"grant_type": "refresh_token", "refresh_token": refresh}
        user = True
    else:
        data = {"grant_type": "client_credentials"}
        user = False

    tok = _post_form("https://www.reddit.com/api/v1/access_token", data, cid, csec)
    if "access_token" not in tok:
        die(f"no access_token in response: {tok}")

    entry = {
        "access_token": tok["access_token"],
        "expires_at": time.time() + int(tok.get("expires_in", 3600)),
        "user": user,
    }
    fd = os.open(path, os.O_WRONLY | os.O_CREAT | os.O_TRUNC, 0o600)
    with os.fdopen(fd, "w") as f:
        json.dump(entry, f)
    _token = entry["access_token"], user
    return _token


# ---------------------------------------------------------------- http

def api(path, params=None, token=None):
    token = token or get_token()[0]
    url = f"{API}{path}"
    if params:
        params = {k: v for k, v in params.items() if v is not None}
        url += "?" + urllib.parse.urlencode(params)
    req = urllib.request.Request(url)
    req.add_header("User-Agent", UA)
    req.add_header("Authorization", f"bearer {token}")

    for attempt in range(MAX_RETRIES):
        try:
            with urllib.request.urlopen(req, timeout=HTTP_TIMEOUT) as r:
                remaining = r.headers.get("X-Ratelimit-Remaining")
                if remaining and float(remaining) < RATELIMIT_FLOOR:
                    time.sleep(2)
                return json.loads(r.read().decode())
        except urllib.error.HTTPError as e:
            if e.code == 429:
                time.sleep(2 ** attempt)
                continue
            if e.code in (401, 403):
                if e.code == 401:
                    # Cached token was revoked/expired early: drop it so the
                    # next run fetches a fresh one.
                    for f in os.listdir(CACHE_DIR):
                        if f.startswith("token-"):
                            os.remove(os.path.join(CACHE_DIR, f))
                die(f"HTTP {e.code} on {path} — token lacks scope, or this "
                    f"endpoint needs user context (set REDDIT_REFRESH_TOKEN "
                    f"via scripts/reddit_auth.py)")
            die(f"HTTP {e.code} on {path}: {e.read().decode()[:200]}")
        except urllib.error.URLError as e:
            if attempt == 3:
                die(f"network error: {e}")
            time.sleep(2 ** attempt)
    die("exhausted retries")


def paginate(path, params, limit):
    """Fetch up to `limit` children across pages (API caps at 100/request)."""
    out, after = [], None
    while len(out) < limit:
        want = min(100, limit - len(out))
        page = api(path, {**params, "limit": want, "after": after})
        data = page.get("data", {})
        kids = data.get("children", [])
        if not kids:
            break
        out.extend(kids)
        after = data.get("after")
        if not after:
            break
    return out[:limit]


# ---------------------------------------------------------------- format

def age(ts):
    if not ts:
        return "?"
    s = time.time() - ts
    for unit, sec in (("y", 31536000), ("mo", 2592000), ("d", 86400),
                      ("h", 3600), ("m", 60)):
        if s >= sec:
            return f"{int(s // sec)}{unit}"
    return "now"


def ymd(ts):
    return time.strftime("%Y-%m-%d", time.gmtime(ts)) if ts else "?"


def link(permalink):
    return f"https://reddit.com{permalink}" if permalink else ""


def clean(text, width=None):
    """One line: for titles and descriptions."""
    if not text:
        return ""
    # Reddit HTML-escapes bodies: &gt; &amp; &#39; all show up raw otherwise.
    t = re.sub(r"\s+", " ", html.unescape(text)).strip()
    if width and len(t) > width:
        t = t[:width - 1].rstrip() + "…"
    return t


def body_text(text, width, pad):
    """Multi-line body. Line breaks carry lists, quotes and code, so they
    survive; blank lines don't, they cost tokens and say nothing. width 0 or
    None means no truncation."""
    if not text:
        return ""
    lines = [ln.rstrip() for ln in html.unescape(text).strip().splitlines()]
    t = "\n".join(ln for ln in lines if ln.strip())
    if width and len(t) > width:
        t = t[:width - 1].rstrip() + "…"
    return pad + t.replace("\n", "\n" + pad)


def fmt_post(p, idx=None, body_chars=280):
    d = p["data"] if "data" in p else p
    head = f"{idx}. " if idx is not None else ""
    flair = f" [{d['link_flair_text']}]" if d.get("link_flair_text") else ""
    lines = [
        f"{head}{d.get('title', '(no title)')}{flair}",
        f"   r/{d.get('subreddit')} · u/{d.get('author')} · "
        f"{d.get('score', 0)} pts · {d.get('num_comments', 0)} comments · "
        f"{ymd(d.get('created_utc'))}",
        f"   {link(d.get('permalink'))}",
    ]
    if d.get("selftext"):
        lines.append(body_text(d["selftext"], body_chars, "   "))
    elif d.get("url") and not d.get("is_self"):
        lines.append(f"   → {d['url']}")
    return "\n".join(lines)


def fmt_comment(d, indent=0, body_chars=400):
    pad = "  " * indent
    return (f"{pad}▸ u/{d.get('author')} · {d.get('score', 0)} pts · "
            f"{ymd(d.get('created_utc'))} · {link(d.get('permalink'))}\n"
            f"{body_text(d.get('body', ''), body_chars, pad + '  ')}")


def more_note(m, parent_link):
    """Text for a `more` stub. count > 0: siblings the listing withheld
    (--limit). count 0: Reddit's "continue this thread", i.e. the --depth cut;
    it carries no count, so it used to vanish without trace."""
    n = m.get("count", 0)
    what = f"{n} more replies" if n else "replies continue deeper"
    return f"… {what} → {parent_link}" if parent_link else f"… {what}"


def walk_comments(children, depth, out, body_chars, parent_link=""):
    # No local depth cap: the API already applied --depth and marks the cut
    # with count-0 `more` stubs, which are rendered here.
    for c in children:
        if c.get("kind") == "more":
            out.append("  " * depth + more_note(c["data"], parent_link))
            continue
        if c.get("kind") != "t1":
            continue
        d = c["data"]
        out.append(fmt_comment(d, depth, body_chars))
        replies = d.get("replies")
        if replies and isinstance(replies, dict):
            walk_comments(replies["data"]["children"], depth + 1, out,
                          body_chars, link(d.get("permalink")))


def record(thing, depth=None):
    """Flat, context-cheap form of a t1/t3 for --jsonl: full body, no
    Reddit envelope (raw --json runs ~140 KB for a 90-comment thread)."""
    kind, d = thing.get("kind"), thing.get("data", {})
    r = {"type": {"t1": "comment", "t3": "post"}.get(kind, kind),
         "id": d.get("id"), "permalink": link(d.get("permalink")),
         "subreddit": d.get("subreddit"), "author": d.get("author"),
         "score": d.get("score"), "date": ymd(d.get("created_utc"))}
    if kind == "t3":
        r.update(title=d.get("title"), num_comments=d.get("num_comments"),
                 body=html.unescape(d.get("selftext") or ""),
                 url=None if d.get("is_self") else d.get("url"))
    elif kind == "t1":
        r.update(parent=d.get("parent_id"), depth=depth,
                 body=html.unescape(d.get("body") or ""))
    return r


def comment_records(children, depth=0, parent_link=""):
    out = []
    for c in children:
        if c.get("kind") == "more":
            out.append({"type": "more", "count": c["data"].get("count", 0),
                        "parent": c["data"].get("parent_id"), "depth": depth,
                        "permalink": parent_link})
        elif c.get("kind") == "t1":
            out.append(record(c, depth))
            r = c["data"].get("replies")
            if r and isinstance(r, dict):
                out += comment_records(r["data"]["children"], depth + 1,
                                       link(c["data"].get("permalink")))
    return out


def flatten_comments(children, acc):
    for c in children:
        if c.get("kind") != "t1":
            continue
        d = c["data"]
        acc.append(d)
        r = d.get("replies")
        if r and isinstance(r, dict):
            flatten_comments(r["data"]["children"], acc)
    return acc


def is_share_link(s):
    return bool(re.search(r"/s/[A-Za-z0-9]+", s)) or "redd.it/" in s


def resolve_redirect(u):
    """Follow reddit's /s/ share links and redd.it shortlinks to the real URL.

    Needs the bearer token: reddit serves a 403 to unauthenticated callers
    *instead* of the redirect, so anonymously the link never expands.
    """
    req = urllib.request.Request(u, method="HEAD")
    req.add_header("User-Agent", UA)
    req.add_header("Authorization", f"bearer {get_token()[0]}")
    try:
        with urllib.request.urlopen(req, timeout=HTTP_TIMEOUT) as r:
            final = r.url
    except urllib.error.HTTPError as e:
        # A 403 may still arrive after a hop or two, leaving a usable url on
        # the exception; an unexpanded one is caught by the guard below.
        final = getattr(e, "url", None) or u
    except urllib.error.URLError as e:
        die(f"network error expanding {u}: {e}")

    # Never hand an unexpanded share link to classify(): '/r/<sub>/s/<id>'
    # matches its subreddit pattern, so the failure would surface as a hot
    # listing for the subreddit rather than as an error.
    if is_share_link(final):
        die(f"could not expand share link: {u}")
    return final


def classify(s):
    """Map a bare id / t3_id / any reddit URL to (kind, *args).

    kinds: thread(id) | comment(thread_id, comment_id) | sub(name) | user(name)
    """
    s = s.strip()
    if not s.startswith("http"):
        return ("thread", s[3:] if s.startswith("t3_") else s)

    # Share links and shortlinks carry no ids of their own; expand them first.
    expanded = is_share_link(s)
    if expanded:
        s = resolve_redirect(s)

    path = urllib.parse.urlparse(s).path

    # /r/<sub>/comments/<thread>/<slug>/<comment>
    m = re.search(r"/comments/([a-z0-9]+)(?:/[^/]*(?:/([a-z0-9]+))?)?", path)
    if m:
        return ("comment", m.group(1), m.group(2)) if m.group(2) \
            else ("thread", m.group(1))

    # A share link always points at a thread or comment. If it expanded to
    # anything else, the id is dead or mistyped and reddit bounced us to the
    # subreddit — falling through would answer with that sub's hot listing,
    # which looks like success.
    if expanded:
        die(f"share link expanded to {s}, which is not a thread — the link is "
            f"probably expired or mistyped")

    m = re.search(r"/(?:r)/([A-Za-z0-9_]+)", path)
    if m:
        return ("sub", m.group(1))
    m = re.search(r"/(?:user|u)/([A-Za-z0-9_\-]+)", path)
    if m:
        return ("user", m.group(1))
    die(f"unrecognised reddit URL: {s}")


# Kept for callers that only ever want a thread id.
def parse_id(s):
    kind, *rest = classify(s)
    if kind in ("thread", "comment"):
        return rest[0]
    die(f"not a thread URL ({kind}: {rest[0]}) — use the `url` command, which "
        f"routes any reddit link to the right endpoint")


def emit(args, raw, text, records=None):
    if getattr(args, "jsonl", False):
        if records is None:
            records = [record(k) for k in raw]
        for r in records:
            print(json.dumps(r, ensure_ascii=False))
    elif args.json:
        print(json.dumps(raw, indent=2))
    else:
        print(text)


# ---------------------------------------------------------------- commands

def cmd_search(args):
    path = f"/r/{args.sub}/search" if args.sub else "/search"
    params = {"q": args.query, "sort": args.sort, "t": args.time,
              "restrict_sr": "1" if args.sub else None, "type": "link",
              "include_over_18": "on"}
    kids = paginate(path, params, args.limit)
    if not kids:
        return emit(args, [], "no results")
    scope = f"r/{args.sub}" if args.sub else "all of reddit"
    body = (f"{len(kids)} results for {args.query!r} in {scope} "
            f"(sort={args.sort}, time={args.time})\n\n" +
            "\n\n".join(fmt_post(p, i + 1) for i, p in enumerate(kids)))
    emit(args, kids, body)


def cmd_frontpage(args):
    token, user = get_token()
    if not user:
        die("frontpage needs your account. Set REDDIT_REFRESH_TOKEN "
            "(run scripts/reddit_auth.py once). Without it Reddit returns "
            "generic popular posts, not your feed.")
    path = "/best" if args.sort == "best" else f"/{args.sort}"
    kids = paginate(path, {"t": args.time}, args.limit)
    body = (f"your frontpage ({args.sort}) — {len(kids)} posts\n\n" +
            "\n\n".join(fmt_post(p, i + 1) for i, p in enumerate(kids)))
    emit(args, kids, body)


def cmd_sub(args):
    kids = paginate(f"/r/{args.sub}/{args.sort}", {"t": args.time}, args.limit)
    if not kids:
        return emit(args, [], f"no posts in r/{args.sub} (does it exist?)")
    body = (f"r/{args.sub} · {args.sort} — {len(kids)} posts\n\n" +
            "\n\n".join(fmt_post(p, i + 1) for i, p in enumerate(kids)))
    emit(args, kids, body)


def cmd_comments(args):
    kind, *rest = classify(args.post)
    if kind not in ("thread", "comment"):
        die(f"that URL points at a {kind}, not a thread — use the `url` "
            f"command, which routes any reddit link to the right endpoint")
    pid = rest[0]
    focus = rest[1] if kind == "comment" else None

    params = {"limit": args.limit, "depth": args.depth, "sort": args.sort}
    if focus:
        # Ask Reddit for just this comment's subtree rather than the whole
        # thread, which is what the permalink actually points at.
        params["comment"] = focus
    res = api(f"/comments/{pid}", params)
    if not isinstance(res, list) or len(res) < 2:
        die(f"no thread found for {args.post!r}")

    post = res[0]["data"]["children"][0]
    out = []
    thread_link = link(post["data"].get("permalink"))
    walk_comments(res[1]["data"]["children"], 0, out, args.body_chars,
                  thread_link)
    header = (f"--- focused on comment {focus} (sort={args.sort}) ---"
              if focus else f"--- comments (sort={args.sort}) ---")
    post_chars = args.body_chars and max(1500, args.body_chars)
    body = (f"{fmt_post(post, body_chars=post_chars)}\n\n{header}\n\n"
            + "\n\n".join(out))
    emit(args, res, body, [record(post)] + comment_records(
        res[1]["data"]["children"], 0, thread_link))


def cmd_url(args):
    """Route any reddit URL to the right endpoint. The entry point when the
    user pastes a link and WebFetch would 403."""
    kind, *rest = classify(args.url)
    if kind in ("thread", "comment"):
        args.post, args.sort = args.url, "top"
        return cmd_comments(args)
    if kind == "sub":
        args.sub, args.sort, args.time = rest[0], "hot", "day"
        return cmd_sub(args)
    if kind == "user":
        args.username, args.what, args.sort, args.time = \
            rest[0], "overview", "new", "all"
        return cmd_user(args)
    die(f"cannot route {args.url!r}")


def cmd_search_comments(args):
    """Two-stage: Reddit has no comment-search API, so find threads, then
    rank their comments by query-term overlap."""
    path = f"/r/{args.sub}/search" if args.sub else "/search"
    threads = paginate(path, {"q": args.query, "sort": args.sort,
                              "t": args.time, "type": "link",
                              "restrict_sr": "1" if args.sub else None},
                       args.threads)
    if not threads:
        return emit(args, [], "no threads matched; nothing to search within")

    terms = [t.lower() for t in re.findall(r"\w+", args.query) if len(t) > 2]
    hits = []
    for t in threads:
        td = t["data"]
        res = api(f"/comments/{td['id']}", {"limit": 200, "depth": 6,
                                            "sort": "top"})
        if not isinstance(res, list) or len(res) < 2:
            continue
        for c in flatten_comments(res[1]["data"]["children"], []):
            b = (c.get("body") or "").lower()
            if not b or b == "[deleted]" or b == "[removed]":
                continue
            score = sum(1 for t_ in terms if t_ in b)
            if score:
                hits.append((score, c.get("score", 0), c, td))

    if not hits:
        return emit(args, [], f"searched {len(threads)} threads, "
                              f"no comments mentioned {args.query!r}")

    hits.sort(key=lambda h: (-h[0], -h[1]))
    hits = hits[:args.limit]
    chunks = []
    for i, (m, _, c, td) in enumerate(hits, 1):
        chunks.append(
            f"{i}. u/{c.get('author')} · {c.get('score', 0)} pts · "
            f"{ymd(c.get('created_utc'))} · {m}/{len(terms)} terms\n"
            f"   in: {clean(td.get('title'), 80)} (r/{td.get('subreddit')})\n"
            f"   {link(c.get('permalink'))}\n"
            f"{body_text(c.get('body'), args.body_chars, '   ')}")
    body = (f"{len(hits)} comments matching {args.query!r}, mined from "
            f"{len(threads)} threads\n"
            f"(Reddit's API has no comment index — these are the best "
            f"comments inside the most relevant threads)\n\n"
            + "\n\n".join(chunks))
    emit(args, [h[2] for h in hits], body,
         [record({"kind": "t1", "data": h[2]}) for h in hits])


def cmd_subs(args):
    token, user = get_token()
    if not user:
        die("needs your account — run scripts/reddit_auth.py first")
    kids = paginate("/subreddits/mine/subscriber", {}, args.limit)
    rows = sorted((k["data"] for k in kids),
                  key=lambda d: -(d.get("subscribers") or 0))
    body = f"{len(rows)} subscribed subreddits\n\n" + "\n".join(
        f"  r/{d['display_name']:<28} {d.get('subscribers', 0):>9,} subs  "
        f"{clean(d.get('public_description'), 60)}" for d in rows)
    emit(args, kids, body, [
        {"type": "subreddit", "name": d["display_name"],
         "subscribers": d.get("subscribers"),
         "description": d.get("public_description")} for d in rows])


def fmt_listing(kids, body_chars):
    """User and history listings mix comments (t1) and posts (t3)."""
    parts = []
    for i, k in enumerate(kids, 1):
        d = k["data"]
        if k["kind"] == "t1":
            parts.append(f"{i}. [comment] r/{d['subreddit']} · "
                         f"{d.get('score', 0)} pts · {ymd(d.get('created_utc'))}\n"
                         f"   {link(d.get('permalink'))}\n"
                         f"{body_text(d.get('body'), body_chars, '   ')}")
        else:
            parts.append(fmt_post(k, i))
    return "\n\n".join(parts)


def cmd_history(args):
    token, user = get_token()
    if not user:
        die("needs your account — run scripts/reddit_auth.py first")
    me = api("/api/v1/me")["name"]
    kids = paginate(f"/user/{me}/{args.what}", {}, args.limit)
    if not kids:
        return emit(args, [], f"nothing in {args.what}")
    emit(args, kids, f"your {args.what} — {len(kids)}\n\n"
         + fmt_listing(kids, 200))


def cmd_user(args):
    kids = paginate(f"/user/{args.username}/{args.what}",
                    {"sort": args.sort, "t": args.time}, args.limit)
    if not kids:
        return emit(args, [], f"nothing found for u/{args.username}")
    emit(args, kids, f"u/{args.username} · {args.what} — {len(kids)}\n\n"
         + fmt_listing(kids, args.body_chars))


def cmd_whoami(args):
    token, user = get_token()
    if not user:
        print("app-only token (no user context). "
              "Run scripts/reddit_auth.py to link your account.")
        return
    me = api("/api/v1/me")
    print(f"authenticated as u/{me['name']} · "
          f"{me.get('total_karma', 0):,} karma · "
          f"account age {age(me.get('created_utc'))}")


# ---------------------------------------------------------------- cli

SORTS = ["relevance", "hot", "top", "new", "comments"]
TIMES = ["hour", "day", "week", "month", "year", "all"]


def main():
    p = argparse.ArgumentParser(
        prog="reddit", description="Read-only Reddit via the official OAuth API")
    jsonl_help = "one compact JSON record per line, full bodies"
    p.add_argument("--json", action="store_true", help="raw JSON output")
    p.add_argument("--jsonl", action="store_true", help=jsonl_help)
    # Also accept both after the subcommand. SUPPRESS keeps the subparser
    # from overwriting a top-level flag with its own default.
    common = argparse.ArgumentParser(add_help=False)
    common.add_argument("--json", action="store_true",
                        default=argparse.SUPPRESS, help="raw JSON output")
    common.add_argument("--jsonl", action="store_true",
                        default=argparse.SUPPRESS, help=jsonl_help)
    sub = p.add_subparsers(dest="cmd", required=True)

    s = sub.add_parser("search", parents=[common],
                       help="search posts")
    s.add_argument("query")
    s.add_argument("--sub", help="restrict to a subreddit")
    s.add_argument("--sort", choices=SORTS, default="relevance")
    s.add_argument("--time", choices=TIMES, default="all")
    s.add_argument("--limit", type=int, default=10)
    s.set_defaults(func=cmd_search)

    s = sub.add_parser("search-comments", parents=[common],
                       help="find comments matching a query (two-stage)")
    s.add_argument("query")
    s.add_argument("--sub")
    s.add_argument("--sort", choices=SORTS, default="relevance")
    s.add_argument("--time", choices=TIMES, default="all")
    s.add_argument("--threads", type=int, default=5,
                   help="how many threads to mine (default 5)")
    s.add_argument("--limit", type=int, default=15, help="comments to return")
    s.add_argument("--body-chars", type=int, default=400,
                   help="truncate bodies; 0 = full text")
    s.set_defaults(func=cmd_search_comments)

    s = sub.add_parser("frontpage", parents=[common],
                       help="YOUR personalised frontpage")
    s.add_argument("--sort", choices=["best", "hot", "new", "top"],
                   default="best")
    s.add_argument("--time", choices=TIMES, default="day")
    s.add_argument("--limit", type=int, default=15)
    s.set_defaults(func=cmd_frontpage)

    s = sub.add_parser("sub", parents=[common],
                       help="listing for one subreddit")
    s.add_argument("sub")
    s.add_argument("--sort", choices=["hot", "new", "top", "rising"],
                   default="hot")
    s.add_argument("--time", choices=TIMES, default="day")
    s.add_argument("--limit", type=int, default=15)
    s.set_defaults(func=cmd_sub)

    s = sub.add_parser("comments", parents=[common],
                       help="full comment thread for a post")
    s.add_argument("post", help="post id, t3_id, or reddit URL")
    s.add_argument("--sort", default="top",
                   choices=["top", "best", "new", "controversial", "old", "qa"])
    s.add_argument("--limit", type=int, default=100)
    s.add_argument("--depth", type=int, default=4)
    s.add_argument("--body-chars", type=int, default=400,
                   help="truncate bodies; 0 = full text")
    s.set_defaults(func=cmd_comments)

    s = sub.add_parser("subs", parents=[common],
                       help="your subscribed subreddits")
    # High enough to fetch every subscription in one go; paginate() stops as
    # soon as Reddit runs out of pages, so this costs nothing extra.
    s.add_argument("--limit", type=int, default=2000)
    s.set_defaults(func=cmd_subs)

    s = sub.add_parser("history", parents=[common],
                       help="your saved / upvoted / submitted")
    s.add_argument("what", choices=["saved", "upvoted", "submitted",
                                    "comments", "downvoted", "hidden"])
    s.add_argument("--limit", type=int, default=25)
    s.set_defaults(func=cmd_history)

    s = sub.add_parser("user", parents=[common],
                       help="another user's posts or comments")
    s.add_argument("username")
    s.add_argument("what", nargs="?", default="overview",
                   choices=["overview", "submitted", "comments"])
    s.add_argument("--sort", choices=["new", "hot", "top"], default="new")
    s.add_argument("--time", choices=TIMES, default="all")
    s.add_argument("--limit", type=int, default=15)
    s.add_argument("--body-chars", type=int, default=300,
                   help="truncate bodies; 0 = full text")
    s.set_defaults(func=cmd_user)

    s = sub.add_parser("url", parents=[common],
                       help="fetch any reddit URL (thread, comment "
                            "permalink, subreddit, user, share link)")
    s.add_argument("url")
    s.add_argument("--limit", type=int, default=100)
    s.add_argument("--depth", type=int, default=4)
    s.add_argument("--body-chars", type=int, default=400,
                   help="truncate bodies; 0 = full text")
    s.set_defaults(func=cmd_url)

    s = sub.add_parser("whoami", help="show which token is in use")
    s.set_defaults(func=cmd_whoami)

    args = p.parse_args()
    args.func(args)


if __name__ == "__main__":
    main()
