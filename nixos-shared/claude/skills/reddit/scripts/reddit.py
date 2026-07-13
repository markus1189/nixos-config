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
    try:
        r = subprocess.run(["pass", entry], capture_output=True, text=True,
                           timeout=20)
    except (OSError, subprocess.TimeoutExpired):
        return None
    if r.returncode != 0 or not r.stdout.strip():
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
    basic = urllib.parse.quote(cid), urllib.parse.quote(csec)
    import base64
    raw = base64.b64encode(f"{cid}:{csec}".encode()).decode()
    req.add_header("Authorization", f"Basic {raw}")
    try:
        with urllib.request.urlopen(req, timeout=30) as r:
            return json.loads(r.read().decode())
    except urllib.error.HTTPError as e:
        die(f"token request failed ({e.code}): {e.read().decode()[:300]}")


def get_token():
    """Return (access_token, is_user_context). Cached on disk until expiry."""
    cid, csec = _creds()
    refresh = (os.environ.get("REDDIT_REFRESH_TOKEN")
               or _pass("api/reddit/refreshToken"))
    key = hashlib.sha256(f"{cid}:{refresh or 'app'}".encode()).hexdigest()[:16]
    os.makedirs(CACHE_DIR, mode=0o700, exist_ok=True)
    path = os.path.join(CACHE_DIR, f"token-{key}.json")

    if os.path.exists(path):
        try:
            with open(path) as f:
                c = json.load(f)
            if c.get("expires_at", 0) > time.time() + 60:
                return c["access_token"], c["user"]
        except (OSError, ValueError, KeyError):
            pass

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
    return entry["access_token"], user


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


def clean(text, width=None):
    if not text:
        return ""
    # Reddit HTML-escapes bodies: &gt; &amp; &#39; all show up raw otherwise.
    t = re.sub(r"\s+", " ", html.unescape(text)).strip()
    if width and len(t) > width:
        t = t[:width - 1].rstrip() + "…"
    return t


def fmt_post(p, idx=None, body_chars=280):
    d = p["data"] if "data" in p else p
    head = f"{idx}. " if idx is not None else ""
    flair = f" [{d['link_flair_text']}]" if d.get("link_flair_text") else ""
    lines = [
        f"{head}{d.get('title', '(no title)')}{flair}",
        f"   r/{d.get('subreddit')} · u/{d.get('author')} · "
        f"{d.get('score', 0)} pts · {d.get('num_comments', 0)} comments · "
        f"{age(d.get('created_utc'))} ago",
        f"   https://reddit.com{d.get('permalink', '')}",
    ]
    if d.get("selftext"):
        lines.append(f"   {clean(d['selftext'], body_chars)}")
    elif d.get("url") and not d.get("is_self"):
        lines.append(f"   → {d['url']}")
    return "\n".join(lines)


def fmt_comment(d, indent=0, body_chars=400):
    pad = "  " * indent
    return (f"{pad}▸ u/{d.get('author')} · {d.get('score', 0)} pts · "
            f"{age(d.get('created_utc'))} ago\n"
            f"{pad}  {clean(d.get('body', ''), body_chars)}")


def walk_comments(children, depth, max_depth, out, body_chars):
    for c in children:
        if c.get("kind") == "more":
            n = c["data"].get("count", 0)
            if n:
                out.append("  " * depth + f"… {n} more replies")
            continue
        if c.get("kind") != "t1":
            continue
        d = c["data"]
        out.append(fmt_comment(d, depth, body_chars))
        replies = d.get("replies")
        if replies and isinstance(replies, dict) and depth + 1 < max_depth:
            walk_comments(replies["data"]["children"], depth + 1,
                          max_depth, out, body_chars)


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


def resolve_redirect(u):
    """Follow reddit's /s/ share links and redd.it shortlinks to the real URL."""
    req = urllib.request.Request(u, method="HEAD")
    req.add_header("User-Agent", UA)
    try:
        with urllib.request.urlopen(req, timeout=HTTP_TIMEOUT) as r:
            return r.url
    except urllib.error.HTTPError as e:
        return getattr(e, "url", u)
    except urllib.error.URLError:
        return u


def classify(s):
    """Map a bare id / t3_id / any reddit URL to (kind, *args).

    kinds: thread(id) | comment(thread_id, comment_id) | sub(name) | user(name)
    """
    s = s.strip()
    if not s.startswith("http"):
        return ("thread", s[3:] if s.startswith("t3_") else s)

    # Share links and shortlinks carry no ids of their own; expand them first.
    if re.search(r"/s/[A-Za-z0-9]+", s) or "redd.it/" in s:
        s = resolve_redirect(s)

    path = urllib.parse.urlparse(s).path

    # /r/<sub>/comments/<thread>/<slug>/<comment>
    m = re.search(r"/comments/([a-z0-9]+)(?:/[^/]*(?:/([a-z0-9]+))?)?", path)
    if m:
        return ("comment", m.group(1), m.group(2)) if m.group(2) \
            else ("thread", m.group(1))
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


def emit(args, raw, text):
    print(json.dumps(raw, indent=2) if args.json else text)


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
    walk_comments(res[1]["data"]["children"], 0, args.depth, out,
                  args.body_chars)
    header = (f"--- focused on comment {focus} (sort={args.sort}) ---"
              if focus else f"--- comments (sort={args.sort}) ---")
    body = f"{fmt_post(post, body_chars=1500)}\n\n{header}\n\n" + "\n\n".join(out)
    emit(args, res, body)


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
            f"{age(c.get('created_utc'))} ago · {m}/{len(terms)} terms\n"
            f"   in: {clean(td.get('title'), 80)} (r/{td.get('subreddit')})\n"
            f"   https://reddit.com{c.get('permalink', '')}\n"
            f"   {clean(c.get('body'), args.body_chars)}")
    body = (f"{len(hits)} comments matching {args.query!r}, mined from "
            f"{len(threads)} threads\n"
            f"(Reddit's API has no comment index — these are the best "
            f"comments inside the most relevant threads)\n\n"
            + "\n\n".join(chunks))
    emit(args, [h[2] for h in hits], body)


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
    emit(args, kids, body)


def cmd_history(args):
    token, user = get_token()
    if not user:
        die("needs your account — run scripts/reddit_auth.py first")
    me = api("/api/v1/me")["name"]
    kids = paginate(f"/user/{me}/{args.what}", {}, args.limit)
    if not kids:
        return emit(args, [], f"nothing in {args.what}")
    parts = []
    for i, k in enumerate(kids, 1):
        d = k["data"]
        if k["kind"] == "t1":
            parts.append(f"{i}. [comment] r/{d['subreddit']} · "
                         f"{d.get('score', 0)} pts · {age(d.get('created_utc'))} ago\n"
                         f"   https://reddit.com{d.get('permalink', '')}\n"
                         f"   {clean(d.get('body'), 200)}")
        else:
            parts.append(fmt_post(k, i))
    emit(args, kids, f"your {args.what} — {len(kids)}\n\n" + "\n\n".join(parts))


def cmd_user(args):
    kids = paginate(f"/user/{args.username}/{args.what}",
                    {"sort": args.sort, "t": args.time}, args.limit)
    if not kids:
        return emit(args, [], f"nothing found for u/{args.username}")
    parts = []
    for i, k in enumerate(kids, 1):
        d = k["data"]
        if k["kind"] == "t1":
            parts.append(f"{i}. [comment] r/{d['subreddit']} · "
                         f"{d.get('score', 0)} pts · {age(d.get('created_utc'))} ago\n"
                         f"   https://reddit.com{d.get('permalink', '')}\n"
                         f"   {clean(d.get('body'), args.body_chars)}")
        else:
            parts.append(fmt_post(k, i))
    emit(args, kids,
         f"u/{args.username} · {args.what} — {len(kids)}\n\n" + "\n\n".join(parts))


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
    p.add_argument("--json", action="store_true", help="raw JSON output")
    sub = p.add_subparsers(dest="cmd", required=True)

    s = sub.add_parser("search", help="search posts")
    s.add_argument("query")
    s.add_argument("--sub", help="restrict to a subreddit")
    s.add_argument("--sort", choices=SORTS, default="relevance")
    s.add_argument("--time", choices=TIMES, default="all")
    s.add_argument("--limit", type=int, default=10)
    s.set_defaults(func=cmd_search)

    s = sub.add_parser("search-comments",
                       help="find comments matching a query (two-stage)")
    s.add_argument("query")
    s.add_argument("--sub")
    s.add_argument("--sort", choices=SORTS, default="relevance")
    s.add_argument("--time", choices=TIMES, default="all")
    s.add_argument("--threads", type=int, default=5,
                   help="how many threads to mine (default 5)")
    s.add_argument("--limit", type=int, default=15, help="comments to return")
    s.add_argument("--body-chars", type=int, default=400)
    s.set_defaults(func=cmd_search_comments)

    s = sub.add_parser("frontpage", help="YOUR personalised frontpage")
    s.add_argument("--sort", choices=["best", "hot", "new", "top"],
                   default="best")
    s.add_argument("--time", choices=TIMES, default="day")
    s.add_argument("--limit", type=int, default=15)
    s.set_defaults(func=cmd_frontpage)

    s = sub.add_parser("sub", help="listing for one subreddit")
    s.add_argument("sub")
    s.add_argument("--sort", choices=["hot", "new", "top", "rising"],
                   default="hot")
    s.add_argument("--time", choices=TIMES, default="day")
    s.add_argument("--limit", type=int, default=15)
    s.set_defaults(func=cmd_sub)

    s = sub.add_parser("comments", help="full comment thread for a post")
    s.add_argument("post", help="post id, t3_id, or reddit URL")
    s.add_argument("--sort", default="top",
                   choices=["top", "best", "new", "controversial", "old", "qa"])
    s.add_argument("--limit", type=int, default=100)
    s.add_argument("--depth", type=int, default=4)
    s.add_argument("--body-chars", type=int, default=400)
    s.set_defaults(func=cmd_comments)

    s = sub.add_parser("subs", help="your subscribed subreddits")
    # High enough to fetch every subscription in one go; paginate() stops as
    # soon as Reddit runs out of pages, so this costs nothing extra.
    s.add_argument("--limit", type=int, default=2000)
    s.set_defaults(func=cmd_subs)

    s = sub.add_parser("history", help="your saved / upvoted / submitted")
    s.add_argument("what", choices=["saved", "upvoted", "submitted",
                                    "comments", "downvoted", "hidden"])
    s.add_argument("--limit", type=int, default=25)
    s.set_defaults(func=cmd_history)

    s = sub.add_parser("user", help="another user's posts or comments")
    s.add_argument("username")
    s.add_argument("what", nargs="?", default="overview",
                   choices=["overview", "submitted", "comments"])
    s.add_argument("--sort", choices=["new", "hot", "top"], default="new")
    s.add_argument("--time", choices=TIMES, default="all")
    s.add_argument("--limit", type=int, default=15)
    s.add_argument("--body-chars", type=int, default=300)
    s.set_defaults(func=cmd_user)

    s = sub.add_parser("url", help="fetch any reddit URL (thread, comment "
                                   "permalink, subreddit, user, share link)")
    s.add_argument("url")
    s.add_argument("--limit", type=int, default=100)
    s.add_argument("--depth", type=int, default=4)
    s.add_argument("--body-chars", type=int, default=400)
    s.set_defaults(func=cmd_url)

    s = sub.add_parser("whoami", help="show which token is in use")
    s.set_defaults(func=cmd_whoami)

    args = p.parse_args()
    args.func(args)


if __name__ == "__main__":
    main()
