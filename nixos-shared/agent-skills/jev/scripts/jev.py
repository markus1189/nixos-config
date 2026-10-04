#!/usr/bin/env nix
#! nix shell nixpkgs#python3 --command python
"""Ask Jev (TypeSafe's decision model) typed questions via OpenRouter's Decisions API.

  jev.py < request.json                         one request {state, questions}; model added if missing
  jev.py --each items.jsonl --questions q.json  same questions per item; state = {"item": <line>}; JSONL out
  jev.py --dry-run ...                          print request body(s), don't call

Key: $OPENROUTER_API_KEY, else `pass api/openrouter/jev-skill`. Summary (requests, failures,
cost, resolved model) on stderr. Exit: 0 ok, 1 some requests failed, 2 usage/input/auth/billing.
"""
import argparse, json, os, random, subprocess, sys, threading, time, urllib.error, urllib.request
from concurrent.futures import ThreadPoolExecutor

URL = "https://openrouter.ai/api/alpha/decisions"
# Pinned: thresholds tuned on one version don't carry over. `~typesafe/jev-latest` tracks releases.
MODEL = "typesafe/jev-1.13"
# Transient: rate limits, upstream/gateway errors, overload (529). OpenRouter also returns 520
# with HTTP 200 and the code in the body, so the body's error.code is checked too.
RETRY = {408, 429, 500, 502, 503, 504, 520, 524, 529}
# Bad key, no credits, forbidden: every further request fails the same way, so stop the batch.
FATAL = {401, 402, 403}
TRIES = 5        # backoff 1+2+4+8 s ≈ 15 s before giving up on one request
TIMEOUT = 30     # p95 latency is ~0.4 s; 30 s only bounds a hung connection
JOBS = 8         # community reports trouble above ~8 concurrent workers per key


def die(msg):
    print(f"jev.py: {msg}", file=sys.stderr)
    sys.exit(2)


def key():
    if k := os.environ.get("OPENROUTER_API_KEY"):
        return k
    try:
        r = subprocess.run(["pass", "api/openrouter/jev-skill"], capture_output=True, text=True)
    except FileNotFoundError:
        die("no key: set OPENROUTER_API_KEY (pass is not installed)")
    if r.returncode or not r.stdout.strip():
        die("no key: set OPENROUTER_API_KEY or store it at `pass api/openrouter/jev-skill`")
    return r.stdout.splitlines()[0]


def call(body, k, stop):
    """POST with retries; returns the response dict, or {"error": ...}. Sets `stop` on auth/billing errors."""
    if stop.is_set():
        return {"error": {"code": "skipped", "message": "batch stopped after an auth/billing error"}}
    data = json.dumps(body).encode()
    for attempt in range(TRIES):
        req = urllib.request.Request(URL, data, {"Authorization": f"Bearer {k}", "Content-Type": "application/json"})
        try:
            with urllib.request.urlopen(req, timeout=TIMEOUT) as r:
                d = json.load(r)
        except urllib.error.HTTPError as e:
            try: d = json.load(e)
            except ValueError: d = {}
            if not isinstance(d.get("error"), dict):
                d = {"error": {"code": e.code, "message": str(e)}}
            d["error"].setdefault("code", e.code)
        except (urllib.error.URLError, TimeoutError, ValueError) as e:
            d = {"error": {"code": 0, "message": f"transport: {e}"}}
        code = (d.get("error") or {}).get("code")
        if code is None:
            return d
        if code in FATAL:
            stop.set()
            return d
        if code not in RETRY and code != 0:
            return d
        time.sleep(2 ** attempt + random.random())
    return d


def load_json(path, what):
    try:
        with open(path) as f:
            return json.load(f)
    except OSError as e:
        die(f"{what}: {e}")
    except ValueError as e:
        die(f"{what} {path}: invalid JSON: {e}")


def load_items(path):
    try:
        src = sys.stdin if path == "-" else open(path)
    except OSError as e:
        die(f"--each: {e}")
    items = []
    for n, line in enumerate(src, 1):
        if not line.strip():
            continue
        try: items.append(json.loads(line))
        except ValueError as e: die(f"--each {path}:{n}: invalid JSON: {e}")
    if not items:
        die(f"--each {path}: no items")
    return items


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--each", metavar="JSONL", help="one item per line (- for stdin); one request per item")
    ap.add_argument("--questions", metavar="JSON", help="questions map {id: {type, instructions, criteria}}; required with --each")
    ap.add_argument("--context", metavar="FILE", help="JSON or text added to every item's state as state.context")
    ap.add_argument("--model", default=MODEL, help="default: %(default)s")
    ap.add_argument("-j", "--jobs", type=int, default=JOBS, help="concurrent requests (default: %(default)s)")
    ap.add_argument("--dry-run", action="store_true", help="print request bodies as JSONL and exit; no key, no network")
    a = ap.parse_args()

    if a.each:
        if not a.questions:
            ap.error("--each needs --questions")
        qs = load_json(a.questions, "--questions")
        ctx = None
        if a.context:
            try: raw = open(a.context).read()
            except OSError as e: die(f"--context: {e}")
            try: ctx = json.loads(raw)
            except ValueError: ctx = raw
        bodies = [{"model": a.model, "state": {"item": it, **({"context": ctx} if ctx is not None else {})},
                   "questions": qs} for it in load_items(a.each)]
    else:
        if sys.stdin.isatty():
            ap.error("pipe a request JSON on stdin, or use --each")
        try: b = json.load(sys.stdin)
        except ValueError as e: die(f"stdin: invalid JSON: {e}")
        b.setdefault("model", a.model)
        bodies = [b]

    if a.dry_run:
        for b in bodies: print(json.dumps(b, ensure_ascii=False))
        return 0

    k, stop, t0 = key(), threading.Event(), time.monotonic()
    with ThreadPoolExecutor(a.jobs) as ex:
        results = list(ex.map(lambda b: call(b, k, stop), bodies))
    cost = sum((r.get("usage") or {}).get("cost", 0) for r in results)
    failed = sum(1 for r in results if "error" in r)

    if a.each:
        for i, r in enumerate(results):
            print(json.dumps({"i": i, **({"error": r["error"]} if "error" in r else {"answers": r["answers"]})},
                             ensure_ascii=False))
    else:
        print(json.dumps(results[0], indent=2, ensure_ascii=False))
    models = ",".join(sorted({r["model"] for r in results if "model" in r})) or "?"
    print(f"jev.py: {len(results)} req, {failed} failed, ${cost:.6f}, {time.monotonic() - t0:.2f}s, model={models}",
          file=sys.stderr)
    if stop.is_set():
        print("jev.py: stopped on an auth/billing error (see the error lines)", file=sys.stderr)
        return 2
    return 1 if failed else 0


if __name__ == "__main__":
    sys.exit(main())
