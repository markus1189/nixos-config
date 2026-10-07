#!/usr/bin/env nix
#! nix shell nixpkgs#python3 --command python
"""Ask decision models (Jev, Clef) typed questions: state + questions -> calibrated answers.

  jev.py < request.json                         one request {state, questions}; model added if missing
  jev.py --each items.jsonl --questions q.json  same questions per item; state = {"item": <line>}; JSONL out
  jev.py --model jev,clef ...                   compare mode: every item to every model, `disagree` per line
  jev.py --image a.png --model cloudflare/clef --via openrouter --no-zdr ...
                                                attach images; per item: "_images": [paths] in the item
  jev.py --dry-run ...                          print {url, body} per request (base64 elided); no key, no network

Backends: typesafe/* -> OpenRouter /systemone (zero data retention requested); anything else ->
Requesty EU chat completions with a "questions" response_format. --via overrides.
Keys: $OPENROUTER_API_KEY or `pass api/openrouter/jev-skill`; $REQUESTY_API_KEY or
`pass api/requesty/systemone`. Summary (requests, failures, cost, models) on stderr.
Exit: 0 ok, 1 some requests failed, 2 usage/input/auth/billing.
"""
import argparse, base64, json, os, random, subprocess, sys, threading, time, urllib.error, urllib.request
from concurrent.futures import ThreadPoolExecutor

# Pinned: thresholds tuned on one version don't carry over. `~typesafe/jev-latest` tracks releases.
MODEL = "typesafe/jev-1.13"
# sference/clef is the only Clef endpoint approved for the systemone key; cloudflare/* needs a
# Model Library approval in the Requesty console first.
ALIASES = {"jev": MODEL, "clef": "sference/clef"}
OPENROUTER_URL = "https://openrouter.ai/api/v1/systemone"
# The org enforces EU data residency: router.requesty.ai answers 403 for every request.
REQUESTY_URL = os.environ.get("REQUESTY_BASE_URL", "https://router.eu.requesty.ai/v1") + "/chat/completions"
KEYS = {"openrouter": ("OPENROUTER_API_KEY", "api/openrouter/jev-skill"),
        "requesty": ("REQUESTY_API_KEY", "api/requesty/systemone")}
# Zero data retention, no data collection: TypeSafe's endpoint qualifies, so the promise is enforced
# per request rather than assumed from the provider listing. OpenRouter has no ZDR endpoint for
# Clef (404 with zdr), and Requesty takes no per-request equivalent.
PROVIDER = {"zdr": True, "data_collection": "deny"}
PROVIDER_NO_ZDR = {"data_collection": "deny"}
# USD per input token (output is free); only for estimating when usage.cost is absent,
# which the response schema allows.
PRICE = {MODEL: 0.042e-6, "sference/clef": 0.24e-6, "cloudflare/clef": 0.24e-6, "cloudflare/clef-flash": 0.09e-6}
# Transient: rate limits, upstream/gateway errors, overload (529). OpenRouter also returns 520
# with HTTP 200 and the code in the body, so the body's error.code is checked too.
RETRY = {408, 429, 500, 502, 503, 504, 520, 524, 529}
# Bad key, no credits, forbidden: every further request fails the same way, so stop the batch.
# Exceptions in call(): the in-flight-budget 402 is transient, and a 403 carrying metadata is a
# moderation or guardrail verdict on one item's content. Requesty's residency and
# model-not-approved errors are 403s without metadata, so they stop the batch.
FATAL = {401, 402, 403}
RETRY_AFTER_MAX = 60  # seconds; a longer Retry-After fails the item instead of stalling the batch
TRIES = 5        # backoff 1+2+4+8 s ≈ 15 s before giving up on one request
TIMEOUT = 30     # p95 latency is ~0.4 s; 30 s only bounds a hung connection
JOBS = 8         # community reports trouble above ~8 concurrent workers per key
# Clef's documented image limits (developers.cloudflare.com/workers-ai/models/clef/).
MAX_IMAGES, MAX_IMAGE, MAX_IMAGES_TOTAL = 4, 4 << 20, 8 << 20
# Compare mode: scores are divided by their top level index first, as SKILL.md says to combine them.
SCORE_GAP = 0.25


def die(msg):
    print(f"jev.py: {msg}", file=sys.stderr)
    sys.exit(2)


def backend_for(model, via):
    return via or ("openrouter" if model.startswith("typesafe/") else "requesty")


def key(backend):
    env, path = KEYS[backend]
    if k := os.environ.get(env):
        return k
    try:
        r = subprocess.run(["pass", path], capture_output=True, text=True)
    except FileNotFoundError:
        die(f"no key: set {env} (pass is not installed)")
    if r.returncode or not r.stdout.strip():
        die(f"no key: set {env} or store it at `pass {path}`")
    return r.stdout.splitlines()[0]


def load_image(path):
    """-> (data URL, decoded size); ValueError on a missing file or a type Clef rejects."""
    try:
        with open(path, "rb") as f:
            data = f.read()
    except OSError as e:
        raise ValueError(f"image {path}: {e.strerror}")
    if data.startswith(b"\x89PNG\r\n\x1a\n"): mime = "image/png"
    elif data.startswith(b"\xff\xd8\xff"): mime = "image/jpeg"
    elif data[:4] == b"RIFF" and data[8:12] == b"WEBP": mime = "image/webp"
    else: raise ValueError(f"image {path}: not PNG, JPEG or WebP")
    return f"data:{mime};base64,{base64.b64encode(data).decode()}", len(data)


def limit_problems(sizes):
    p = []
    if len(sizes) > MAX_IMAGES: p.append(f"{len(sizes)} images > {MAX_IMAGES}")
    if any(s > MAX_IMAGE for s in sizes): p.append(f"an image > {MAX_IMAGE >> 20} MiB")
    if sum(sizes) > MAX_IMAGES_TOTAL: p.append(f"images total > {MAX_IMAGES_TOTAL >> 20} MiB")
    return p


def wire(backend, model, state, questions, images, provider):
    """-> (url, body) for one request."""
    # Measured on 8 decoy items: Clef follows `item.x` paths into JSON text as well as into a
    # native state object, so state can be flattened where the transport needs text.
    text = state if isinstance(state, str) else json.dumps(state, ensure_ascii=False)
    if backend == "openrouter":
        if images:
            # OpenRouter rejects Cloudflare's top-level `images`; it wants them as parts of a state array.
            state = [{"type": "text", "text": text}] + [{"type": "image_url", "image_url": {"url": u}} for u in images]
        return OPENROUTER_URL, {"model": model, "provider": provider, "state": state, "questions": questions}
    return REQUESTY_URL, {"model": model, "messages": [{"role": "user", "content": text}],
                          "response_format": {"type": "questions", "questions": questions}}


def normalize(backend, d):
    """Requesty's chat completion -> the /systemone shape {model, answers, usage}."""
    if backend == "openrouter" or "choices" not in d:
        return d
    try:
        answers = json.loads(d["choices"][0]["message"]["content"])
    except (KeyError, IndexError, TypeError, ValueError):
        return d
    u = d.get("usage") or {}
    usage = {"input_tokens": u.get("prompt_tokens"), **({"cost": u["cost"]} if "cost" in u else {})}
    return {"model": d.get("model"), "answers": answers, "usage": usage}


def call(backend, url, body, questions, k, stop):
    """POST with retries; returns {model, answers, usage}, or {"error": ...}. Sets `stop` on auth/billing errors."""
    if stop.is_set():
        return {"error": {"code": "skipped", "message": "batch stopped after an auth/billing error"}}
    data = json.dumps(body).encode()
    for attempt in range(TRIES):
        req = urllib.request.Request(url, data, {"Authorization": f"Bearer {k}", "Content-Type": "application/json"})
        retry_after = None
        try:
            with urllib.request.urlopen(req, timeout=TIMEOUT) as r:
                d = json.load(r)
        except urllib.error.HTTPError as e:
            retry_after = e.headers.get("Retry-After")
            try: d = json.load(e)
            except ValueError: d = {}
            if not isinstance(d.get("error"), dict):
                # A non-JSON 403 comes from Cloudflare, which users saw trigger on one item's content;
                # a bad key gets a JSON error. Fail the item, not the batch.
                code = "waf_403" if e.code == 403 else e.code
                d = {"error": {"code": code, "message": str(e)}}
            d["error"].setdefault("code", e.code)
        except (urllib.error.URLError, TimeoutError, ValueError) as e:
            d = {"error": {"code": 0, "message": f"transport: {e}"}}
        if isinstance(d.get("error"), dict):
            d["error"].setdefault("code", "error")
        code = (d.get("error") or {}).get("code")
        if code is None:
            d = normalize(backend, d)
            if "answers" not in d:
                return {"error": {"code": "no_answers", "message": f"response without answers: {json.dumps(d)[:200]}"}}
            if missing := sorted(set(questions) - set(d["answers"])):
                return {"error": {"code": "missing_answers", "message": f"no answer for {missing}"}}
            return d
        meta = d["error"].get("metadata") or {}
        if code == 402 and meta.get("limit_source") == "openrouter_in_flight_budget":
            pass
        elif code == 403 and meta:
            return d
        elif code in FATAL:
            stop.set()
            return d
        elif code not in RETRY and code != 0:
            return d
        if attempt == TRIES - 1:
            break
        wait = 2 ** attempt + random.random()
        if retry_after:
            try: wait = float(retry_after)
            except ValueError: pass  # HTTP-date form: keep the backoff
            if wait > RETRY_AFTER_MAX:
                break
        time.sleep(wait)
    return d


def disagree(questions, by_model):
    """Question ids on which the models' answers differ."""
    out = []
    for qid, q in questions.items():
        a = [ans[qid] for ans in by_model.values()]
        t = q.get("type")
        if t == "noul":
            hit = len({x["noul"] >= 0.5 for x in a}) > 1
        elif t == "choice":
            hit = len({x["choice"] for x in a}) > 1
        elif t == "score" and len(q.get("criteria") or []) > 1:
            s = [x["score"] / (len(q["criteria"]) - 1) for x in a]
            hit = max(s) - min(s) >= SCORE_GAP
        else:
            continue
        if hit:
            out.append(qid)
    return out


def elide(body):
    """Copy of body with base64 image data replaced by its size, for --dry-run."""
    def walk(x):
        if isinstance(x, dict): return {k: walk(v) for k, v in x.items()}
        if isinstance(x, list): return [walk(v) for v in x]
        if isinstance(x, str) and x.startswith("data:image/") and ";base64," in x:
            head, b64 = x.split(",", 1)
            return f"{head},<{len(b64) * 3 // 4} bytes>"
        return x
    return walk(body)


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
    ap.add_argument("--model", default=MODEL,
                    help="model id, alias (%s) or a comma list to compare; default: %%(default)s"
                    % ", ".join(f"{a}={m}" for a, m in ALIASES.items()))
    ap.add_argument("--via", choices=sorted(KEYS), help="force a backend (default: typesafe/* openrouter, else requesty)")
    ap.add_argument("--image", action="append", default=[], metavar="FILE",
                    help="PNG/JPEG/WebP sent with every request (repeatable; cloudflare/clef* via openrouter with --no-zdr)")
    ap.add_argument("--no-zdr", action="store_true",
                    help="OpenRouter without zero data retention (no-training still enforced); needed for Clef there")
    ap.add_argument("-j", "--jobs", type=int, default=JOBS, help="concurrent items (default: %(default)s)")
    ap.add_argument("--dry-run", action="store_true", help="print {url, body} per request as JSONL and exit; no key, no network")
    a = ap.parse_args()

    models = [ALIASES.get(m.strip(), m.strip()) for m in a.model.split(",") if m.strip()]
    if len(set(models)) != len(models):
        ap.error("--model lists a model twice")

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
        jobs = []  # (state, per-item image paths or None)
        for it in load_items(a.each):
            paths = None
            if isinstance(it, dict) and "_images" in it:
                it = dict(it)
                paths = it.pop("_images")
            jobs.append(({"item": it, **({"context": ctx} if ctx is not None else {})}, paths))
    else:
        if sys.stdin.isatty():
            ap.error("pipe a request JSON on stdin, or use --each")
        try: b = json.load(sys.stdin)
        except ValueError as e: die(f"stdin: invalid JSON: {e}")
        if not isinstance(b, dict) or "state" not in b or not b.get("questions"):
            die("stdin: expected {state, questions}")
        if "model" in b:
            models = [ALIASES.get(b["model"], b["model"])]
        qs = b["questions"]
        jobs = [(b["state"], None)]

    backends = {m: backend_for(m, a.via) for m in models}
    if a.image or any(p is not None for _, p in jobs):
        # Jev rejects images, and sference declares image_input unsupported for its Clef (Requesty
        # answers 400 or silently drops them). Refuse before paying for a comparison where only
        # one side saw the picture.
        if bad := [m for m in models if backends[m] != "openrouter" or not m.startswith("cloudflare/clef")]:
            die(f"images need --model cloudflare/clef[-flash] --via openrouter --no-zdr; not supported by: {', '.join(bad)}")
        if not a.no_zdr:
            die("images need --no-zdr: OpenRouter has no zero-data-retention endpoint for Clef")
    provider = PROVIDER_NO_ZDR if a.no_zdr else PROVIDER
    shared, shared_sizes = [], []
    for p in a.image:
        try: u, n = load_image(p)
        except ValueError as e: die(str(e))
        shared.append(u); shared_sizes.append(n)
    if problems := limit_problems(shared_sizes):
        die("--image: " + "; ".join(problems) + " (Clef's limits; every request would fail)")

    def images_for(i, paths):
        """-> (data URLs, local error or None). Over-limit items are sent anyway: the server
        decides, and the rest of the batch still succeeds if it refuses."""
        if paths is None:
            return shared, None
        if not isinstance(paths, list) or not all(isinstance(p, str) for p in paths):
            return None, {"code": "image", "message": "_images must be a list of file paths"}
        urls, sizes = list(shared), list(shared_sizes)
        for p in paths:
            try: u, n = load_image(p)
            except ValueError as e: return None, {"code": "image", "message": str(e)}
            urls.append(u); sizes.append(n)
        if problems := limit_problems(sizes):
            print(f"jev.py: item {i}: {'; '.join(problems)}; sending anyway", file=sys.stderr)
        return urls, None

    if a.dry_run:
        for i, (state, paths) in enumerate(jobs):
            imgs, err = images_for(i, paths)
            for m in models:
                if err:
                    print(json.dumps({"i": i, "model": m, "error": err}, ensure_ascii=False))
                    continue
                url, body = wire(backends[m], m, state, qs, imgs, provider)
                print(json.dumps({"url": url, "body": elide(body)}, ensure_ascii=False))
        return 0

    ks = {b: key(b) for b in set(backends.values())}
    stop, t0 = threading.Event(), time.monotonic()

    def run(job):
        i, (state, paths) = job
        imgs, err = images_for(i, paths)
        out = {}
        for m in models:
            if err:
                out[m] = {"error": err}
                continue
            url, body = wire(backends[m], m, state, qs, imgs, provider)
            out[m] = call(backends[m], url, body, qs, ks[backends[m]], stop)
        return out

    results, compare = [], len(models) > 1
    n_dis, dis_by_q = 0, {q: 0 for q in qs}
    # Print each line as it arrives (map keeps input order) so an interrupted or timed-out
    # batch leaves its paid answers behind.
    with ThreadPoolExecutor(a.jobs) as ex:
        for i, by_model in enumerate(ex.map(run, enumerate(jobs))):
            results.append(by_model)
            errs = {m: r["error"] for m, r in by_model.items() if "error" in r}
            line = {"i": i} if a.each else {}
            if not compare:
                r = by_model[models[0]]
                if not a.each:
                    print(json.dumps(r, indent=2, ensure_ascii=False))
                    continue
                line.update({"error": r["error"]} if "error" in r else {"answers": r["answers"]})
            else:
                line["answers"] = {m: r["answers"] for m, r in by_model.items() if "answers" in r}
                if errs:
                    line["error"] = errs
                else:
                    line["disagree"] = d = disagree(qs, line["answers"])
                    n_dis += bool(d)
                    for q in d: dis_by_q[q] += 1
            print(json.dumps(line, ensure_ascii=False, **({} if a.each else {"indent": 2})), flush=True)

    flat = [(m, r) for by_model in results for m, r in by_model.items()]
    cost, estimated = 0.0, False
    for m, r in flat:
        u = r.get("usage") or {}
        if "cost" in u:
            cost += u["cost"]
        elif u.get("input_tokens") and m in PRICE:
            cost += u["input_tokens"] * PRICE[m]
            estimated = True
    failed = sum(1 for _, r in flat if "error" in r)
    served = ",".join(sorted({r["model"] for _, r in flat if r.get("model")})) or "?"
    print(f"jev.py: {len(flat)} req, {failed} failed, ${cost:.6f}{' (est.)' if estimated else ''}, "
          f"{time.monotonic() - t0:.2f}s, model={served}", file=sys.stderr)
    if compare:
        compared = sum(1 for by_model in results if not any("error" in r for r in by_model.values()))
        print(f"jev.py: compare: {n_dis}/{compared} items disagree ("
              + ", ".join(f"{q} {n}" for q, n in dis_by_q.items()) + ")", file=sys.stderr)
    if stop.is_set():
        print("jev.py: stopped on an auth/billing error (see the error lines)", file=sys.stderr)
        return 2
    return 1 if failed else 0


if __name__ == "__main__":
    sys.exit(main())
