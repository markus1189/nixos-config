#!/usr/bin/env nix
#! nix shell nixpkgs#python3 nixpkgs#imagemagick --command python
"""Ask decision models (Jev, Clef) typed questions: state + questions -> calibrated answers.

  jev.py < request.json                         one request {state, questions[, model]}; pretty JSON out
  jev.py --each items.jsonl --questions q.json  same questions per item; state = {"item": <line>}; JSONL out
  jev.py --model jev,clef ...                   compare mode: every item to every model, `disagree` per line
  jev.py --image a.png --model cloudflare/clef --via openrouter --no-zdr ...
                                                attach images; per item: "_images": [paths] in the item.
                                                Images are shrunk to fit (reported per image); --no-downscale sends them as is
  jev.py --dry-run ...                          print {url, body} per request (base64 elided); no key, no network

Backends: typesafe/* (also ~typesafe/*) -> OpenRouter /systemone with zero data retention; anything
else -> Requesty EU chat completions with a "questions" response_format. --via overrides.
TypeSafe's own API (--via typesafe, typesafe/* only; the default for ~typesafe/jev-preview, which no
other backend serves) needs --no-zdr, as does Jev via Requesty.
Keys: $OPENROUTER_API_KEY or `pass api/openrouter/jev-skill`; $REQUESTY_API_KEY or
`pass api/requesty/systemone`; $TYPESAFE_API_KEY or `pass api/typesafe-ai/playground`.
$OPENROUTER_BASE_URL, $REQUESTY_BASE_URL and $TYPESAFE_BASE_URL replace the API roots.
Summary (requests, failures, cost, models) on stderr.
Exit: 0 ok, 1 some requests failed, 2 usage/input/auth/billing or a refusal.
"""
import argparse, base64, http.client, json, math, os, random, subprocess, sys, threading, time
import urllib.error, urllib.request
from concurrent.futures import ThreadPoolExecutor

# Pinned: thresholds tuned on one version don't carry over. `~typesafe/jev-latest` tracks releases.
MODEL = "typesafe/jev-1.13"
# sference/clef is the only Clef endpoint approved for the systemone key; cloudflare/* needs a
# Model Library approval in the Requesty console first.
ALIASES = {"jev": MODEL, "clef": "sference/clef"}
OPENROUTER_URL = os.environ.get("OPENROUTER_BASE_URL", "https://openrouter.ai/api/v1").rstrip("/") + "/systemone"
# The org enforces EU data residency: router.requesty.ai answers 403 for every request.
REQUESTY_URL = os.environ.get("REQUESTY_BASE_URL", "https://router.eu.requesty.ai/v1").rstrip("/") + "/chat/completions"
# ZDR on TypeSafe's own API is enterprise-only; the standard account's DPA retains data "as long as
# necessary". It rejects an OpenRouter `provider` field with a bare 400.
TYPESAFE_URL = os.environ.get("TYPESAFE_BASE_URL", "https://api.typesafe.ai/v1").rstrip("/") + "/systemone"
# OpenRouter's pinned id; TypeSafe answers "Unknown model" to the unsuffixed `jev-1.13`.
TYPESAFE_IDS = {MODEL: "jev-1.13.0"}
# OpenRouter answers 400 "does not exist" for these (checked 2026-10-08).
TYPESAFE_ONLY = {"typesafe/jev-preview"}
KEYS = {"openrouter": ("OPENROUTER_API_KEY", "api/openrouter/jev-skill"),
        "requesty": ("REQUESTY_API_KEY", "api/requesty/systemone"),
        "typesafe": ("TYPESAFE_API_KEY", "api/typesafe-ai/playground")}
# Zero data retention, no data collection: OpenRouter's TypeSafe endpoint qualifies, so the promise is enforced
# per request rather than assumed from the provider listing. OpenRouter has no ZDR endpoint for
# Clef (404 with zdr), and Requesty takes no per-request equivalent.
PROVIDER = {"zdr": True, "data_collection": "deny"}
PROVIDER_NO_ZDR = {"data_collection": "deny"}
# USD per input token (output is free); only for estimating when usage.cost is absent, which the
# response schema allows and TypeSafe's own API always does. Keyed by family(); every Jev id
# costs the same (docs.typesafe.ai/models.md, 2026-10-08).
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
SFERENCE_MAX_QUESTIONS = 16  # measured: 17 fail with a bare "Validation failed" 400
MAX_IMAGES = 4  # Clef's documented limit
# Measured on cloudflare/clef via OpenRouter: billed tokens stop growing at 1024 px (a 2048 px
# image costs the same), and the request is refused with 413 before inference somewhere between
# 376,087 B of image (passed) and 402,019 B (failed), PNG and JPEG alike. The server estimates
# bytes/3 tokens, so the cap is probably 131,072 tokens (inferred) and the state text counts too:
# the budget stays under the last size that passed and charges text at ~4 bytes/token (inferred).
# The documented 4 MiB per image does not apply on this route.
MAX_SIDE, REQUEST_BYTES = 1024, 360 << 10
SHRINK, MIN_SIDE, JPEG_QUALITY = 0.75, 256, 85
# Compare mode: scores are divided by their top level index first, as SKILL.md says to combine them.
SCORE_GAP = 0.25
ANSWER_FIELD = {"noul": (int, float), "choice": str, "score": (int, float)}


def die(msg):
    print(f"jev.py: {msg}", file=sys.stderr)
    sys.exit(2)


def family(model):
    """Provider prefix of a model id, case- and `~`-insensitive: `typesafe/`, `sference/`, `cloudflare/clef`..."""
    return model.lstrip("~").lower()


def is_typesafe(model):
    return family(model).startswith("typesafe/")


def backend_for(model, via):
    if via:
        return via
    if family(model) in TYPESAFE_ONLY:
        return "typesafe"
    return "openrouter" if is_typesafe(model) else "requesty"


def typesafe_id(model):
    m = family(model)
    return TYPESAFE_IDS.get(m, m.removeprefix("typesafe/"))


def price(model):
    return PRICE.get(family(model)) or (PRICE[MODEL] if is_typesafe(model) else None)


def provider_for(model, no_zdr):
    # Only OpenRouter takes a provider field; there Jev keeps ZDR even when --no-zdr admits Clef.
    return PROVIDER_NO_ZDR if no_zdr and not is_typesafe(model) else PROVIDER


def key(backend):
    env, path = KEYS[backend]
    if k := os.environ.get(env, "").strip():
        source = f"${env}"
    else:
        try:
            r = subprocess.run(["pass", path], capture_output=True, text=True)
        except FileNotFoundError:
            die(f"no key: set {env} (pass is not installed)")
        lines = r.stdout.splitlines()
        if r.returncode or not lines or not (k := lines[0].strip()):
            die(f"no key: set {env} or store it at `pass {path}`")
        source = f"`pass {path}`"
    # urllib refuses such a header, which would otherwise surface as a retried transport error per item.
    if not k.isprintable() or not k.isascii():
        die(f"key from {source} contains non-printable or non-ASCII characters")
    return k


def magick(args, data):
    try:
        r = subprocess.run(["magick", *args], input=data, capture_output=True)
    except FileNotFoundError:
        raise ValueError("imagemagick (magick) not found; pass --no-downscale")
    if r.returncode:
        raise ValueError(f"magick: {r.stderr.decode(errors='replace').strip()[:200]}")
    return r.stdout


def size_of(data):
    # `-[0]`: only the first frame, else an animation prints one size per frame.
    w, h = magick(["-[0]", "-format", "%w %h", "info:"], data).decode().split()
    return int(w), int(h)


def describe(data, fmt):
    w, h = size_of(data)
    return f"{w}x{h} {fmt} {len(data) / 1024:.0f} KiB"


def load_image(path, budget, downscale):
    """-> (data URL, bytes sent, note on what was changed or None); ValueError on a missing
    file, a type Clef rejects, or a failed conversion."""
    try:
        with open(path, "rb") as f:
            data = f.read()
    except OSError as e:
        raise ValueError(f"image {path}: {e.strerror}")
    if data.startswith(b"\x89PNG\r\n\x1a\n"): fmt = "png"
    elif data.startswith(b"\xff\xd8\xff"): fmt = "jpeg"
    elif data[:4] == b"RIFF" and data[8:12] == b"WEBP": fmt = "webp"
    else: raise ValueError(f"image {path}: not PNG, JPEG or WebP")
    out, out_fmt, note = data, fmt, None
    if downscale:
        try:
            w, h = size_of(data)
            side = min(max(w, h), MAX_SIDE)
            # Always re-encoded: -strip drops EXIF (GPS, camera, owner), which would otherwise leave
            # the machine with every photo; -auto-orient first, since stripping loses the rotation.
            out = magick(["-[0]", "-auto-orient", "-strip", "-resize", f"{side}x{side}>", f"{fmt}:-"], data)
            # Transparency is flattened onto white: JPEG has no alpha channel. Re-encode at least
            # once even when small: a few pixels can still carry megabytes of trailing data.
            while len(out) > budget:
                out_fmt = "jpeg"
                out = magick(["-[0]", "-auto-orient", "-strip", "-resize", f"{side}x{side}>", "-background", "white",
                              "-alpha", "remove", "-alpha", "off", "-quality", str(JPEG_QUALITY), "jpeg:-"], data)
                if side < MIN_SIDE:
                    break
                side = int(side * SHRINK)
            if side < max(w, h) or out_fmt != fmt:
                note = f"{path}: {describe(data, fmt)} -> {describe(out, out_fmt)}"
        except ValueError as e:
            raise ValueError(f"image {path}: {e}")
    return f"data:image/{out_fmt};base64,{base64.b64encode(out).decode()}", len(out), note


def text_cost(text_len):
    """State text expressed in image bytes against the same 413 budget (inferred ratio)."""
    return text_len * 3 // 4


def limit_problems(sizes, text_len=0):
    p, total = [], sum(sizes) + text_cost(text_len)
    if len(sizes) > MAX_IMAGES: p.append(f"{len(sizes)} images > {MAX_IMAGES}")
    if total > REQUEST_BYTES: p.append(f"images plus state ≈{total >> 10} KiB > {REQUEST_BYTES >> 10} KiB")
    return p


def state_text(state):
    return state if isinstance(state, str) else json.dumps(state, ensure_ascii=False)


def sent_len(state):
    """Bytes the state text occupies in the request body, where json.dumps escapes non-ASCII."""
    return len(json.dumps(state_text(state))) - 2


def wire(backend, model, state, questions, images, provider):
    """-> (url, body) for one request."""
    # Measured on 8 decoy items: Clef follows `item.x` paths into JSON text as well as into a
    # native state object, so state can be flattened where the transport needs text.
    text = state_text(state)
    if backend == "openrouter":
        if images:
            # OpenRouter rejects Cloudflare's top-level `images`; it wants them as parts of a state array.
            state = [{"type": "text", "text": text}] + [{"type": "image_url", "image_url": {"url": u}} for u in images]
        return OPENROUTER_URL, {"model": model, "provider": provider, "state": state, "questions": questions}
    if backend == "typesafe":
        return TYPESAFE_URL, {"model": typesafe_id(model), "state": state, "questions": questions}
    # Only reached with images under --do-it-anyway: Requesty documents image_url parts for cloudflare/clef.
    content = [{"type": "text", "text": text}] + [{"type": "image_url", "image_url": {"url": u}} for u in images] \
        if images else text
    return REQUESTY_URL, {"model": model, "messages": [{"role": "user", "content": content}],
                          "response_format": {"type": "questions", "questions": questions}}


def normalize(backend, d):
    """Requesty's chat completion -> the /systemone shape {model, answers, usage}."""
    if backend == "openrouter" or "choices" not in d:
        return d
    try:
        content = d["choices"][0]["message"]["content"]
        answers = content if isinstance(content, dict) else json.loads(content)
    except (KeyError, IndexError, TypeError, ValueError):
        return d
    if not isinstance(answers, dict):
        return d
    u = d.get("usage") if isinstance(d.get("usage"), dict) else {}
    usage = {"input_tokens": u.get("prompt_tokens"), **({"cost": u["cost"]} if is_number(u.get("cost")) else {})}
    return {"model": d.get("model"), "answers": answers, "usage": usage}


def is_number(x):
    return isinstance(x, (int, float)) and not isinstance(x, bool) and math.isfinite(x)


def bad_answers(questions, answers):
    """Question ids whose answer lacks the field its type promises (noul/choice/score)."""
    bad = []
    for qid, q in questions.items():
        want = ANSWER_FIELD.get(q.get("type")) if isinstance(q, dict) else None
        a = answers.get(qid)
        v = a.get(q["type"]) if isinstance(a, dict) and want else None
        if not isinstance(a, dict) or (want and (not isinstance(v, want) or isinstance(v, bool)
                                                 or (want is not str and not is_number(v)))):
            bad.append(qid)
    return bad


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
            except (ValueError, OSError, http.client.HTTPException): d = None
            if isinstance(d, dict) and "error" not in d and isinstance(det := d.get("detail"), (dict, str)):
                # TypeSafe's error shape: {"detail": {"error_type", "message"}}; a string detail is
                # FastAPI's default (e.g. a 403 "Not authenticated"), which must not pass as a WAF block.
                det = det if isinstance(det, dict) else {"message": det}
                d = {"error": {"code": e.code, "message": str(det.get("message") or json.dumps(det))[:300],
                               **({"provider_code": det["error_type"]} if det.get("error_type") else {})}}
            if not isinstance(d, dict) or not isinstance(d.get("error"), dict):
                # A non-JSON 403 comes from Cloudflare, which users saw trigger on one item's content;
                # a bad key gets a JSON error. Fail the item, not the batch.
                code = "waf_403" if e.code == 403 else e.code
                d = {"error": {"code": code, "message": str(e) if d is None else json.dumps(d)[:300]}}
            else:
                # Classify by HTTP status: a null or string body code (OpenAI style) would dodge
                # RETRY and FATAL. A differing body code is kept for the reader.
                if d["error"].get("code") not in (None, e.code):
                    d["error"]["provider_code"] = d["error"]["code"]
                d["error"]["code"] = e.code
        except (OSError, http.client.HTTPException, ValueError) as e:
            # OSError covers URLError, timeouts and resets; HTTPException a truncated body.
            d = {"error": {"code": 0, "message": f"transport: {e!r}"}}
        if not isinstance(d, dict):
            return {"error": {"code": "bad_response", "message": f"not a JSON object: {json.dumps(d)[:200]}"}}
        if d.get("error") is None:
            d.pop("error", None)
        elif not isinstance(d["error"], dict):
            d["error"] = {"code": "error", "message": str(d["error"])[:300]}
        if "error" not in d:
            d = normalize(backend, d)
            answers = d.get("answers")
            if not isinstance(answers, dict):
                return {"error": {"code": "no_answers", "message": f"response without answers: {json.dumps(d)[:200]}"}}
            if missing := sorted(set(questions) - set(answers)):
                return {"error": {"code": "missing_answers", "message": f"no answer for {missing}"}}
            if bad := bad_answers(questions, answers):
                return {"error": {"code": "bad_answer", "message": f"unexpected answer shape for {bad}: "
                                                                  f"{json.dumps({q: answers[q] for q in bad})[:200]}"}}
            return d
        code = d["error"].get("code")
        if not isinstance(code, (int, str)) or isinstance(code, bool):
            code = d["error"]["code"] = "error"  # null, list, dict: not classifiable
        meta = d["error"].get("metadata") or {}
        if code == 402 and isinstance(meta, dict) and meta.get("limit_source") == "openrouter_in_flight_budget":
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
            try: v = float(retry_after)
            except ValueError: v = None  # HTTP-date form: keep the backoff
            if v is not None and v >= 0:  # NaN fails this, infinity the cap below
                if v > RETRY_AFTER_MAX:
                    break
                wait = v
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
            return f"{head},<{len(b64) * 3 // 4 - b64.count('=')} bytes>"
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
    try:
        for n, line in enumerate(src, 1):
            if not line.strip():
                continue
            try: items.append(json.loads(line))
            except ValueError as e: die(f"--each {path}:{n}: invalid JSON: {e}")
    except UnicodeDecodeError as e:
        die(f"--each {path}: not UTF-8: {e}")
    if not items:
        die(f"--each {path}: no items")
    return items


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--each", metavar="JSONL", help="one item per line (- for stdin); one request per item")
    ap.add_argument("--questions", metavar="JSON", help="questions map {id: {type, instructions, criteria}}; required with --each")
    ap.add_argument("--context", metavar="FILE", help="JSON or text added to every item's state as state.context (--each only)")
    ap.add_argument("--model", help="model id, alias (%s) or a comma list to compare; default: %s"
                    % (", ".join(f"{a}={m}" for a, m in ALIASES.items()), MODEL))
    ap.add_argument("--via", choices=sorted(KEYS),
                    help="force a backend (default: typesafe/jev-preview typesafe, other typesafe/* openrouter, "
                         "else requesty); typesafe takes only typesafe/* and needs --no-zdr")
    ap.add_argument("--image", action="append", default=[], metavar="FILE",
                    help="PNG/JPEG/WebP sent with every request (repeatable; cloudflare/clef* via openrouter with --no-zdr)")
    ap.add_argument("--no-downscale", action="store_true",
                    help=f"send images unchanged instead of shrinking them to {MAX_SIDE} px and {REQUEST_BYTES >> 10} KiB per request")
    ap.add_argument("--do-it-anyway", action="store_true",
                    help="send despite refusals based on provider capabilities or limits that may have gone stale "
                         "(images per model, the --no-zdr requirement, image count and bytes, sference's question cap, "
                         "jev-preview off TypeSafe's API); never drops ZDR, never sends images to TypeSafe's API, which has "
                         "no image input")
    ap.add_argument("--no-zdr", action="store_true",
                    help="accept losing zero data retention: Clef via OpenRouter (no-training still enforced there), "
                         "Jev via requesty or typesafe")
    ap.add_argument("-j", "--jobs", type=int, default=JOBS, help="concurrent items (default: %(default)s)")
    ap.add_argument("--dry-run", action="store_true", help="print {url, body} per request as JSONL and exit; no key, no network")
    a = ap.parse_args()
    if a.jobs < 1:
        ap.error("--jobs must be at least 1")

    def resolve(spec, what="--model"):
        ms = [ALIASES.get(m.strip(), m.strip()) for m in spec.split(",") if m.strip()]
        if not ms:
            die(f"{what} is empty")
        if bad := [m for m in ms if not family(m) or m.endswith("/")]:
            die(f"{what} has an empty model name: {', '.join(bad)}")
        return ms

    models = resolve(MODEL if a.model is None else a.model)

    if a.each:
        if not a.questions:
            ap.error("--each needs --questions")
        qs = load_json(a.questions, "--questions")
        ctx, has_ctx = None, bool(a.context)
        if a.context:
            try: raw = open(a.context).read()
            except (OSError, UnicodeDecodeError) as e: die(f"--context: {e}")
            try: ctx = json.loads(raw)
            except ValueError: ctx = raw
        jobs = []  # (state, per-item image paths or None)
        for it in load_items(a.each):
            paths = None
            if isinstance(it, dict) and "_images" in it:
                it = dict(it)
                paths = it.pop("_images")
                # [] means no images; null (e.g. jq on a missing field) must not pass as that.
                paths = None if paths == [] else (False if paths is None else paths)
            jobs.append(({"item": it, **({"context": ctx} if has_ctx else {})}, paths))
    else:
        if a.questions or a.context:
            ap.error("--questions and --context apply only with --each; put them in the request JSON")
        if sys.stdin.isatty():
            ap.error("pipe a request JSON on stdin, or use --each")
        try: b = json.load(sys.stdin)
        except ValueError as e: die(f"stdin: invalid JSON: {e}")
        if not isinstance(b, dict) or "state" not in b or not b.get("questions"):
            die("stdin: expected {state, questions}")
        qs = b["questions"]
        if "model" in b:
            if not isinstance(b["model"], str):
                die("stdin: model must be a string")
            mb = resolve(b["model"], "stdin model")
            if a.model is not None and [m.lower() for m in models] != [m.lower() for m in mb]:
                die(f"model given twice: --model {a.model} and the request's {b['model']!r}")
            models = mb
        jobs = [(b["state"], None)]
    if not isinstance(qs, dict) or not qs or not all(isinstance(q, dict) and isinstance(q.get("type"), str) for q in qs.values()):
        die("questions must be a non-empty map {id: {type: noul|choice|score, instructions, ...}}")

    backends = {m: backend_for(m, a.via) for m in models}
    if a.via == "typesafe" and (nt := [m for m in models if not is_typesafe(m)]):
        die(f"--via typesafe serves only typesafe/* models, not: {', '.join(nt)}")
    # By what is sent: OpenRouter may tell `~` aliases from pinned ids, TypeSafe maps both to one name.
    sent = [(backends[m], typesafe_id(m) if backends[m] == "typesafe" else m.lower()) for m in models]
    if len(set(sent)) != len(sent):
        die("--model lists a model twice (after mapping to what each backend is sent)")
    forced = []  # (message, models it concerns, images possibly ignored)

    def refuse(msg, concerned, images_ignored=False):
        """Refusals resting on provider facts that can change; --do-it-anyway sends regardless."""
        if not a.do_it_anyway:
            die(f"{msg} (if this check looks stale, retry with --do-it-anyway)")
        print(f"jev.py: --do-it-anyway: sending although {msg}", file=sys.stderr)
        forced.append((msg, set(concerned), images_ignored))

    if off := [m for m in models if family(m) in TYPESAFE_ONLY and backends[m] != "typesafe"]:
        refuse(f"{', '.join(off)} exists only on TypeSafe's own API; drop --via or use --via typesafe", off)
    sref = [m for m in models if family(m).startswith("sference/")]
    if sref and len(qs) > SFERENCE_MAX_QUESTIONS:
        refuse(f"{len(qs)} questions > {SFERENCE_MAX_QUESTIONS}, sference's cap; split them into several runs", sref)
    if a.image or any(p is not None for _, p in jobs):
        if ti := [m for m in models if backends[m] == "typesafe"]:
            die(f"TypeSafe's own API takes no images: {', '.join(ti)}")
        # Jev rejects images, and sference declares image_input unsupported for its Clef (Requesty
        # answers 400 or silently drops them). Refuse before paying for a comparison where only
        # one side saw the picture.
        if bad := [m for m in models if backends[m] != "openrouter" or not family(m).startswith("cloudflare/clef")]:
            refuse(f"images need --model cloudflare/clef[-flash] --via openrouter --no-zdr; not supported by: {', '.join(bad)}",
                   bad, images_ignored=True)

    if not a.no_zdr and (jr := [m for m in models if is_typesafe(m) and backends[m] != "openrouter"]):
        # A privacy guard, not a provider fact: --do-it-anyway does not lift it.
        die(f"{', '.join(f'{m} via {backends[m]}' for m in jr)} loses zero data retention, which only OpenRouter enforces; "
            "pass --no-zdr to accept that")
    if not a.no_zdr and (orc := [m for m in models if backends[m] == "openrouter" and not is_typesafe(m)]):
        refuse(f"{', '.join(orc)} via OpenRouter needs --no-zdr: it has no zero-data-retention endpoint for Clef", orc)

    # The byte budget is shared evenly by every image a request can carry, after the longest
    # state's text, which every shared image travels with.
    most_per_item = max((len(p) for _, p in jobs if isinstance(p, list)), default=0)
    longest_text = max(sent_len(s) for s, _ in jobs)
    shared, shared_sizes, shared_notes = [], [], []
    for p in a.image:
        budget = (REQUEST_BYTES - text_cost(longest_text)) // (len(a.image) + most_per_item)
        try: u, n, note = load_image(p, budget, not a.no_downscale)
        except ValueError as e: die(str(e))
        shared.append(u); shared_sizes.append(n)
        if note:
            shared_notes.append(note)
            print(f"jev.py: downscaled {note}", file=sys.stderr)
    if problems := limit_problems(shared_sizes):
        refuse("--image: " + "; ".join(problems) + " (every request would fail)", models)

    def images_for(i, state, paths):
        """-> (data URLs, local error or None, notes on downscaled per-item images). Items still
        over the limits are sent anyway: the server decides, and the rest of the batch survives."""
        text_len = sent_len(state)
        if paths is None:
            if shared and (problems := limit_problems(shared_sizes, text_len)):
                print(f"jev.py: item {i}: {'; '.join(problems)}; sending anyway", file=sys.stderr)
            return shared, None, []
        if not isinstance(paths, list) or not all(isinstance(p, str) for p in paths):
            return None, {"code": "image", "message": "_images must be a list of file paths"}, []
        urls, sizes, notes = list(shared), list(shared_sizes), []
        budget = (REQUEST_BYTES - sum(shared_sizes) - text_cost(text_len)) // len(paths)
        for p in paths:
            try: u, n, note = load_image(p, budget, not a.no_downscale)
            except ValueError as e: return None, {"code": "image", "message": str(e)}, notes
            urls.append(u); sizes.append(n)
            if note: notes.append(note)
        if problems := limit_problems(sizes, text_len):
            print(f"jev.py: item {i}: {'; '.join(problems)}; sending anyway", file=sys.stderr)
        return urls, None, notes

    if a.dry_run:
        local_errors = 0
        for i, (state, paths) in enumerate(jobs):
            imgs, err, notes = images_for(i, state, paths)
            for m in models:
                if err:
                    local_errors += 1
                    print(json.dumps({"i": i, "model": m, "error": err}, ensure_ascii=False))
                    continue
                url, body = wire(backends[m], m, state, qs, imgs, provider_for(m, a.no_zdr))
                print(json.dumps({"url": url, "body": elide(body), **({"downscaled": notes} if notes else {})},
                                 ensure_ascii=False))
        return 1 if local_errors else 0

    ks = {b: key(b) for b in set(backends.values())}
    # One per backend: a bad Requesty key must not throw away Jev's answers in a compare run.
    stops, t0 = {b: threading.Event() for b in ks}, time.monotonic()

    def run(job):
        i, (state, paths) = job
        imgs, err, notes = images_for(i, state, paths)
        out = {}
        for m in models:
            if err:
                out[m] = {"error": err}
                continue
            url, body = wire(backends[m], m, state, qs, imgs, provider_for(m, a.no_zdr))
            out[m] = call(backends[m], url, body, qs, ks[backends[m]], stops[backends[m]])
        return out, notes

    results, compare = [], len(models) > 1
    n_dis, dis_by_q, n_item_downscaled = 0, {q: 0 for q in qs}, 0
    # Print each line as it arrives (map keeps input order) so an interrupted or timed-out
    # batch leaves its paid answers behind.
    with ThreadPoolExecutor(a.jobs) as ex:
        for i, (by_model, notes) in enumerate(ex.map(run, enumerate(jobs))):
            results.append(by_model)
            n_item_downscaled += len(notes)
            errs = {m: r["error"] for m, r in by_model.items() if "error" in r}
            line = {"i": i} if a.each else {}
            if not compare:
                r = by_model[models[0]]
                if not a.each:
                    print(json.dumps({**r, **({"downscaled": shared_notes} if shared_notes else {})},
                                     indent=2, ensure_ascii=False))
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
            if notes:
                line["downscaled"] = notes
            print(json.dumps(line, ensure_ascii=False, **({} if a.each else {"indent": 2})), flush=True)

    flat = [(m, r) for by_model in results for m, r in by_model.items()]
    cost, estimated, unknown = 0.0, False, False
    for m, r in flat:
        u = r.get("usage") if isinstance(r.get("usage"), dict) else {}
        if is_number(u.get("cost")):
            cost += u["cost"]
        elif is_number(u.get("input_tokens")) and price(m):
            cost += u["input_tokens"] * price(m)
            estimated = True
        elif "answers" in r:
            unknown = True
    failed = sum(1 for _, r in flat if "error" in r)
    served = ",".join(sorted({str(r["model"]) for _, r in flat if r.get("model")})) or "?"
    print(f"jev.py: {len(flat)} req, {failed} failed, ${cost:.6f}{' (est.)' if estimated else ''}"
          f"{' + unknown' if unknown else ''}, {time.monotonic() - t0:.2f}s, model={served}", file=sys.stderr)
    if n_item_downscaled:
        print(f"jev.py: downscaled {n_item_downscaled} item images (see `downscaled` in their output lines)",
              file=sys.stderr)
    if compare:
        compared = sum(1 for by_model in results if not any("error" in r for r in by_model.values()))
        print(f"jev.py: compare: {n_dis}/{compared} items disagree ("
              + ", ".join(f"{q} {n}" for q, n in dis_by_q.items()) + ")", file=sys.stderr)
    for msg, concerned, images_ignored in forced:
        ok = sum(1 for m, r in flat if m in concerned and "answers" in r)
        if not ok:
            continue
        if images_ignored:
            # Requesty answers sference image requests while silently dropping the images.
            print(f"jev.py: --do-it-anyway: {ok} request(s) answered despite: {msg}. The model may have ignored "
                  "the images; ask a question only the image can answer (e.g. the colour of a solid red PNG) "
                  "before calling the check stale.", file=sys.stderr)
        else:
            print(f"jev.py: --do-it-anyway: {ok} request(s) succeeded despite: {msg}. That check is likely stale; "
                  "fix it in ~/repos/nixos-config/nixos-shared/agent-skills/jev/scripts/jev.py rather than "
                  "forcing again.", file=sys.stderr)
    if any(e.is_set() for e in stops.values()):
        print("jev.py: stopped on an auth/billing error (see the error lines)", file=sys.stderr)
        return 2
    return 1 if failed else 0


if __name__ == "__main__":
    try:
        sys.exit(main())
    except BrokenPipeError:
        # The reader went away (e.g. `| head`): leave quietly instead of a traceback.
        os.dup2(os.open(os.devnull, os.O_WRONLY), sys.stdout.fileno())
        sys.exit(1)
