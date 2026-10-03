"""kagi-search: search Kagi from the shell, via the official API or a session link.

Usage: kagi-search [-n N] [--json] [--lens LENS] [--time PERIOD] [--backend B] QUERY...

Backends (--backend auto tries session first, then api; it also falls back to
api when the session link is rejected or the page can't be parsed):
  session  Scrapes https://kagi.com/html/search with the account's session link,
           covered by the subscription. Kagi prefers the API for scripted use.
           Credential: $KAGI_SESSION_TOKEN, or the file $KAGI_SESSION_FILE
           (default /run/agenix/kagi-session). Either the bare token or the
           whole session link.
  api      POST https://kagi.com/api/v1/search, billed per query ($12/1k).
           Credential: $KAGI_API_KEY, or the file $KAGI_API_KEY_FILE
           (default /run/agenix/kagi-api-key).

Output: text, or with --json a list of {title, url, abstract, date} (ddgr's
field names; date is YYYY-MM-DD, the raw text if unparseable, or null).

Exit status:
  0 results   1 no results   2 usage   3 credential missing or rejected
  4 unexpected response (Kagi changed its markup or API) or internal error
  5 network/HTTP/rate limit

Calls are serialised through a lock file with a minimum gap of
$KAGI_MIN_INTERVAL seconds (default 1). Secrets never reach argv, disk or
error messages.
"""
import argparse
import datetime as dt
import fcntl
import html
import json
import math
import os
import re
import sys
import time
import urllib.parse
from pathlib import Path

import requests
from bs4 import BeautifulSoup

API_URL = "https://kagi.com/api/v1/search"
SESSION_URL = "https://kagi.com/html/search"
USER_AGENT = "kagi-search (personal CLI)"
LENSES = ["academic", "fediverse", "forums", "programming", "small_web", "usenet_archive"]
# session: Kagi's `dr` parameter; api: days back for filters.after
TIME = {"day": ("1", 1), "week": ("2", 7), "month": ("3", 31), "year": ("4", 366)}
CREDENTIALS = {
    "api": ("KAGI_API_KEY", "KAGI_API_KEY_FILE", "/run/agenix/kagi-api-key"),
    "session": ("KAGI_SESSION_TOKEN", "KAGI_SESSION_FILE", "/run/agenix/kagi-session"),
}


class KagiError(Exception):
    exit_code = 5


class NoResults(KagiError):
    exit_code = 1


class CredentialError(KagiError):
    exit_code = 3


class UnexpectedResponse(KagiError):
    exit_code = 4


# --- credentials ---------------------------------------------------------


def read_credential(backend: str, environ=os.environ) -> str | None:
    var, file_var, default_file = CREDENTIALS[backend]
    value = environ.get(var, "").strip()
    if not value:
        path = Path(environ.get(file_var) or default_file)
        try:
            value = path.read_text().strip()
        except FileNotFoundError:
            return None
        except OSError as e:
            raise CredentialError(f"{backend}: cannot read {path}: {e.strerror}") from None
    if not value:
        return None
    if backend == "session" and "://" in value:
        token = urllib.parse.parse_qs(urllib.parse.urlsplit(value).query).get("token")
        if not token:
            raise CredentialError("session: the link has no token= parameter")
        value = token[0]
    return value


def candidates(requested: str, environ=os.environ):
    """Yield (backend, secret, problem) per configured backend, reading credentials lazily.

    A credential that exists but can't be used (unreadable file, link without token=)
    is yielded as a problem, so auto mode can still fall back to the next backend.
    """
    order = ["session", "api"] if requested == "auto" else [requested]
    found = False
    for backend in order:
        try:
            secret = read_credential(backend, environ)
        except CredentialError as e:
            found = True
            yield backend, None, e
            continue
        if secret:
            found = True
            yield backend, secret, None
    if not found:
        where = "; ".join(f"${v} or {environ.get(f) or d}" for v, f, d in (CREDENTIALS[b] for b in order))
        raise CredentialError(f"no credential found ({where})")


def pick_backend(requested: str, environ=os.environ) -> tuple[str, str]:
    """The first usable (backend, secret); the first problem if none is usable."""
    problem = None
    for backend, secret, error in candidates(requested, environ):
        if error is None:
            return backend, secret
        problem = problem or error
    raise problem


# --- rate gap ------------------------------------------------------------


class RateGap:
    """Serialise calls across processes and keep `interval` seconds between them."""

    def __init__(self, interval: float, path: Path | None = None):
        runtime = os.environ.get("XDG_RUNTIME_DIR")
        self.interval = interval
        self.path = path or (Path(runtime) / "kagi-search.lock" if runtime
                             else Path(f"/tmp/kagi-search-{os.getuid()}.lock"))

    def __enter__(self):
        self.fd = os.open(self.path, os.O_RDWR | os.O_CREAT, 0o600)
        fcntl.flock(self.fd, fcntl.LOCK_EX)
        try:
            last = float(os.pread(self.fd, 32, 0) or 0)
        except ValueError:
            last = 0.0
        wait = last + self.interval - time.time()
        # a timestamp from the future (clock stepped back, garbage) must not stall us
        time.sleep(min(self.interval, wait) if math.isfinite(wait) and wait > 0 else 0)
        return self

    def __exit__(self, *exc):
        os.ftruncate(self.fd, 0)
        os.pwrite(self.fd, f"{time.time():.3f}".encode(), 0)
        fcntl.flock(self.fd, fcntl.LOCK_UN)
        os.close(self.fd)


# --- shared helpers ------------------------------------------------------


def redact(msg: str, *secrets: str) -> str:
    for secret in secrets:
        msg = msg.replace(secret, "<REDACTED>")
    return re.sub(r"(token=)[^&\s\"']+", r"\1<REDACTED>", msg)


def squash(s: str) -> str:
    return re.sub(r"\s+", " ", s).strip()


def strip_markup(s: str) -> str:
    """API snippets carry <strong> highlights and entities; session text is already plain."""
    return squash(html.unescape(re.sub(r"<[^>]+>", "", s)))


def normalise_date(raw: str | None, today: dt.date | None = None) -> str | None:
    if not raw:
        return None
    raw = raw.strip()
    today = today or dt.date.today()
    relative = re.fullmatch(r"(today|yesterday|(\d+) (minute|hour|day|week)s? ago)", raw.lower())
    if relative:
        word, count, unit = relative.groups()
        days = {"today": 0, "yesterday": 1}.get(word)
        if days is None:
            days = int(count) * {"minute": 0, "hour": 0, "day": 1, "week": 7}[unit]
        return (today - dt.timedelta(days=days)).isoformat()
    for fmt in ("%Y-%m-%dT%H:%M:%SZ", "%b %d, %Y", "%B %d, %Y"):
        try:
            return dt.datetime.strptime(raw, fmt).date().isoformat()
        except ValueError:
            pass
    return raw


def result(title: str, url: str, abstract: str, date: str | None) -> dict:
    return {"title": squash(title), "url": url, "abstract": squash(abstract), "date": normalise_date(date)}


# --- api backend ---------------------------------------------------------


def api_request(query: str, n: int, lens: str | None, period: str | None,
                today: dt.date | None = None) -> dict:
    body = {"query": query, "limit": n}
    if lens:
        body["lens_id"] = lens
    if period:
        since = (today or dt.date.today()) - dt.timedelta(days=TIME[period][1])
        body["filters"] = {"after": since.isoformat()}
    return body


def parse_api(status: int, text: str) -> list[dict]:
    try:
        doc = json.loads(text)
    except ValueError:
        doc = None
    if not isinstance(doc, dict):
        if status in (401, 403):
            raise CredentialError(f"api: key rejected (HTTP {status})")
        if status != 200:
            raise KagiError(f"api: HTTP {status}" + (", rate limited" if status == 429 else ""))
        raise UnexpectedResponse("api: HTTP 200 but the body is not a JSON object")
    errors = doc.get("errors") or doc.get("error") or []
    if not isinstance(errors, list):
        errors = [errors]
    errors = [e if isinstance(e, dict) else {"message": str(e)} for e in errors]
    if status != 200 or errors:
        detail = "; ".join(f"{e.get('code')}: {e.get('message')}" for e in errors) or "no detail"
        codes = {str(e.get("code") or "") for e in errors}
        # observed: a bad key is HTTP 400 with general.invalid_token, not 401
        if status in (401, 403) or any(c.endswith("invalid_token") for c in codes):
            raise CredentialError(f"api: key rejected (HTTP {status}, {detail})")
        if status == 429:
            raise KagiError(f"api: rate limited (HTTP 429, {detail})")
        raise KagiError(f"api: HTTP {status}, {detail}")
    data = doc.get("data")
    if not isinstance(data, dict) or not isinstance(data.get("search"), list):
        raise UnexpectedResponse("api: response has no data.search list")
    return [result(strip_markup(str(x.get("title") or "")), x["url"], strip_markup(str(x.get("snippet") or "")), x.get("time"))
            for x in data["search"] if isinstance(x, dict) and isinstance(x.get("url"), str)]


def search_api(key: str, query: str, n: int, lens: str | None, period: str | None) -> list[dict]:
    r = requests.post(API_URL, json=api_request(query, n, lens, period), timeout=60,
                      headers={"Authorization": f"Bearer {key}", "User-Agent": USER_AGENT})
    return parse_api(r.status_code, r.text)


# --- session backend -----------------------------------------------------


def session_params(query: str, token: str, lens: str | None, period: str | None) -> dict:
    params = {"q": query, "token": token}
    if lens:
        params["lens"] = lens
    if period:
        params["dr"] = TIME[period][0]
    return params


def parse_session(page: str) -> list[dict]:
    soup = BeautifulSoup(page, "lxml")
    main = soup.select_one("main.results-box")
    if main is None:
        raise UnexpectedResponse("session: no main.results-box in the page, Kagi's markup changed")
    items = main.select(".search-result")
    out, seen = [], set()
    for item in items:
        link = item.select_one("a.__sri_title_link")
        if link is None or not link.get("href") or link["href"] in seen:
            continue
        seen.add(link["href"])
        date = abstract = None
        desc = item.select_one(".__sri-desc")
        if desc is not None:
            stamp = desc.select_one(".__sri-time")
            if stamp is not None:
                date = stamp.get_text(" ", strip=True)
                stamp.decompose()
            for extra in desc.select(".summarize-link"):
                extra.decompose()
            abstract = desc.get_text(" ", strip=True)
        out.append(result(link.get_text(" ", strip=True), link["href"], abstract or "", date))
    if items and not out:
        raise UnexpectedResponse("session: results found but none had a title link, Kagi's markup changed")
    return out


def search_session(token: str, query: str, n: int, lens: str | None, period: str | None) -> list[dict]:
    s = requests.Session()
    s.headers["User-Agent"] = USER_AGENT
    # The token request sets the session cookie and redirects back to the results.
    r = s.get(SESSION_URL, params=session_params(query, token, lens, period), timeout=30)
    landed = urllib.parse.urlsplit(r.url)
    if landed.hostname != "kagi.com" or not landed.path.startswith(("/html/search", "/search")):
        raise CredentialError(f"session: link rejected, landed on {landed.scheme}://{landed.hostname}{landed.path}")
    if r.status_code == 429:
        raise KagiError("session: rate limited (HTTP 429)")
    r.raise_for_status()
    r.encoding = "utf-8"
    return parse_session(r.text)[:n]


BACKENDS = {"api": search_api, "session": search_session}


# --- cli -----------------------------------------------------------------


def format_text(results: list[dict]) -> str:
    lines = []
    for i, x in enumerate(results, 1):
        lines.append(f"{i}. {x['title']}\n   {x['url']}" + (f"  ({x['date']})" if x["date"] else ""))
        if x["abstract"]:
            lines.append(f"   {x['abstract'][:240]}")
    return "\n".join(lines)


def parse_args(argv):
    p = argparse.ArgumentParser(prog="kagi-search", description="Search Kagi via its API or a session link.",
                                epilog="Exit: 0 results, 1 none, 2 usage, 3 credential, 4 unexpected response, 5 network")
    p.add_argument("query", nargs="+")
    p.add_argument("-n", type=int, default=10, help="max results, 1-50 (default 10)")
    p.add_argument("--json", action="store_true", help="JSON list of {title, url, abstract, date}")
    p.add_argument("--lens", choices=LENSES)
    p.add_argument("--time", choices=TIME, help="only results from the last day/week/month/year")
    p.add_argument("--backend", choices=["auto", "session", "api"], default="auto",
                   help="auto = session if a session link is configured, else api")
    a = p.parse_args(argv)
    if not 1 <= a.n <= 50:
        p.error("-n must be between 1 and 50")
    return a


# A broken session link (rejected, or Kagi changed the page) is worth retrying on
# the API; no results, network errors and rate limits are not.
FALLBACK_ON = (CredentialError, UnexpectedResponse)


def run(a, secrets: list[str], environ=os.environ) -> list[dict]:
    failed = None
    for backend, secret, problem in candidates(a.backend, environ):
        if failed:
            print(f"kagi-search: {redact(str(failed), *secrets)}; falling back to {backend}", file=sys.stderr)
        if problem:
            failed = problem
            continue
        secrets.append(secret)
        try:
            return BACKENDS[backend](secret, " ".join(a.query), a.n, a.lens, a.time)[: a.n]
        except FALLBACK_ON as e:
            failed = e
    raise failed


def main(argv=None) -> int:
    a = parse_args(argv)
    try:
        interval = float(os.environ.get("KAGI_MIN_INTERVAL", "1"))
    except ValueError:
        print("kagi-search: KAGI_MIN_INTERVAL must be a number of seconds", file=sys.stderr)
        return 2
    secrets: list[str] = []
    try:
        with RateGap(interval):
            res = run(a, secrets)
        if not res:
            raise NoResults("no results")
    except KagiError as e:
        print(f"kagi-search: {redact(str(e), *secrets)}", file=sys.stderr)
        return e.exit_code
    except requests.RequestException as e:
        print(f"kagi-search: network: {redact(str(e), *secrets)}", file=sys.stderr)
        return 5
    except Exception as e:  # never let a crash look like exit 1 ("no results")
        print(f"kagi-search: unexpected {type(e).__name__}: {redact(str(e), *secrets)}", file=sys.stderr)
        return 4
    if a.json:
        json.dump(res, sys.stdout, ensure_ascii=False, indent=1)
        print()
    else:
        print(format_text(res))
    return 0


if __name__ == "__main__":
    sys.exit(main())
