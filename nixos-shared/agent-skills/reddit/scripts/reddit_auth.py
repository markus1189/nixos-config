#!/usr/bin/env nix
#! nix shell nixpkgs#python3 --command python
"""One-time Reddit OAuth authorization-code flow → permanent refresh token.

Reddit's password grant only works for apps registered as type "script", and
breaks under 2FA. The authorization-code flow works for any app type, survives
2FA, and yields a refresh token that never expires. Run this once.

    ./reddit_auth.py   # app credentials from /run/agenix/reddit.env
"""

import argparse
import base64
import http.server
import json
import secrets
import sys
import threading
import urllib.error
import urllib.parse
import urllib.request
from pathlib import Path

UA = "claude-code:reddit-skill:v0.2 (by /u/markus1189)"
SCOPES = ["identity", "read", "mysubreddits", "history"]
ENV_FILE = Path("/run/agenix/reddit.env")

_result = {}


class Handler(http.server.BaseHTTPRequestHandler):
    def do_GET(self):
        q = urllib.parse.parse_qs(urllib.parse.urlparse(self.path).query)
        _result.update({k: v[0] for k, v in q.items()})
        ok = "code" in _result
        self.send_response(200)
        self.send_header("Content-Type", "text/html; charset=utf-8")
        self.end_headers()
        msg = ("<h2>Authorized.</h2><p>You may close this tab and return to "
               "the terminal.</p>") if ok else \
              f"<h2>Failed.</h2><pre>{_result.get('error', 'no code')}</pre>"
        self.wfile.write(f"<body style='font:16px sans-serif;padding:3em'>{msg}"
                         "</body>".encode())

    def log_message(self, *a):
        pass


def secret(key):
    for line in ENV_FILE.read_text().splitlines():
        k, sep, v = line.strip().removeprefix("export ").partition("=")
        if sep and k.strip() == key:
            return v.strip().strip("\"'")
    sys.exit(f"error: no {key} in {ENV_FILE}")


def main():
    ap = argparse.ArgumentParser()
    # Matches the URI registered for this app at reddit.com/prefs/apps. Reddit
    # compares it byte-for-byte and answers "invalid redirect_uri parameter"
    # on any mismatch, trailing slash included.
    ap.add_argument("--redirect-uri", default="http://localhost:8000",
                    help="must byte-exactly match the URI registered at "
                         "reddit.com/prefs/apps (default matches this account)")
    args = ap.parse_args()

    cid, csec = secret("REDDIT_CLIENT_ID"), secret("REDDIT_CLIENT_SECRET")

    # Bind the local server to the redirect's host:port; its path is irrelevant
    # to us but must be preserved verbatim in the token exchange.
    redirect = args.redirect_uri
    u = urllib.parse.urlparse(redirect)
    host = u.hostname or "localhost"
    port = u.port or (443 if u.scheme == "https" else 80)
    state = secrets.token_urlsafe(16)

    auth_url = "https://www.reddit.com/api/v1/authorize?" + urllib.parse.urlencode({
        "client_id": cid,
        "response_type": "code",
        "state": state,
        "redirect_uri": redirect,
        "duration": "permanent",
        "scope": " ".join(SCOPES),
    })

    try:
        srv = http.server.HTTPServer((host, port), Handler)
    except OSError as e:
        sys.exit(f"error: cannot listen on {host}:{port} — {e}\n"
                 f"(if the port is privileged or taken, register a different "
                 f"redirect URI at reddit.com/prefs/apps and pass it with "
                 f"--redirect-uri)")
    threading.Thread(target=srv.handle_request, daemon=True).start()

    print(f"""
Reddit app  : client_id {cid[:6]}…
Redirect URI: {redirect}
Scopes      : {' '.join(SCOPES)}   (read-only)

IMPORTANT: this redirect URI must match the one registered at
https://www.reddit.com/prefs/apps exactly. If it doesn't, edit the app there.

Open this URL, then click "Allow":

{auth_url}

waiting for the callback on {redirect} …""", flush=True)

    srv.socket.settimeout(300)
    for _ in range(300):
        if _result:
            break
        threading.Event().wait(1)

    if "code" not in _result:
        sys.exit(f"error: no authorization code received "
                 f"({_result.get('error', 'timed out after 5 min')})")
    if _result.get("state") != state:
        sys.exit("error: state mismatch — possible CSRF, aborting")

    body = urllib.parse.urlencode({
        "grant_type": "authorization_code",
        "code": _result["code"],
        "redirect_uri": redirect,
    }).encode()
    req = urllib.request.Request("https://www.reddit.com/api/v1/access_token",
                                 data=body, method="POST")
    req.add_header("User-Agent", UA)
    req.add_header("Authorization", "Basic " +
                   base64.b64encode(f"{cid}:{csec}".encode()).decode())
    try:
        with urllib.request.urlopen(req, timeout=30) as r:
            tok = json.loads(r.read().decode())
    except urllib.error.HTTPError as e:
        sys.exit(f"error: token exchange failed ({e.code}): "
                 f"{e.read().decode()[:300]}")

    rt = tok.get("refresh_token")
    if not rt:
        sys.exit(f"error: no refresh_token in response: {tok}\n"
                 "(was duration=permanent honoured?)")

    # agenix files are root-encrypted and read-only at runtime, so the
    # token can only reach reddit.env.age through a manual edit.
    print(f"""
refresh_token: {rt}
Granted scopes: {tok.get('scope')}

Store it: `agenix -e secrets/reddit.env.age` in nixos-config, set
REDDIT_REFRESH_TOKEN=<token>, rebuild. Then verify with:
  {Path(__file__).resolve().parent / "reddit.py"} whoami
""")


if __name__ == "__main__":
    main()
