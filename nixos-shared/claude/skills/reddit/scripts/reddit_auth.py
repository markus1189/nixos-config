#!/usr/bin/env nix
#! nix shell nixpkgs#python3 --command python
"""One-time Reddit OAuth authorization-code flow → permanent refresh token.

Reddit's password grant only works for apps registered as type "script", and
breaks under 2FA. The authorization-code flow works for any app type, survives
2FA, and yields a refresh token that never expires. Run this once.

    ./reddit_auth.py                 # uses pass api/reddit/{clientId,clientSecret}
    ./reddit_auth.py --client agent  # uses pass api/reddit/agent/*
"""

import argparse
import base64
import http.server
import json
import secrets
import subprocess
import sys
import threading
import urllib.error
import urllib.parse
import urllib.request

UA = "claude-code:reddit-skill:v0.2 (by /u/markus1189)"
SCOPES = ["identity", "read", "mysubreddits", "history"]
PASS_ENTRY = "api/reddit/refreshToken"

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


def sh(cmd):
    r = subprocess.run(cmd, capture_output=True, text=True)
    if r.returncode != 0:
        sys.exit(f"error: `{' '.join(cmd)}` failed: {r.stderr.strip()}")
    return r.stdout.splitlines()[0].strip()


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--client", choices=["default", "agent"], default="default",
                    help="which pass credential pair to use")
    # Matches the URI registered for this app at reddit.com/prefs/apps. Reddit
    # compares it byte-for-byte and answers "invalid redirect_uri parameter"
    # on any mismatch, trailing slash included.
    ap.add_argument("--redirect-uri", default="http://localhost:8000",
                    help="must byte-exactly match the URI registered at "
                         "reddit.com/prefs/apps (default matches this account)")
    ap.add_argument("--no-store", action="store_true",
                    help="print the refresh token instead of writing to pass")
    args = ap.parse_args()

    base = "api/reddit/agent" if args.client == "agent" else "api/reddit"
    cid = sh(["pass", f"{base}/clientId"])
    csec = sh(["pass", f"{base}/clientSecret"])

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
Reddit app  : {base}  (client_id {cid[:6]}…)
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

    if args.no_store:
        print(f"\nrefresh_token: {rt}")
        return

    p = subprocess.run(["pass", "insert", "-m", "-f", PASS_ENTRY],
                       input=rt + "\n", capture_output=True, text=True)
    if p.returncode != 0:
        sys.exit(f"error: pass insert failed: {p.stderr.strip()}\n"
                 f"refresh_token (store it yourself): {rt}")

    print(f"""
Stored refresh token at: pass {PASS_ENTRY}
Granted scopes: {tok.get('scope')}

Verify with:
  env REDDIT_CLIENT_ID="$(pass {base}/clientId)" \\
      REDDIT_CLIENT_SECRET="$(pass {base}/clientSecret)" \\
      REDDIT_REFRESH_TOKEN="$(pass {PASS_ENTRY})" \\
      ~/.claude/skills/reddit/scripts/reddit.py whoami
""")


if __name__ == "__main__":
    main()
