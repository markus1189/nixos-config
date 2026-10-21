#!/usr/bin/env nix
#! nix shell nixpkgs#gws nixpkgs#python3 --command python3
"""Print Heike's unread Amazon share mails as JSON lines: date, title, link, note."""
import base64
import json
import re
import subprocess
import sys

ENV = "/run/agenix/splid.env"


def env():
    out = {}
    with open(ENV) as f:
        for line in f:
            m = re.match(r'\s*([A-Z_]+)=(.*)', line)
            if m:
                out[m[1]] = m[2].strip().strip('"')
    return out


def gws(method, params):
    r = subprocess.run(["gws", "gmail", "users", "messages", method, "--params", json.dumps(params)],
                       capture_output=True, text=True, check=True)
    return json.loads(r.stdout)


def plain(part):
    data = part.get("body", {}).get("data")
    if data and part.get("mimeType", "").startswith("text/plain"):
        yield base64.urlsafe_b64decode(data + "==").decode("utf8", "replace")
    for p in part.get("parts") or []:
        yield from plain(p)


def main():
    e = env()
    q = f'from:{e["HEIKE_EMAIL"]} subject:"{e["HEIKE_SUBJECT"]}" is:unread in:inbox'
    ids = [m["id"] for m in gws("list", {"userId": "me", "q": q, "maxResults": 100}).get("messages", [])]
    for i in reversed(ids):
        m = gws("get", {"userId": "me", "id": i, "format": "full"})
        h = {x["name"]: x["value"] for x in m["payload"]["headers"]}
        body = "\n".join(plain(m["payload"])).strip()
        link = re.search(r'https://\S*amzn\S+|https://\S*amazon\.\S+', body)
        if not link:
            print(json.dumps({"date": h.get("Date"), "error": "no amazon link", "body": body}), flush=True)
            continue
        title = " ".join(body[:link.start()].split()).rstrip(" |")
        note = " ".join(body[link.end():].split())
        print(json.dumps({"date": h.get("Date"), "title": title, "link": link[0], "note": note},
                         ensure_ascii=False))


if __name__ == "__main__":
    sys.exit(main())
