#!/usr/bin/env python3
"""Refresh prices.json and limits.json from Requesty's /v1/models.

Usage: update.py [--check] [MODEL_ID...]

Refreshes every model already in prices.json, plus any MODEL_ID given (which
adds it). Prices are USD per million tokens, limits are tokens. --check writes
nothing and exits 1 if either table differs from the API.

Key: $REQUESTY_API_KEY_CC, else `pass api/requesty/agent`. The key matters:
/v1/models lists only the organization's access list when called with one.

Exit status: 0 up to date / written, 1 drift (--check), 2 a model is not on
the access list.
"""

import json
import os
import subprocess
import sys
import urllib.request
from pathlib import Path

PRICES = Path(__file__).resolve().parent / "prices.json"
LIMITS = PRICES.with_name("limits.json")
URL = "https://router.eu.requesty.ai/v1/models"

# Requesty's per-token field -> our per-million field.
FIELDS = {
    "input_price": "input",
    "output_price": "output",
    "cached_price": "cacheRead",
    "caching_price": "cacheWrite",
}

LIMIT_FIELDS = {
    "context_window": "context",
    "max_output_tokens": "output",
}


def api_key():
    key = os.environ.get("REQUESTY_API_KEY_CC")
    if key:
        return key
    return subprocess.run(
        ["pass", "api/requesty/agent"], check=True, capture_output=True, text=True
    ).stdout.strip()


def per_million(per_token):
    # round() strips float noise (1.2e-07 * 1e6 = 0.12000000000000001); int()
    # keeps whole prices as 11, not 11.0, so the diff stays quiet.
    value = round((per_token or 0) * 1e6, 6)
    return int(value) if value == int(value) else value


def main(argv):
    check = "--check" in argv
    wanted = [a for a in argv if a != "--check"]

    current = json.loads(PRICES.read_text()) if PRICES.exists() else {}
    request = urllib.request.Request(
        URL, headers={"Authorization": f"Bearer {api_key()}"}
    )
    with urllib.request.urlopen(request, timeout=30) as response:
        listed = {m["id"]: m for m in json.load(response)["data"]}

    ids = sorted(set(current) | set(wanted))
    missing = [i for i in ids if i not in listed]
    if missing:
        print("not on the access list: " + ", ".join(missing), file=sys.stderr)
        return 2

    tables = [
        (
            PRICES,
            {
                i: {ours: per_million(listed[i].get(theirs)) for theirs, ours in FIELDS.items()}
                for i in ids
            },
        ),
        (
            LIMITS,
            {
                i: {ours: listed[i][theirs] for theirs, ours in LIMIT_FIELDS.items()}
                for i in ids
            },
        ),
    ]
    stale = []
    for path, fresh in tables:
        old = json.loads(path.read_text()) if path.exists() else {}
        for i in ids:
            if old.get(i) != fresh[i]:
                print(f"{path.name} {i}: {old.get(i)} -> {fresh[i]}", file=sys.stderr)
        if fresh != old:
            stale.append((path, fresh))

    if stale and check:
        return 1
    for path, fresh in stale:
        path.write_text(json.dumps(fresh, indent=2) + "\n")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
