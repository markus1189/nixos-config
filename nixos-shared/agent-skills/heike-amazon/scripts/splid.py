#!/usr/bin/env python3
"""Add an expense to the Splid group via its Parse backend."""
import argparse
import json
import re
import sys
import urllib.request
import uuid
from datetime import datetime, timezone
from decimal import Decimal

ENV = "/run/agenix/splid.env"


def load_env():
    out = {}
    with open(ENV) as f:
        for line in f:
            m = re.match(r'\s*([A-Z_]+)=(.*)', line)
            if m:
                out[m[1]] = m[2].strip().strip('"')
    return out


def main():
    e = load_env()
    members = {k.removeprefix("SPLID_MEMBER_").lower(): v for k, v in e.items() if k.startswith("SPLID_MEMBER_")}
    ap = argparse.ArgumentParser()
    ap.add_argument("--title", required=True)
    ap.add_argument("--amount", required=True)
    ap.add_argument("--payer", required=True, choices=sorted(members))
    ap.add_argument("--for", dest="debtors", required=True, nargs="+", choices=sorted(members),
                    help="split equally among these")
    ap.add_argument("--dry-run", action="store_true")
    a = ap.parse_args()

    if not re.fullmatch(r"\d+(\.\d{1,2})?", a.amount) or Decimal(a.amount) <= 0:
        sys.exit(f"invalid amount {a.amount!r}: want e.g. 63.94")
    title = a.title.strip()
    if not title or len(title) > 100 or any(ord(c) < 32 for c in title):
        sys.exit("invalid title: non-empty, <=100 chars, no control characters")
    if len(set(a.debtors)) != len(a.debtors):
        sys.exit("duplicate --for member")

    share = 1 / len(a.debtors)
    body = {
        "date": {"__op": "Delete"},
        "UpdateInstallationID": str(uuid.uuid4()),
        "secondaryPayers": {"__op": "Delete"},
        "primaryPayer": members[a.payer],
        "UpdateID": str(uuid.uuid4()),
        "title": title,
        "GlobalId": str(uuid.uuid4()),
        "isPayment": False,
        "category": {"__op": "Delete"},
        "items": [{"P": {"P": {members[d]: share for d in a.debtors}, "PT": 0}, "AM": float(a.amount)}],
        "currencyCode": "EUR",
        "createdGlobally": {"__type": "Date",
                            "iso": datetime.now(timezone.utc).isoformat(timespec="milliseconds").replace("+00:00", "Z")},
        "group": {"__type": "Pointer", "className": "_User", "objectId": e["SPLID_GROUP_ID"]},
    }
    if a.dry_run:
        print(json.dumps(body, indent=2, ensure_ascii=False))
        return 0

    req = urllib.request.Request(
        e["SPLID_PARSE_URL"].rstrip("/") + "/classes/Entry",
        data=json.dumps(body).encode(),
        method="POST",
        headers={
            "X-Parse-Application-Id": e["SPLID_PARSE_APP_ID"],
            "X-Parse-Client-Key": e["SPLID_PARSE_CLIENT_KEY"],
            "X-Parse-Installation-Id": e["SPLID_INSTALLATION_ID"],
            "X-Parse-App-Build-Version": e["SPLID_APP_BUILD_VERSION"],
            "X-Parse-App-Display-Version": e["SPLID_APP_DISPLAY_VERSION"],
            "X-Parse-OS-Version": e["SPLID_OS_VERSION"],
            "User-Agent": e["SPLID_USER_AGENT"],
            "Content-Type": "application/json",
        },
    )
    with urllib.request.urlopen(req, timeout=30) as r:
        print(r.status, r.read().decode())
    return 0


if __name__ == "__main__":
    sys.exit(main())
