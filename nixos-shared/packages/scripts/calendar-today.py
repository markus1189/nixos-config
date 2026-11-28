"""Today's events from a Google Calendar as markdown, via the gws CLI.

calendar-today --account NAME [--date D]
    print one day (default today) of the account whose gws login is
    /run/agenix/gcal-NAME.json
calendar-today --account NAME --write --host TAG
    (over)write ~/Stuff/YYYY-MM/DD-TAG/autogen-calendar-NAME.md for today,
    only if its content changed
"""

import argparse
import html
import json
import os
import re
import subprocess
import sys
import tempfile
from datetime import date, datetime, time, timedelta
from html.parser import HTMLParser
from pathlib import Path

# Replaced with a store path by the Nix build.
GWS = "@gws@"
if GWS.startswith("@"):
    GWS = "gws"

VIDEO = re.compile(
    r"https://(?:[\w-]+\.)?(?:zoom\.us|teams\.microsoft\.com|meet\.google\.com"
    r"|webex\.com|whereby\.com|jit\.si)/[^\s\"'<>)\]]+"
)
RSVP = {
    "accepted": "✓",
    "declined": "✗",
    "tentative": "?",
    "needsAction": "·",
}


class Text(HTMLParser):
    """Calendar descriptions are HTML when written in the web UI, plain
    text otherwise; links keep their target."""

    def __init__(self):
        super().__init__()
        self.out, self.href = [], None

    def handle_starttag(self, tag, attrs):
        if tag in ("br", "p", "div", "li", "tr"):
            self.out.append("\n")
        if tag == "li":
            self.out.append("- ")
        if tag == "a":
            self.href = dict(attrs).get("href")

    def handle_endtag(self, tag):
        if tag == "a":
            self.href = None

    def handle_data(self, data):
        if self.href and self.href != data.strip():
            data = f"[{data}]({self.href})"
        self.out.append(data)


def plain(desc):
    if not re.search(r"<[a-z][^>]*>", desc, re.I):
        return desc.strip()
    p = Text()
    p.feed(desc)
    text = html.unescape("".join(p.out))
    return re.sub(r"\n\s*\n\s*\n+", "\n\n", text).strip()


def fetch(creds, day):
    start = datetime.combine(day, time()).astimezone()
    params = {
        "calendarId": "primary",
        "timeMin": start.isoformat(),
        "timeMax": (start + timedelta(days=1)).isoformat(),
        "singleEvents": True,
        "orderBy": "startTime",
        "maxResults": 250,
    }
    # gws always writes a token cache and key into its config dir: a fresh
    # one per run, so ~/.config/gws is neither read nor written.
    with tempfile.TemporaryDirectory() as cfg:
        env = dict(
            os.environ,
            GOOGLE_WORKSPACE_CLI_CREDENTIALS_FILE=str(creds),
            GOOGLE_WORKSPACE_CLI_CONFIG_DIR=cfg,
            GOOGLE_WORKSPACE_CLI_KEYRING_BACKEND="file",
        )
        r = subprocess.run(
            [GWS, "calendar", "events", "list",
             "--params", json.dumps(params)],
            capture_output=True, text=True, env=env, timeout=60,
        )
    try:
        out = json.loads(r.stdout)
    except ValueError:
        out = {}
    if r.returncode or "error" in out or "items" not in out:
        err = out.get("error", {}).get("message") or r.stderr.strip()
        raise RuntimeError(err or f"gws exit {r.returncode}")
    return out["items"]


def when(ev, key):
    t = ev[key]
    if "dateTime" in t:
        return datetime.fromisoformat(t["dateTime"]).astimezone()
    return date.fromisoformat(t["date"])


def video_links(ev):
    links = [
        e["uri"]
        for e in (ev.get("conferenceData") or {}).get("entryPoints", [])
        if e.get("entryPointType") == "video"
    ]
    if ev.get("hangoutLink"):
        links.append(ev["hangoutLink"])
    for field in ("location", "description"):
        links += VIDEO.findall(ev.get(field) or "")
    return list(dict.fromkeys(links))


def person(a):
    name = a.get("displayName") or a.get("email", "?")
    if a.get("displayName") and a.get("email"):
        name += f" <{a['email']}>"
    tags = [x for x in ("organizer", "optional") if a.get(x)]
    return name + (f" ({', '.join(tags)})" if tags else "")


def my_status(ev):
    me = next((a for a in ev.get("attendees", []) if a.get("self")), None)
    return me.get("responseStatus") if me else None


def event_md(ev):
    start, end = when(ev, "start"), when(ev, "end")
    span = (
        f"{start:%H:%M}–{end:%H:%M}"
        if isinstance(start, datetime)
        else "all day"
    )
    title = ev.get("summary") or "(no title)"
    status = my_status(ev)
    flag = {"declined": " — declined", "tentative": " — tentative",
            "needsAction": " — not answered"}.get(status, "")
    if ev.get("status") == "cancelled":
        flag = " — cancelled"
    out = [f"## {span} · {title}{flag}", ""]
    if loc := ev.get("location"):
        out.append(f"- **Location:** {loc}")
    for link in video_links(ev):
        out.append(f"- **Video:** {link}")
    org = ev.get("organizer", {})
    if org.get("email") and not org.get("self"):
        name = org.get("displayName") or org["email"]
        out.append(f"- **Organizer:** {name}")
    # You are in the heading's RSVP flag, rooms are in Location.
    att = [
        a for a in ev.get("attendees", [])
        if not a.get("self") and not a.get("resource")
    ]
    if att:
        out.append(f"- **Participants ({len(att)}):**")
        out += [
            f"  - {RSVP.get(a.get('responseStatus'), '·')} {person(a)}"
            for a in att
        ]
    if desc := plain(ev.get("description") or ""):
        out += ["", desc]
    return "\n".join(out).rstrip() + "\n"


def render(events, day, account):
    places = [
        e.get("summary") or "?"
        for e in events
        if e.get("eventType") == "workingLocation"
    ]
    rest = [e for e in events if e.get("eventType") != "workingLocation"]
    o = [f"# {account} calendar {day:%Y-%m-%d %a}", ""]
    if places:
        o += [f"Working location: {', '.join(dict.fromkeys(places))}", ""]
    o.append(
        "RSVP: ✓ accepted · ? tentative · ✗ declined · · not answered."
        " Focus time and declined events are listed too."
    )
    o.append("")
    if not rest:
        o.append("No events.")
    o += [event_md(e) for e in rest]
    return "\n".join(o).rstrip() + "\n"


def write(stuff, host, day, text, name):
    """Day dirs belong to stuff-today and are never created here."""
    day_dir = stuff / f"{day:%Y-%m}" / f"{day:%d}-{host}"
    if not day_dir.is_dir():
        return "no dir"
    target = day_dir / name
    if target.exists() and target.read_text() == text:
        return "unchanged"
    tmp = target.with_name(f".{name}.tmp")
    tmp.write_text(text)
    tmp.replace(target)
    return "written"


def main(argv=None):
    ap = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    ap.add_argument("--write", action="store_true")
    ap.add_argument("--host", help="host tag in DD-<host>; needed by --write")
    ap.add_argument("--date", type=date.fromisoformat, default=date.today())
    ap.add_argument("--account", required=True,
                    help="names the secret and the output file")
    ap.add_argument(
        "--credentials",
        type=Path,
        help="gws authorized_user JSON (gws auth export --unmasked);"
        " default /run/agenix/gcal-ACCOUNT.json",
    )
    ap.add_argument(
        "--stuff", type=Path, default=Path.home() / "Stuff",
        help=argparse.SUPPRESS,
    )
    a = ap.parse_args(argv)
    if a.write and not a.host:
        ap.error("--write needs --host")
    creds = a.credentials or Path(f"/run/agenix/gcal-{a.account}.json")
    name = f"autogen-calendar-{a.account}.md"
    try:
        text = render(fetch(creds, a.date), a.date, a.account)
    except (RuntimeError, OSError, subprocess.TimeoutExpired) as e:
        # Written only where no good copy exists yet, so a network blip
        # keeps the day's file and an expired token still shows up in it.
        text = (
            f"# {a.account} calendar {a.date:%Y-%m-%d %a}\n\n"
            f"**Fetch failed:** {e}\n\n"
            "An `invalid_grant` means the refresh token expired: redo the "
            f"gws login and re-encrypt secrets/gcal-{a.account}.json.age.\n"
        )
        if a.write:
            day_dir = a.stuff / f"{a.date:%Y-%m}" / f"{a.date:%d}-{a.host}"
            if not (day_dir / name).exists():
                write(a.stuff, a.host, a.date, text, name)
        print(f"calendar-today: {e}", file=sys.stderr)
        sys.exit(1)
    if a.write:
        print(f"{a.date}: {write(a.stuff, a.host, a.date, text, name)}",
              file=sys.stderr)
    else:
        print(text, end="")


if __name__ == "__main__":
    main()
