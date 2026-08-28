"""botler -- a Telegram command bot.

Long-polls getUpdates and runs a whitelisted table of commands on behalf of a
small allowlist of chat ids. Two design points are load-bearing:

- It performs ALL of its own Telegram API calls rather than shelling out to the
  notifySend* family. Those scripts source /run/agenix/telegram.env, which
  defines TELEGRAM_BOT_TOKEN and would therefore override botler's own token --
  replies would arrive from the wrong bot. Handlers here never touch Telegram;
  they only produce a file or write to stdout.

- getUpdates is exclusive per token: a second poller makes both sides see HTTP
  409, and whoever polls first consumes the update. Hence botler runs on exactly
  one host, with a token of its own.

Stdlib only, on purpose: writers.writePython3Bin then needs no `libraries` and
flake8 runs over this file at build time.
"""

import argparse
import json
import logging
import os
import subprocess
import sys
import tempfile
import time
import urllib.error
import urllib.request
import uuid

API = "https://api.telegram.org"

# Telegram holds a long poll open for --timeout seconds and answers empty after
# that; the socket timeout has to sit comfortably above it or every idle poll
# looks like a network failure.
LONG_POLL_SECONDS = 50
SOCKET_TIMEOUT = LONG_POLL_SECONDS + 15
UPLOAD_TIMEOUT = 120
HANDLER_TIMEOUT = 180
MAX_BACKOFF = 60
MAX_TEXT = 4096

log = logging.getLogger("botler")


def multipart(fields, field, filename, payload, content_type):
    """Hand-built multipart/form-data -- `requests` is not a dependency here."""
    boundary = uuid.uuid4().hex
    body = bytearray()
    for key, value in fields.items():
        body += f'--{boundary}\r\nContent-Disposition: form-data; name="{key}"\r\n\r\n{value}\r\n'.encode()
    body += f'--{boundary}\r\nContent-Disposition: form-data; name="{field}"; filename="{filename}"\r\n'.encode()
    body += f"Content-Type: {content_type}\r\n\r\n".encode()
    body += payload
    body += f"\r\n--{boundary}--\r\n".encode()
    return bytes(body), f"multipart/form-data; boundary={boundary}"


class Bot:
    def __init__(self, token):
        self.token = token

    def url(self, method):
        return f"{API}/bot{self.token}/{method}"

    def call(self, method, params=None, timeout=30):
        req = urllib.request.Request(
            self.url(method),
            data=json.dumps(params or {}).encode(),
            headers={"Content-Type": "application/json"},
        )
        with urllib.request.urlopen(req, timeout=timeout) as resp:
            return json.load(resp)["result"]

    def upload(self, method, fields, field, path, content_type):
        with open(path, "rb") as handle:
            body, ctype = multipart(fields, field, os.path.basename(path), handle.read(), content_type)
        req = urllib.request.Request(self.url(method), data=body, headers={"Content-Type": ctype})
        with urllib.request.urlopen(req, timeout=UPLOAD_TIMEOUT) as resp:
            return json.load(resp)["result"]

    def send_message(self, chat_id, text):
        self.call("sendMessage", {"chat_id": chat_id, "text": text[:MAX_TEXT]})

    def send_animation(self, chat_id, path, caption):
        # sendAnimation, not sendPhoto: Telegram transcodes the GIF to MP4 and
        # plays it inline, whereas a photo upload flattens it to a still frame.
        self.upload(
            "sendAnimation",
            {"chat_id": str(chat_id), "caption": caption},
            "animation",
            path,
            "image/gif",
        )

    def send_chat_action(self, chat_id, action):
        # Cosmetic ("sending video..."), so a failure here must never abort the
        # command it was announcing.
        try:
            self.call("sendChatAction", {"chat_id": chat_id, "action": action})
        except (OSError, ValueError, KeyError) as err:
            log.warning("sendChatAction failed: %s", err)


def load_offset(path):
    try:
        with open(path) as handle:
            return int(handle.read().strip())
    except (FileNotFoundError, ValueError):
        return None


def save_offset(path, offset):
    tmp = f"{path}.tmp"
    with open(tmp, "w") as handle:
        handle.write(str(offset))
    os.replace(tmp, path)


def skip_backlog(bot):
    """Offset past whatever is already queued.

    A first start must not replay a backlog of stale commands -- the messages
    sitting in the queue may be hours old, and Telegram keeps them for 24h.
    offset=-1 returns just the newest update and confirms nothing.
    """
    updates = bot.call("getUpdates", {"offset": -1, "timeout": 0})
    if updates:
        return updates[-1]["update_id"] + 1
    return 0


def help_text(commands):
    lines = ["botler", ""]
    for name in sorted(commands):
        lines.append(f"/{name} - {commands[name].get('help', '')}".rstrip(" -"))
    lines.append("/help - diese Uebersicht")
    return "\n".join(lines)


def run_command(bot, name, spec, chat_id):
    kind = spec.get("kind", "text")
    with tempfile.TemporaryDirectory(prefix="botler-") as tmp:
        if kind == "animation":
            out = os.path.join(tmp, "out.gif")
            argv = [spec["exec"], out]
            bot.send_chat_action(chat_id, "upload_video")
        else:
            out = None
            argv = [spec["exec"]]
            bot.send_chat_action(chat_id, "typing")

        log.info("/%s: running %s", name, " ".join(argv))
        try:
            proc = subprocess.run(argv, capture_output=True, text=True, timeout=HANDLER_TIMEOUT)
        except subprocess.TimeoutExpired:
            log.error("/%s: handler exceeded %ss", name, HANDLER_TIMEOUT)
            bot.send_message(chat_id, f"/{name}: Zeitueberschreitung.")
            return
        except OSError as err:
            log.error("/%s: cannot execute handler: %s", name, err)
            bot.send_message(chat_id, f"/{name}: fehlgeschlagen.")
            return

        if proc.returncode != 0:
            log.error("/%s: handler exited %d: %s", name, proc.returncode, proc.stderr.strip())
            bot.send_message(chat_id, f"/{name}: fehlgeschlagen.")
            return
        if proc.stderr.strip():
            log.info("/%s: handler stderr: %s", name, proc.stderr.strip())

        if kind == "animation":
            if not os.path.exists(out):
                log.error("/%s: handler produced no file at %s", name, out)
                bot.send_message(chat_id, f"/{name}: keine Ausgabe erzeugt.")
                return
            bot.send_animation(chat_id, out, spec.get("caption", ""))
        else:
            bot.send_message(chat_id, proc.stdout.strip() or "(keine Ausgabe)")
        log.info("/%s: delivered", name)


def dispatch(bot, commands, allowed, update):
    message = update.get("message")
    if not message:
        return
    text = message.get("text")
    chat_id = message.get("chat", {}).get("id")
    if not text or chat_id is None:
        return
    if str(chat_id) not in allowed:
        log.warning("ignoring message from chat %s (not in allowlist)", chat_id)
        return

    # "/radar@somebot extra args" -> "radar"
    name = text.strip().split()[0].split("@", 1)[0].lstrip("/").lower()

    spec = commands.get(name)
    if spec is None:
        if name not in ("start", "help"):
            log.info("unknown command /%s", name)
        bot.send_message(chat_id, help_text(commands))
        return
    run_command(bot, name, spec, chat_id)


def poll_forever(bot, commands, allowed, offset_path):
    offset = load_offset(offset_path)
    if offset is None:
        offset = skip_backlog(bot)
        save_offset(offset_path, offset)
        log.info("no stored offset, starting at %d", offset)
    else:
        log.info("resuming at offset %d", offset)

    backoff = 1
    while True:
        try:
            updates = bot.call(
                "getUpdates",
                {"offset": offset, "timeout": LONG_POLL_SECONDS, "allowed_updates": ["message"]},
                timeout=SOCKET_TIMEOUT,
            )
        except urllib.error.HTTPError as err:
            if err.code == 409:
                log.warning("409 Conflict: another getUpdates poller holds this token")
            else:
                log.warning("getUpdates: HTTP %s %s", err.code, err.reason)
            time.sleep(backoff)
            backoff = min(backoff * 2, MAX_BACKOFF)
            continue
        except (OSError, ValueError, KeyError) as err:
            log.warning("getUpdates: %s", err)
            time.sleep(backoff)
            backoff = min(backoff * 2, MAX_BACKOFF)
            continue

        backoff = 1
        if not updates:
            continue

        # Persisted before executing, so a handler that takes the process down
        # loses its command rather than replaying it on every restart.
        offset = max(u["update_id"] for u in updates) + 1
        save_offset(offset_path, offset)

        for update in updates:
            try:
                dispatch(bot, commands, allowed, update)
            except Exception as err:  # one bad message must not end the service
                log.exception("dispatch failed: %s", err)


def main():
    parser = argparse.ArgumentParser(description="Telegram command bot")
    parser.add_argument("--commands-file", required=True, help="JSON object: name -> {help, kind, exec, caption}")
    parser.add_argument("--allow", action="append", default=[], metavar="CHAT_ID", help="chat id allowed to issue commands (repeatable)")
    parser.add_argument("--state-dir", default=None, help="where to persist the update offset (default: $STATE_DIRECTORY)")
    args = parser.parse_args()

    logging.basicConfig(level=logging.INFO, format="%(levelname)s %(message)s")

    token = os.environ.get("TELEGRAM_BOT_TOKEN")
    if not token:
        sys.exit("TELEGRAM_BOT_TOKEN is not set")
    if not args.allow:
        # An empty allowlist would mean "serve the whole internet", which is
        # never what is wanted here -- refuse instead of guessing.
        sys.exit("--allow is required: refusing to accept commands from everyone")

    state_dir = args.state_dir or os.environ.get("STATE_DIRECTORY", "").split(":")[0]
    if not state_dir:
        sys.exit("no state directory: pass --state-dir or run under systemd with StateDirectory=")
    os.makedirs(state_dir, exist_ok=True)

    with open(args.commands_file) as handle:
        commands = json.load(handle)

    log.info("serving %s to chat ids %s", ", ".join(f"/{c}" for c in sorted(commands)) or "(nothing)", ", ".join(args.allow))
    poll_forever(Bot(token), commands, set(args.allow), os.path.join(state_dir, "offset"))


if __name__ == "__main__":
    main()
