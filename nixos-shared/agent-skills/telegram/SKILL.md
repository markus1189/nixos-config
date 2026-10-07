---
name: telegram
description: "Reads the Telegram bot's inbox and sends text, photos or documents through the bot. Use when the user mentions Telegram or the bot, or asks to check or send Telegram messages."
---

# Telegram Bot Integration

The scripts take the token from `TELEGRAM_BOT_TOKEN`, else from
`/run/agenix/telegram.env`; no setup needed. Errors go to stderr with the token
redacted. Run them from this skill's directory.

## Checking Inbox Messages

```bash
./scripts/check_messages.py
```

Prints each new message (chat ID, user, time, message ID, text/caption) and
downloads photos, documents, voice and video to `~/Downloads/` (override with
`TELEGRAM_DOWNLOAD_DIR`); the output lists the saved paths. **Destructive:**
it confirms the updates, so every message shown is gone from the inbox
afterwards. Run it once and keep the output. Files over 20 MB cannot be
downloaded (Bot API limit); the message is shown with a download error.

## Sending Messages

```bash
./scripts/send_message.py 12345 "Hello from Claude"
./scripts/send_message.py 12345 --photo ~/image.png --caption "Optional caption"
./scripts/send_message.py 12345 --document ~/report.pdf --caption "Optional caption"
```

The bot can only message users who have sent it `/start` and not blocked it
(403 otherwise), and only groups it was added to.

## Chat IDs

**Default chat ID: 299952716** (user @markus1189). Use it when none is given.
Other chat IDs appear in the inbox output.
