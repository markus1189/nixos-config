---
name: telegram
description: Direct integration with Telegram Bot API for checking inbox messages and sending messages (text, photos, documents) to chats. Use when the user asks to "check Telegram inbox", "check messages", "send message to [chat]", or needs to interact with a Telegram bot. Requires TELEGRAM_BOT_TOKEN environment variable.
---

# Telegram Bot Integration

Interact directly with Telegram Bot API to check messages and send content to chats.

## Prerequisites

The bot token is required. Use `env` to pass it from your password store for single-command usage:

```bash
env TELEGRAM_BOT_TOKEN="$(pass api/telegram)" ./scripts/check_messages.py
```

Optional: Set custom download directory (default: `~/Downloads`):
```bash
env TELEGRAM_DOWNLOAD_DIR='~/my-telegram-files' ./scripts/check_messages.py
```

Scripts use Nix shebangs to automatically provide the `pyTelegramBotAPI` dependency - no manual installation needed.

## Checking Inbox Messages

Use `scripts/check_messages.py` when the user asks about Telegram messages.

**Triggers:**
- "Check my Telegram inbox"
- "Any new Telegram messages?"
- "What messages did I get?"

**What it does:**
- Polls Telegram API for new updates
- Downloads photos and documents to `~/Downloads/` (or `TELEGRAM_DOWNLOAD_DIR`)
- Displays message details (chat ID, user, timestamp, content)
- Marks messages as read after displaying

**Example:**
```bash
env TELEGRAM_BOT_TOKEN="$(pass api/telegram)" ./scripts/check_messages.py
```

**Output format:**
```
Chat ID: 12345
User: John Doe
Username: @johndoe
Time: 2025-01-10 14:30:45
Message ID: 789
Text: Hello bot!
Photo: /home/user/Downloads/telegram_12345_789_photo.jpg
```

## Sending Messages

Use `scripts/send_message.py` when the user wants to send content via Telegram.

**Triggers:**
- "Send message to chat 12345"
- "Send this photo via Telegram"
- "Message user X saying..."

### Send Text

```bash
env TELEGRAM_BOT_TOKEN="$(pass api/telegram)" ./scripts/send_message.py <chat_id> <message_text>
```

Example:
```bash
env TELEGRAM_BOT_TOKEN="$(pass api/telegram)" ./scripts/send_message.py 12345 "Hello from Claude"
```

### Send Photo

```bash
env TELEGRAM_BOT_TOKEN="$(pass api/telegram)" ./scripts/send_message.py <chat_id> --photo <filepath>
```

Example:
```bash
env TELEGRAM_BOT_TOKEN="$(pass api/telegram)" ./scripts/send_message.py 12345 --photo ~/image.png
```

With caption:
```bash
env TELEGRAM_BOT_TOKEN="$(pass api/telegram)" ./scripts/send_message.py 12345 --photo ~/image.png --caption "Check this out"
```

### Send Document

```bash
env TELEGRAM_BOT_TOKEN="$(pass api/telegram)" ./scripts/send_message.py <chat_id> --document <filepath>
```

Example:
```bash
env TELEGRAM_BOT_TOKEN="$(pass api/telegram)" ./scripts/send_message.py 12345 --document ~/report.pdf --caption "Monthly report"
```

## Important Constraints

**Bot limitations:**
- Cannot message users who haven't started conversation with the bot (must send /start first)
- Cannot message users who have blocked the bot
- Bot must be added to groups before messaging them

**Rate limits:**
- ~30 messages/second to different chats
- ~1 message/second to same chat

**File size limits:**
- Photos: 10MB max
- Documents/Videos: 50MB max

## Error Handling

Common errors are handled automatically by the scripts:
- **401**: Invalid bot token
- **403**: User hasn't started conversation or blocked bot
- **429**: Rate limit - scripts report retry delay
- **File not found**: Clear error message with filepath

For detailed error reference, see [references/error_codes.md](references/error_codes.md).

## Chat IDs

Chat IDs are required for sending messages. Users can get their chat ID by:
1. Checking inbox output (displays chat ID for each message)
2. Sending a message to the bot and checking inbox

## Stateless Operation

This skill is stateless. Telegram's API automatically tracks which messages have been read using update offsets. No local state storage needed.
