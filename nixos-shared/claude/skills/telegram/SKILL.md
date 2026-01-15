---
name: telegram
description: "Direct integration with Telegram Bot API for checking inbox messages and sending messages (text, photos, documents) to chats. Triggers when users request 'check Telegram inbox', 'check messages', 'send message to [chat]', or mention Telegram bot interactions. Requires TELEGRAM_BOT_TOKEN environment variable."
---

# Telegram Bot Integration

Interact directly with Telegram Bot API to check messages and send content to chats. Scripts use Nix shebangs to automatically provide dependencies.

## Checking Inbox Messages

Use `scripts/check_messages.py` to retrieve new messages.

**What it does:**
- Polls Telegram API for new updates
- Downloads photos and documents to `~/Downloads/` (or `TELEGRAM_DOWNLOAD_DIR`)
- Displays message details (chat ID, user, timestamp, content)
- Marks messages as read after displaying

**Usage:**
```bash
env TELEGRAM_BOT_TOKEN="$(pass api/telegram)" ./scripts/check_messages.py
```

Optional: Set custom download directory with `TELEGRAM_DOWNLOAD_DIR` environment variable.

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

Use `scripts/send_message.py` to send content via Telegram.

### Send Text

```bash
env TELEGRAM_BOT_TOKEN="$(pass api/telegram)" ./scripts/send_message.py 12345 "Hello from Claude"
```

### Send Photo

```bash
env TELEGRAM_BOT_TOKEN="$(pass api/telegram)" ./scripts/send_message.py 12345 --photo ~/image.png --caption "Optional caption"
```

### Send Document

```bash
env TELEGRAM_BOT_TOKEN="$(pass api/telegram)" ./scripts/send_message.py 12345 --document ~/report.pdf --caption "Optional caption"
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

## Chat IDs

Chat IDs are required for sending messages. Users can get their chat ID by:
1. Checking inbox output (displays chat ID for each message)
2. Sending a message to the bot and checking inbox

## Stateless Operation

This skill is stateless. Telegram's API automatically tracks which messages have been read using update offsets. No local state storage needed.
