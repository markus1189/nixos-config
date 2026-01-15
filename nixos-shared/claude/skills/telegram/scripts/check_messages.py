#!/usr/bin/env nix
#! nix shell --impure --expr ``
#! nix with (import (builtins.getFlake "nixpkgs") {});
#! nix python3.withPackages (ps: with ps; [ pytelegrambotapi requests ])
#! nix ``
#! nix --command python3
"""
Check Telegram messages - retrieves new messages from the bot's inbox

Polls the Telegram Bot API and displays incoming messages with photos/documents.
Uses getUpdates with offset to mark messages as read.

Requirements: pyTelegramBotAPI (provided via Nix)
Environment: TELEGRAM_BOT_TOKEN must be set
"""

import os
import sys
import json
from datetime import datetime
from pathlib import Path

try:
    import telebot
    from telebot import types
except ImportError:
    print("Error: pyTelegramBotAPI not installed")
    print("This script uses Nix shebang to provide dependencies automatically")
    sys.exit(1)


def format_timestamp(unix_timestamp):
    """Convert Unix timestamp to readable format."""
    return datetime.fromtimestamp(unix_timestamp).strftime('%Y-%m-%d %H:%M:%S')


def get_download_dir():
    """Get download directory, creating it if needed."""
    # Use TELEGRAM_DOWNLOAD_DIR env var, or default to ~/Downloads
    download_dir = os.getenv('TELEGRAM_DOWNLOAD_DIR')
    if download_dir:
        path = Path(download_dir).expanduser()
    else:
        path = Path.home() / "Downloads"
    path.mkdir(parents=True, exist_ok=True)
    return path


def download_file(bot, file_id, file_type, chat_id, message_id):
    """Download a file from Telegram and save to download directory."""
    try:
        file_info = bot.get_file(file_id)
        downloaded_file = bot.download_file(file_info.file_path)

        # Generate filename in download directory
        file_extension = Path(file_info.file_path).suffix or ''
        filename = f"telegram_{chat_id}_{message_id}_{file_type}{file_extension}"
        filepath = get_download_dir() / filename

        with open(filepath, 'wb') as f:
            f.write(downloaded_file)

        return str(filepath)
    except Exception as e:
        return f"Error downloading file: {e}"


def check_messages():
    """Poll Telegram API for new messages and display them."""
    # Get bot token from environment
    token = os.getenv('TELEGRAM_BOT_TOKEN')
    if not token:
        print("Error: TELEGRAM_BOT_TOKEN environment variable not set")
        print("Set it with: export TELEGRAM_BOT_TOKEN='your-bot-token-here'")
        sys.exit(1)

    try:
        bot = telebot.TeleBot(token)

        # Get updates (Telegram automatically tracks offset)
        # timeout=0 for immediate return, no long-polling
        updates = bot.get_updates(timeout=0)

        if not updates:
            print("No new messages")
            return

        print(f"Found {len(updates)} new message(s):\n")

        for update in updates:
            if not update.message:
                continue

            msg = update.message
            chat = msg.chat
            user = msg.from_user

            # Basic message info
            print(f"━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
            print(f"Chat ID: {chat.id}")
            print(f"User: {user.first_name or ''} {user.last_name or ''}".strip())
            if user.username:
                print(f"Username: @{user.username}")
            print(f"Time: {format_timestamp(msg.date)}")
            print(f"Message ID: {msg.message_id}")

            # Message content
            if msg.text:
                print(f"Text: {msg.text}")

            # Handle photos
            if msg.photo:
                # Telegram sends multiple sizes, get the largest
                largest_photo = max(msg.photo, key=lambda p: p.file_size)
                filepath = download_file(bot, largest_photo.file_id, "photo", chat.id, msg.message_id)
                print(f"Photo: {filepath}")
                if msg.caption:
                    print(f"Caption: {msg.caption}")

            # Handle documents
            if msg.document:
                filepath = download_file(bot, msg.document.file_id, "document", chat.id, msg.message_id)
                print(f"Document: {msg.document.file_name}")
                print(f"Saved to: {filepath}")
                if msg.caption:
                    print(f"Caption: {msg.caption}")

            # Handle voice messages
            if msg.voice:
                filepath = download_file(bot, msg.voice.file_id, "voice", chat.id, msg.message_id)
                print(f"Voice message: {filepath}")

            # Handle video
            if msg.video:
                filepath = download_file(bot, msg.video.file_id, "video", chat.id, msg.message_id)
                print(f"Video: {filepath}")
                if msg.caption:
                    print(f"Caption: {msg.caption}")

            print()

        # Mark messages as read by getting updates with offset
        # This confirms we've processed up to the last update_id
        if updates:
            last_update_id = updates[-1].update_id
            bot.get_updates(offset=last_update_id + 1, timeout=0)

    except telebot.apihelper.ApiTelegramException as e:
        if e.error_code == 401:
            print("Error: Authentication failed - Invalid bot token")
        elif e.error_code == 429:
            print(f"Error: Telegram API rate limit reached")
            print(f"Retry after {e.result_json.get('parameters', {}).get('retry_after', 'unknown')} seconds")
        else:
            print(f"Telegram API Error ({e.error_code}): {e.description}")
        sys.exit(1)
    except Exception as e:
        print(f"Error: {e}")
        sys.exit(1)


if __name__ == "__main__":
    check_messages()
