#!/usr/bin/env nix
#! nix shell --impure --expr ``
#! nix with (import (builtins.getFlake "nixpkgs") {});
#! nix python3.withPackages (ps: with ps; [ pytelegrambotapi requests ])
#! nix ``
#! nix --command python3
"""
Send Telegram message - sends text, photos, or documents to a chat

Usage:
    send_message.py <chat_id> <text>                    # Send text
    send_message.py <chat_id> --photo <filepath>        # Send photo
    send_message.py <chat_id> --document <filepath>     # Send document
    send_message.py <chat_id> --photo <filepath> --caption "text"  # Photo with caption

Requirements: pyTelegramBotAPI (provided via Nix)
Environment: TELEGRAM_BOT_TOKEN must be set
"""

import os
import sys
from pathlib import Path

try:
    import telebot
    from telebot import types
except ImportError:
    print("Error: pyTelegramBotAPI not installed")
    print("This script uses Nix shebang to provide dependencies automatically")
    sys.exit(1)


def print_usage():
    """Print usage information."""
    print("Usage:")
    print("  send_message.py <chat_id> <text>                    # Send text message")
    print("  send_message.py <chat_id> --photo <filepath>        # Send photo")
    print("  send_message.py <chat_id> --document <filepath>     # Send document")
    print("  send_message.py <chat_id> --photo <path> --caption 'text'  # Photo with caption")
    print("\nExamples:")
    print("  send_message.py 12345 'Hello from the bot'")
    print("  send_message.py 12345 --photo ~/image.png")
    print("  send_message.py 12345 --document ~/report.pdf --caption 'Monthly report'")


def send_message(chat_id, message_type, content, caption=None):
    """
    Send a message via Telegram Bot API.

    Args:
        chat_id: Target chat ID
        message_type: 'text', 'photo', or 'document'
        content: Message text or file path
        caption: Optional caption for photos/documents
    """
    # Get bot token from environment
    token = os.getenv('TELEGRAM_BOT_TOKEN')
    if not token:
        print("Error: TELEGRAM_BOT_TOKEN environment variable not set")
        print("Set it with: export TELEGRAM_BOT_TOKEN='your-bot-token-here'")
        sys.exit(1)

    try:
        bot = telebot.TeleBot(token)

        # Try to parse chat_id as integer
        try:
            chat_id = int(chat_id)
        except ValueError:
            # Keep as string (could be @username or channel)
            pass

        # Send based on type
        if message_type == 'text':
            result = bot.send_message(chat_id, content)
            print(f"✅ Message sent to chat {chat_id}")
            print(f"Message ID: {result.message_id}")

        elif message_type == 'photo':
            filepath = Path(content).expanduser()
            if not filepath.exists():
                print(f"Error: Photo file not found: {filepath}")
                sys.exit(1)

            with open(filepath, 'rb') as photo:
                result = bot.send_photo(chat_id, photo, caption=caption)
            print(f"✅ Photo sent to chat {chat_id}")
            print(f"Message ID: {result.message_id}")

        elif message_type == 'document':
            filepath = Path(content).expanduser()
            if not filepath.exists():
                print(f"Error: Document file not found: {filepath}")
                sys.exit(1)

            with open(filepath, 'rb') as document:
                result = bot.send_document(chat_id, document, caption=caption)
            print(f"✅ Document sent to chat {chat_id}")
            print(f"Message ID: {result.message_id}")

    except telebot.apihelper.ApiTelegramException as e:
        if e.error_code == 400:
            print(f"Error: Bad request - {e.description}")
            print("Common causes:")
            print("  - Invalid chat_id")
            print("  - Message is too long")
            print("  - Invalid file format")
        elif e.error_code == 401:
            print("Error: Authentication failed - Invalid bot token")
        elif e.error_code == 403:
            print(f"Error: Cannot message chat {chat_id}")
            print("User hasn't started conversation with bot or has blocked it")
        elif e.error_code == 429:
            retry_after = e.result_json.get('parameters', {}).get('retry_after', 'unknown')
            print(f"Error: Telegram API rate limit reached")
            print(f"Retry after {retry_after} seconds")
        else:
            print(f"Telegram API Error ({e.error_code}): {e.description}")
        sys.exit(1)

    except Exception as e:
        print(f"Error: {e}")
        sys.exit(1)


def main():
    """Parse arguments and send message."""
    if len(sys.argv) < 3:
        print_usage()
        sys.exit(1)

    chat_id = sys.argv[1]

    # Parse arguments
    if '--photo' in sys.argv:
        photo_idx = sys.argv.index('--photo')
        if photo_idx + 1 >= len(sys.argv):
            print("Error: --photo requires a file path")
            sys.exit(1)
        filepath = sys.argv[photo_idx + 1]

        caption = None
        if '--caption' in sys.argv:
            caption_idx = sys.argv.index('--caption')
            if caption_idx + 1 >= len(sys.argv):
                print("Error: --caption requires text")
                sys.exit(1)
            caption = sys.argv[caption_idx + 1]

        send_message(chat_id, 'photo', filepath, caption)

    elif '--document' in sys.argv:
        doc_idx = sys.argv.index('--document')
        if doc_idx + 1 >= len(sys.argv):
            print("Error: --document requires a file path")
            sys.exit(1)
        filepath = sys.argv[doc_idx + 1]

        caption = None
        if '--caption' in sys.argv:
            caption_idx = sys.argv.index('--caption')
            if caption_idx + 1 >= len(sys.argv):
                print("Error: --caption requires text")
                sys.exit(1)
            caption = sys.argv[caption_idx + 1]

        send_message(chat_id, 'document', filepath, caption)

    else:
        # Everything after chat_id is the message text
        message_text = ' '.join(sys.argv[2:])
        send_message(chat_id, 'text', message_text)


if __name__ == "__main__":
    main()
