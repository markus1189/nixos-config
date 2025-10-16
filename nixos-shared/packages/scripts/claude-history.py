"""
Convert Claude Code conversation JSONL files to Markdown.

Usage:
    claude-history view <conversation-id>
    claude-history search <pattern> [--user] [--assistant] [--all]

Commands:
    view        Convert conversation to markdown
    search      Search for pattern in conversations

Examples:
    claude-history view e80fb85c
    claude-history search "nixpkgs update"
    claude-history search "error" --assistant
"""

import sys
import json
import os
import re
from pathlib import Path
from datetime import datetime
from typing import Dict, List, Optional, Any, Tuple


def get_all_conversation_files() -> List[Path]:
    """Get all conversation JSONL files."""
    projects_dir = Path.home() / ".claude" / "projects"

    if not projects_dir.exists():
        return []

    conversations = []
    for project_dir in projects_dir.iterdir():
        if not project_dir.is_dir():
            continue

        for jsonl_file in project_dir.glob("*.jsonl"):
            conversations.append(jsonl_file)

    return conversations


def find_conversation_file(conv_id: str) -> Optional[Path]:
    """Find conversation JSONL file by full or partial UUID."""
    for jsonl_file in get_all_conversation_files():
        filename = jsonl_file.stem
        if filename == conv_id or filename.startswith(conv_id):
            return jsonl_file

    return None


def parse_conversation(jsonl_path: Path) -> List[Dict[str, Any]]:
    """Parse JSONL conversation file into list of messages."""
    messages = []

    with open(jsonl_path, 'r', encoding='utf-8') as f:
        for line in f:
            line = line.strip()
            if line:
                try:
                    messages.append(json.loads(line))
                except json.JSONDecodeError as e:
                    print(f"Warning: Failed to parse line: {e}", file=sys.stderr)

    return messages


def format_timestamp(ts_str: str) -> str:
    """Format ISO timestamp to readable string."""
    try:
        dt = datetime.fromisoformat(ts_str.replace('Z', '+00:00'))
        return dt.strftime('%Y-%m-%d %H:%M:%S')
    except:
        return ts_str


def escape_markdown(text: str) -> str:
    """Escape special markdown characters in text."""
    # Don't escape if it looks like it's already formatted markdown
    return text


def extract_text_from_content(content: Any) -> str:
    """Extract plain text from message content."""
    text_parts = []

    if isinstance(content, str):
        return content

    if isinstance(content, list):
        for item in content:
            if isinstance(item, dict):
                content_type = item.get('type', 'unknown')
                if content_type == 'text':
                    text_parts.append(item.get('text', ''))
                elif content_type == 'thinking':
                    text_parts.append(item.get('thinking', ''))
                elif content_type == 'tool_result':
                    text_parts.append(str(item.get('content', '')))
            elif isinstance(item, str):
                text_parts.append(item)

    return " ".join(text_parts)


def get_snippet(text: str, pattern: str, context_chars: int = 100) -> str:
    """Get a snippet of text around the first match of pattern."""
    try:
        match = re.search(pattern, text, re.IGNORECASE)
        if not match:
            return text[:150] + "..." if len(text) > 150 else text

        start = max(0, match.start() - context_chars)
        end = min(len(text), match.end() + context_chars)

        snippet = text[start:end]

        # Add ellipsis if truncated
        if start > 0:
            snippet = "..." + snippet
        if end < len(text):
            snippet = snippet + "..."

        return snippet
    except re.error:
        # If pattern is invalid regex, treat as literal string
        idx = text.lower().find(pattern.lower())
        if idx == -1:
            return text[:150] + "..." if len(text) > 150 else text

        start = max(0, idx - context_chars)
        end = min(len(text), idx + len(pattern) + context_chars)
        snippet = text[start:end]

        if start > 0:
            snippet = "..." + snippet
        if end < len(text):
            snippet = snippet + "..."

        return snippet


def format_message_content(content: Any, indent: int = 0) -> str:
    """Format message content (can be string or list of content blocks)."""
    prefix = "  " * indent
    output = []

    if isinstance(content, str):
        return f"{prefix}{content}"

    if isinstance(content, list):
        for item in content:
            if isinstance(item, dict):
                content_type = item.get('type', 'unknown')

                if content_type == 'text':
                    text = item.get('text', '')
                    output.append(f"{prefix}{text}")

                elif content_type == 'thinking':
                    thinking = item.get('thinking', '')
                    output.append(f"{prefix}**[Thinking]**")
                    output.append(f"{prefix}```")
                    output.append(f"{prefix}{thinking}")
                    output.append(f"{prefix}```")

                elif content_type == 'tool_use':
                    tool_name = item.get('name', 'unknown')
                    tool_id = item.get('id', '')
                    tool_input = item.get('input', {})
                    output.append(f"{prefix}**[Tool Use: {tool_name}]** `{tool_id}`")
                    output.append(f"{prefix}```json")
                    output.append(f"{prefix}{json.dumps(tool_input, indent=2)}")
                    output.append(f"{prefix}```")

                elif content_type == 'tool_result':
                    tool_use_id = item.get('tool_use_id', '')
                    result_content = item.get('content', '')
                    is_error = item.get('is_error', False)
                    status = "ERROR" if is_error else "Result"
                    output.append(f"{prefix}**[Tool {status}]** `{tool_use_id}`")
                    output.append(f"{prefix}```")
                    output.append(f"{prefix}{result_content}")
                    output.append(f"{prefix}```")

                else:
                    output.append(f"{prefix}**[{content_type}]**")
                    output.append(f"{prefix}```json")
                    output.append(f"{prefix}{json.dumps(item, indent=2)}")
                    output.append(f"{prefix}```")

            elif isinstance(item, str):
                output.append(f"{prefix}{item}")

    return "\n".join(output)


def generate_markdown(messages: List[Dict[str, Any]]) -> str:
    """Generate markdown from parsed messages."""
    output = []

    # Extract metadata from first few messages
    session_info = None
    summary = None

    for msg in messages[:5]:
        if msg.get('type') == 'summary':
            summary = msg.get('summary', '')
        elif msg.get('sessionId'):
            session_info = msg

    # Header
    output.append("# Claude Code Conversation")
    output.append("")

    if summary:
        output.append(f"**Summary:** {summary}")
        output.append("")

    if session_info:
        output.append("## Session Info")
        output.append("")
        output.append(f"- **Session ID:** `{session_info.get('sessionId', 'N/A')}`")
        output.append(f"- **Version:** {session_info.get('version', 'N/A')}")
        output.append(f"- **Working Directory:** `{session_info.get('cwd', 'N/A')}`")
        if session_info.get('gitBranch'):
            output.append(f"- **Git Branch:** `{session_info.get('gitBranch')}`")
        output.append("")

    output.append("---")
    output.append("")
    output.append("## Conversation")
    output.append("")

    # Process messages chronologically
    for i, msg in enumerate(messages, 1):
        msg_type = msg.get('type', 'unknown')

        if msg_type == 'summary' or msg_type == 'file-history-snapshot':
            continue

        timestamp = format_timestamp(msg.get('timestamp', ''))
        uuid = msg.get('uuid', '')

        if msg_type == 'user':
            output.append(f"### [{i}] User Message")
            output.append(f"*{timestamp}* • `{uuid}`")
            output.append("")

            message = msg.get('message', {})
            content = message.get('content', '')
            output.append(format_message_content(content))
            output.append("")

        elif msg_type == 'assistant':
            output.append(f"### [{i}] Assistant Message")
            output.append(f"*{timestamp}* • `{uuid}`")
            output.append("")

            message = msg.get('message', {})
            content = message.get('content', [])
            output.append(format_message_content(content))

            # Usage stats if available
            usage = message.get('usage', {})
            if usage:
                output.append("")
                output.append("<details>")
                output.append("<summary>Token Usage</summary>")
                output.append("")
                output.append(f"- Input tokens: {usage.get('input_tokens', 0)}")
                output.append(f"- Output tokens: {usage.get('output_tokens', 0)}")
                if usage.get('cache_read_input_tokens'):
                    output.append(f"- Cache read: {usage.get('cache_read_input_tokens', 0)}")
                if usage.get('cache_creation_input_tokens'):
                    output.append(f"- Cache creation: {usage.get('cache_creation_input_tokens', 0)}")
                output.append("</details>")

            output.append("")

        elif msg_type == 'system':
            subtype = msg.get('subtype', '')
            content = msg.get('content', '')
            output.append(f"### [{i}] System Message ({subtype})")
            output.append(f"*{timestamp}* • `{uuid}`")
            output.append("")
            output.append(f"```")
            output.append(content)
            output.append(f"```")
            output.append("")

        else:
            output.append(f"### [{i}] {msg_type.title()}")
            output.append(f"*{timestamp}* • `{uuid}`")
            output.append("")
            output.append("```json")
            output.append(json.dumps(msg, indent=2))
            output.append("```")
            output.append("")

    return "\n".join(output)


def search_conversations(pattern: str, search_user: bool = True, search_assistant: bool = True) -> List[Tuple[Path, str, str, str]]:
    """
    Search for pattern in conversations.

    Returns: List of (file_path, session_id, message_type, snippet) tuples
    """
    results = []

    for jsonl_file in get_all_conversation_files():
        try:
            messages = parse_conversation(jsonl_file)
            session_id = jsonl_file.stem
            summary = None

            # Extract summary if available
            for msg in messages[:5]:
                if msg.get('type') == 'summary':
                    summary = msg.get('summary', '')
                    break

            # Search through messages
            for msg in messages:
                msg_type = msg.get('type', '')

                if msg_type == 'user' and search_user:
                    message = msg.get('message', {})
                    content = message.get('content', '')
                    text = extract_text_from_content(content)

                    try:
                        if re.search(pattern, text, re.IGNORECASE):
                            snippet = get_snippet(text, pattern)
                            timestamp = format_timestamp(msg.get('timestamp', ''))
                            result_text = f"[USER {timestamp}] {snippet}"
                            if summary:
                                result_text = f"({summary}) {result_text}"
                            results.append((jsonl_file, session_id, 'user', result_text))
                    except re.error:
                        # Fallback to literal string search
                        if pattern.lower() in text.lower():
                            snippet = get_snippet(text, pattern)
                            timestamp = format_timestamp(msg.get('timestamp', ''))
                            result_text = f"[USER {timestamp}] {snippet}"
                            if summary:
                                result_text = f"({summary}) {result_text}"
                            results.append((jsonl_file, session_id, 'user', result_text))

                elif msg_type == 'assistant' and search_assistant:
                    message = msg.get('message', {})
                    content = message.get('content', [])
                    text = extract_text_from_content(content)

                    try:
                        if re.search(pattern, text, re.IGNORECASE):
                            snippet = get_snippet(text, pattern)
                            timestamp = format_timestamp(msg.get('timestamp', ''))
                            result_text = f"[ASSISTANT {timestamp}] {snippet}"
                            if summary:
                                result_text = f"({summary}) {result_text}"
                            results.append((jsonl_file, session_id, 'assistant', result_text))
                    except re.error:
                        # Fallback to literal string search
                        if pattern.lower() in text.lower():
                            snippet = get_snippet(text, pattern)
                            timestamp = format_timestamp(msg.get('timestamp', ''))
                            result_text = f"[ASSISTANT {timestamp}] {snippet}"
                            if summary:
                                result_text = f"({summary}) {result_text}"
                            results.append((jsonl_file, session_id, 'assistant', result_text))

        except Exception as e:
            # Skip files that can't be parsed
            continue

    return results


def cmd_view(conv_id: str):
    """View command: convert conversation to markdown."""
    jsonl_path = find_conversation_file(conv_id)
    if not jsonl_path:
        print(f"Error: Could not find conversation matching '{conv_id}'", file=sys.stderr)
        sys.exit(1)

    print(f"Found: {jsonl_path}", file=sys.stderr)

    messages = parse_conversation(jsonl_path)
    markdown = generate_markdown(messages)
    print(markdown)


def cmd_search(pattern: str, args: List[str]):
    """Search command: search for pattern in conversations."""
    search_user = True
    search_assistant = True

    # Parse flags
    if '--user' in args:
        search_assistant = False
    if '--assistant' in args:
        search_user = False
    if '--all' in args:
        search_user = True
        search_assistant = True

    print(f"Searching for: '{pattern}'", file=sys.stderr)
    results = search_conversations(pattern, search_user, search_assistant)

    if not results:
        print("No matches found.", file=sys.stderr)
        sys.exit(0)

    print(f"Found {len(results)} match(es):\n", file=sys.stderr)

    # Group results by session
    sessions = {}
    for file_path, session_id, msg_type, snippet in results:
        if session_id not in sessions:
            sessions[session_id] = []
        sessions[session_id].append((msg_type, snippet))

    # Output results
    for session_id, matches in sessions.items():
        print(f"\n{session_id}")
        for msg_type, snippet in matches:
            print(f"  {snippet}")


def main():
    if len(sys.argv) < 2:
        print(__doc__)
        sys.exit(1)

    command = sys.argv[1]

    if command == 'view':
        if len(sys.argv) < 3:
            print("Error: 'view' requires a conversation ID", file=sys.stderr)
            print(__doc__)
            sys.exit(1)
        cmd_view(sys.argv[2])

    elif command == 'search':
        if len(sys.argv) < 3:
            print("Error: 'search' requires a pattern", file=sys.stderr)
            print(__doc__)
            sys.exit(1)
        cmd_search(sys.argv[2], sys.argv[3:])

    else:
        print(f"Error: Unknown command '{command}'", file=sys.stderr)
        print(__doc__)
        sys.exit(1)


if __name__ == "__main__":
    main()
