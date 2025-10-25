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
import re
from pathlib import Path
from datetime import datetime
from typing import Dict, List, Optional, Any, Tuple

try:
    from colorama import Fore, Back, Style, init
    init(autoreset=True)
    HAS_COLOR = True
except ImportError:
    # Fallback if colorama is not available
    HAS_COLOR = False

    class MockColor:
        def __getattr__(self, name):
            return ''

    Fore = Back = Style = MockColor()


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
                    print(f"Warning: Failed to parse line: {e}",
                          file=sys.stderr)

    return messages


def format_timestamp(ts_str: str, colored: bool = True) -> str:
    """Format ISO timestamp to readable string."""
    try:
        dt = datetime.fromisoformat(ts_str.replace('Z', '+00:00'))
        ts = dt.strftime('%Y-%m-%d %H:%M:%S')
        if colored and HAS_COLOR:
            return f"{Fore.CYAN}{ts}{Style.RESET_ALL}"
        return ts
    except Exception:
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


def format_message_content(
        content: Any, indent: int = 0, colored: bool = True) -> str:
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
                    if colored and HAS_COLOR:
                        thinking_label = (
                            f"{Fore.MAGENTA}**[Thinking]**"
                            f"{Style.RESET_ALL}"
                        )
                    else:
                        thinking_label = "**[Thinking]**"
                    output.append(f"{prefix}{thinking_label}")
                    output.append(f"{prefix}```")
                    output.append(f"{prefix}{thinking}")
                    output.append(f"{prefix}```")

                elif content_type == 'tool_use':
                    tool_name = item.get('name', 'unknown')
                    tool_id = item.get('id', '')
                    tool_input = item.get('input', {})
                    if colored and HAS_COLOR:
                        label = (
                            f"{Fore.YELLOW}**[Tool Use: {tool_name}]**"
                            f"{Style.RESET_ALL} "
                            f"{Fore.BLUE}`{tool_id}`{Style.RESET_ALL}"
                        )
                    else:
                        label = f"**[Tool Use: {tool_name}]** `{tool_id}`"
                    output.append(f"{prefix}{label}")
                    output.append(f"{prefix}```json")
                    output.append(
                        f"{prefix}{json.dumps(tool_input, indent=2)}")
                    output.append(f"{prefix}```")

                elif content_type == 'tool_result':
                    tool_use_id = item.get('tool_use_id', '')
                    result_content = item.get('content', '')
                    is_error = item.get('is_error', False)
                    status = "ERROR" if is_error else "Result"
                    if colored and HAS_COLOR:
                        color = Fore.RED if is_error else Fore.GREEN
                        label = (
                            f"{color}**[Tool {status}]**"
                            f"{Style.RESET_ALL} "
                            f"{Fore.BLUE}`{tool_use_id}`{Style.RESET_ALL}"
                        )
                    else:
                        label = f"**[Tool {status}]** `{tool_use_id}`"
                    output.append(f"{prefix}{label}")
                    output.append(f"{prefix}```")
                    output.append(f"{prefix}{result_content}")
                    output.append(f"{prefix}```")

                else:
                    if colored and HAS_COLOR:
                        type_label = (
                            f"{Fore.CYAN}**[{content_type}]**"
                            f"{Style.RESET_ALL}"
                        )
                    else:
                        type_label = f"**[{content_type}]**"
                    output.append(f"{prefix}{type_label}")
                    output.append(f"{prefix}```json")
                    output.append(f"{prefix}{json.dumps(item, indent=2)}")
                    output.append(f"{prefix}```")

            elif isinstance(item, str):
                output.append(f"{prefix}{item}")

    return "\n".join(output)


def generate_markdown(
        messages: List[Dict[str, Any]], colored: bool = True) -> str:
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
    if colored and HAS_COLOR:
        title = (
            f"{Fore.GREEN}# Claude Code Conversation{Style.RESET_ALL}"
        )
    else:
        title = "# Claude Code Conversation"
    output.append(title)
    output.append("")

    if summary:
        if colored and HAS_COLOR:
            summary_text = (
                f"{Fore.YELLOW}**Summary:**{Style.RESET_ALL} {summary}"
            )
        else:
            summary_text = f"**Summary:** {summary}"
        output.append(summary_text)
        output.append("")

    if session_info:
        if colored and HAS_COLOR:
            section_header = (
                f"{Fore.GREEN}## Session Info{Style.RESET_ALL}"
            )
        else:
            section_header = "## Session Info"
        output.append(section_header)
        output.append("")
        session_id = session_info.get('sessionId', 'N/A')
        if colored and HAS_COLOR:
            output.append(
                f"- **Session ID:** "
                f"{Fore.BLUE}`{session_id}`{Style.RESET_ALL}"
            )
            version = session_info.get('version', 'N/A')
            output.append(
                f"- **Version:** "
                f"{Fore.BLUE}{version}{Style.RESET_ALL}"
            )
            cwd = session_info.get('cwd', 'N/A')
            output.append(
                f"- **Working Directory:** "
                f"{Fore.BLUE}`{cwd}`{Style.RESET_ALL}"
            )
            if session_info.get('gitBranch'):
                branch = session_info.get('gitBranch')
                output.append(
                    f"- **Git Branch:** "
                    f"{Fore.BLUE}`{branch}`{Style.RESET_ALL}"
                )
        else:
            output.append(f"- **Session ID:** `{session_id}`")
            output.append(
                f"- **Version:** {session_info.get('version', 'N/A')}"
            )
            cwd = session_info.get('cwd', 'N/A')
            output.append(f"- **Working Directory:** `{cwd}`")
            if session_info.get('gitBranch'):
                branch = session_info.get('gitBranch')
                output.append(f"- **Git Branch:** `{branch}`")
        output.append("")

    output.append("---")
    output.append("")
    if colored and HAS_COLOR:
        conv_header = f"{Fore.GREEN}## Conversation{Style.RESET_ALL}"
    else:
        conv_header = "## Conversation"
    output.append(conv_header)
    output.append("")

    # Process messages chronologically
    for i, msg in enumerate(messages, 1):
        msg_type = msg.get('type', 'unknown')

        if msg_type == 'summary' or msg_type == 'file-history-snapshot':
            continue

        timestamp = format_timestamp(msg.get('timestamp', ''), colored)
        uuid = msg.get('uuid', '')
        if colored and HAS_COLOR:
            uuid_colored = f"{Fore.BLUE}`{uuid}`{Style.RESET_ALL}"
        else:
            uuid_colored = f"`{uuid}`"

        if msg_type == 'user':
            if colored and HAS_COLOR:
                msg_header = (
                    f"{Fore.CYAN}### [{i}] User Message"
                    f"{Style.RESET_ALL}"
                )
            else:
                msg_header = f"### [{i}] User Message"
            output.append(msg_header)
            output.append(f"*{timestamp}* • {uuid_colored}")
            output.append("")

            message = msg.get('message', {})
            content = message.get('content', '')
            output.append(format_message_content(content, colored=colored))
            output.append("")

        elif msg_type == 'assistant':
            if colored and HAS_COLOR:
                msg_header = (
                    f"{Fore.MAGENTA}### [{i}] Assistant Message"
                    f"{Style.RESET_ALL}"
                )
            else:
                msg_header = f"### [{i}] Assistant Message"
            output.append(msg_header)
            output.append(f"*{timestamp}* • {uuid_colored}")
            output.append("")

            message = msg.get('message', {})
            content = message.get('content', [])
            output.append(format_message_content(content, colored=colored))

            # Usage stats if available
            usage = message.get('usage', {})
            if usage:
                output.append("")
                output.append("<details>")
                output.append("<summary>Token Usage</summary>")
                output.append("")
                input_tok = usage.get('input_tokens', 0)
                output.append(f"- Input tokens: {input_tok}")
                output_tok = usage.get('output_tokens', 0)
                output.append(f"- Output tokens: {output_tok}")
                if usage.get('cache_read_input_tokens'):
                    cache_read = usage.get('cache_read_input_tokens', 0)
                    output.append(f"- Cache read: {cache_read}")
                if usage.get('cache_creation_input_tokens'):
                    cache_create = usage.get('cache_creation_input_tokens', 0)
                    output.append(f"- Cache creation: {cache_create}")
                output.append("</details>")

            output.append("")

        elif msg_type == 'system':
            subtype = msg.get('subtype', '')
            content = msg.get('content', '')
            if colored and HAS_COLOR:
                msg_header = (
                    f"{Fore.YELLOW}### [{i}] System Message "
                    f"({subtype}){Style.RESET_ALL}"
                )
            else:
                msg_header = f"### [{i}] System Message ({subtype})"
            output.append(msg_header)
            output.append(f"*{timestamp}* • {uuid_colored}")
            output.append("")
            output.append("```")
            output.append(content)
            output.append("```")
            output.append("")

        else:
            if colored and HAS_COLOR:
                msg_header = (
                    f"{Fore.WHITE}### [{i}] {msg_type.title()}"
                    f"{Style.RESET_ALL}"
                )
            else:
                msg_header = f"### [{i}] {msg_type.title()}"
            output.append(msg_header)
            output.append(f"*{timestamp}* • {uuid_colored}")
            output.append("")
            output.append("```json")
            output.append(json.dumps(msg, indent=2))
            output.append("```")
            output.append("")

    return "\n".join(output)


def search_conversations(
        pattern: str,
        search_user: bool = True,
        search_assistant: bool = True) -> List[Tuple[Path, str, str, str, str]]:
    """
    Search for pattern in conversations.

    Returns: List of (file_path, session_id, message_type, snippet, timestamp) tuples
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
                            timestamp = msg.get('timestamp', '')
                            ts = format_timestamp(timestamp)
                            result_text = f"[USER {ts}] {snippet}"
                            if summary:
                                result_text = f"({summary}) {result_text}"
                            results.append(
                                (jsonl_file, session_id, 'user',
                                 result_text, timestamp))
                    except re.error:
                        # Fallback to literal string search
                        if pattern.lower() in text.lower():
                            snippet = get_snippet(text, pattern)
                            timestamp = msg.get('timestamp', '')
                            ts = format_timestamp(timestamp)
                            result_text = f"[USER {ts}] {snippet}"
                            if summary:
                                result_text = f"({summary}) {result_text}"
                            results.append(
                                (jsonl_file, session_id, 'user',
                                 result_text, timestamp))

                elif msg_type == 'assistant' and search_assistant:
                    message = msg.get('message', {})
                    content = message.get('content', [])
                    text = extract_text_from_content(content)

                    try:
                        if re.search(pattern, text, re.IGNORECASE):
                            snippet = get_snippet(text, pattern)
                            timestamp = msg.get('timestamp', '')
                            ts = format_timestamp(timestamp)
                            result_text = f"[ASSISTANT {ts}] {snippet}"
                            if summary:
                                result_text = f"({summary}) {result_text}"
                            results.append(
                                (jsonl_file, session_id, 'assistant',
                                 result_text, timestamp))
                    except re.error:
                        # Fallback to literal string search
                        if pattern.lower() in text.lower():
                            snippet = get_snippet(text, pattern)
                            timestamp = msg.get('timestamp', '')
                            ts = format_timestamp(timestamp)
                            result_text = f"[ASSISTANT {ts}] {snippet}"
                            if summary:
                                result_text = f"({summary}) {result_text}"
                            results.append(
                                (jsonl_file, session_id, 'assistant',
                                 result_text, timestamp))

        except Exception:
            # Skip files that can't be parsed
            continue

    return results


def cmd_view(conv_id: str):
    """View command: convert conversation to markdown."""
    jsonl_path = find_conversation_file(conv_id)
    if not jsonl_path:
        if HAS_COLOR:
            error_msg = (
                f"{Fore.RED}Error: Could not find conversation matching "
                f"'{conv_id}'{Style.RESET_ALL}"
            )
        else:
            error_msg = (
                f"Error: Could not find conversation matching '{conv_id}'"
            )
        print(error_msg, file=sys.stderr)
        sys.exit(1)

    if HAS_COLOR:
        found_msg = f"{Fore.GREEN}Found: {jsonl_path}{Style.RESET_ALL}"
    else:
        found_msg = f"Found: {jsonl_path}"
    print(found_msg, file=sys.stderr)

    messages = parse_conversation(jsonl_path)
    markdown = generate_markdown(messages, colored=HAS_COLOR)
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

    if HAS_COLOR:
        search_msg = (
            f"{Fore.CYAN}Searching for: '{pattern}'{Style.RESET_ALL}"
        )
    else:
        search_msg = f"Searching for: '{pattern}'"
    print(search_msg, file=sys.stderr)
    results = search_conversations(pattern, search_user, search_assistant)

    if not results:
        if HAS_COLOR:
            no_match_msg = (
                f"{Fore.YELLOW}No matches found.{Style.RESET_ALL}"
            )
        else:
            no_match_msg = "No matches found."
        print(no_match_msg, file=sys.stderr)
        sys.exit(0)

    if HAS_COLOR:
        found_msg = (
            f"{Fore.GREEN}Found {len(results)} match(es):"
            f"{Style.RESET_ALL}\n"
        )
    else:
        found_msg = f"Found {len(results)} match(es):\n"
    print(found_msg, file=sys.stderr)

    # Group results by session and track most recent timestamp
    sessions = {}
    session_timestamps = {}
    for file_path, session_id, msg_type, snippet, timestamp in results:
        if session_id not in sessions:
            sessions[session_id] = []
            session_timestamps[session_id] = timestamp
        sessions[session_id].append((msg_type, snippet))
        # Keep track of the most recent timestamp for this session
        if timestamp > session_timestamps[session_id]:
            session_timestamps[session_id] = timestamp

    # Output results sorted by most recent timestamp first
    sorted_sessions = sorted(
        sessions.items(),
        key=lambda x: session_timestamps[x[0]],
        reverse=True
    )
    for session_id, matches in sorted_sessions:
        if HAS_COLOR:
            session_header = (
                f"\n{Fore.BLUE}{session_id}{Style.RESET_ALL}"
            )
        else:
            session_header = f"\n{session_id}"
        print(session_header)
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
