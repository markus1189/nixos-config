"""
MPV Watch Later Overview Script
Shows a formatted list of videos with resume positions.
"""

import os
import sys
from pathlib import Path
from datetime import datetime


def format_time(seconds):
    """Convert seconds to HH:MM:SS format."""
    seconds = float(seconds)
    hours = int(seconds // 3600)
    minutes = int((seconds % 3600) // 60)
    secs = int(seconds % 60)
    return f"{hours:02d}:{minutes:02d}:{secs:02d}"


def parse_watch_later_file(filepath):
    """Parse a watch_later config file and extract relevant info."""
    data = {
        'filename': None,
        'start': 0.0,
        'volume': None,
        'pause': None,
        'aid': None,
        'sid': None,
        'ab_loop_a': None,
        'ab_loop_b': None,
        'is_redirect': False
    }

    try:
        with open(filepath, 'r', encoding='utf-8', errors='ignore') as f:
            for line in f:
                line = line.strip()
                if line.startswith('# redirect entry'):
                    data['is_redirect'] = True
                elif line.startswith('# '):
                    data['filename'] = line[2:]
                elif '=' in line:
                    key, value = line.split('=', 1)
                    if key == 'start':
                        data['start'] = float(value)
                    elif key == 'volume':
                        data['volume'] = float(value)
                    elif key == 'pause':
                        data['pause'] = value == 'yes'
                    elif key == 'aid':
                        data['aid'] = value
                    elif key == 'sid':
                        data['sid'] = value
                    elif key == 'ab-loop-a':
                        data['ab_loop_a'] = float(value)
                    elif key == 'ab-loop-b':
                        data['ab_loop_b'] = float(value)
    except Exception as e:
        print(f"Error reading {filepath}: {e}", file=sys.stderr)
        return None

    return data


def get_file_age(filepath):
    """Get the age of the file in days."""
    mtime = os.path.getmtime(filepath)
    age = datetime.now() - datetime.fromtimestamp(mtime)
    return age


def main():
    watch_later_dir = Path.home() / '.local' / 'state' / 'mpv' / 'watch_later'

    if not watch_later_dir.exists():
        print(f"No watch_later directory found at {watch_later_dir}")
        sys.exit(1)

    # Get all watch_later files sorted by modification time (most recent first)
    files = sorted(
        watch_later_dir.glob('*'),
        key=lambda f: f.stat().st_mtime,
        reverse=True
    )

    print("=" * 80)
    print("MPV WATCH LATER OVERVIEW")
    print("=" * 80)
    print()

    entries = []
    for filepath in files:
        if not filepath.is_file():
            continue

        data = parse_watch_later_file(filepath)
        if not data:
            continue

        # Skip redirect entries
        if data['is_redirect']:
            continue

        # Skip if no filename found
        if not data['filename']:
            data['filename'] = "Unknown file"

        age = get_file_age(filepath)
        data['age'] = age
        data['filepath'] = filepath
        entries.append(data)

    # Display all entries
    for i, data in enumerate(entries, 1):
        # Extract just the filename from full path
        full_path = data['filename']
        basename = os.path.basename(full_path)

        # Age indicator
        age_days = data['age'].days
        if age_days == 0:
            age_str = "Today"
        elif age_days == 1:
            age_str = "Yesterday"
        elif age_days < 7:
            age_str = f"{age_days}d ago"
        elif age_days < 30:
            age_str = f"{age_days // 7}w ago"
        else:
            age_str = f"{age_days // 30}mo ago"

        # Get ISO 8601 timestamp
        mtime = os.path.getmtime(data['filepath'])
        timestamp = datetime.fromtimestamp(mtime).isoformat()

        print(f"[{i:2d}] {basename}")
        pos_time = format_time(data['start'])
        print(f"     Position: {pos_time} ({data['start']:.1f}s)")
        print(f"     Last Watched: {timestamp} ({age_str})")

        # Additional info
        extras = []
        if data['pause']:
            extras.append("⏸ PAUSED")
        if data['volume'] and data['volume'] != 100.0:
            extras.append(f"Vol: {data['volume']:.0f}%")
        if data['ab_loop_a'] is not None and data['ab_loop_b'] is not None:
            loop_a = format_time(data['ab_loop_a'])
            loop_b = format_time(data['ab_loop_b'])
            extras.append(f"A-B Loop: {loop_a}-{loop_b}")
        if data['aid']:
            extras.append(f"Audio: {data['aid']}")
        if data['sid']:
            extras.append(f"Sub: {data['sid']}")

        if extras:
            print(f"     {' | '.join(extras)}")

        # Show directory path if different from basename
        if basename != full_path:
            dirname = os.path.dirname(full_path)
            print(f"     📁 {dirname}")

        # Show watch_later config file path
        print(f"     🔧 {data['filepath']}")

        print()

    print("=" * 80)
    print(f"Total watch_later files: {len(entries)}")
    print("=" * 80)


if __name__ == '__main__':
    main()
