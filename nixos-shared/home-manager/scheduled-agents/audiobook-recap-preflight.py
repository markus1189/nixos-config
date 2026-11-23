#!/usr/bin/env python3
"""Decide whether the audiobook-recap agent has anything to do.

Prints one tab-separated line per book:
  RECAP  <book dir>  <old_s>  <new_s>  <total_s>  <epub>
  FAIL   <book dir>  <reason>
  BASELINE <book dir>  <pos_s>   (recap.md created, nothing to recap)
Exit 0 if any RECAP line was printed, 1 if not (FAIL lines alone don't
start the agent; the caller notifies about them).
"""
import argparse
import re
import struct
import sys
import time
from datetime import date
from pathlib import Path

RECAP_NAME = "recap.md"


PRIM = {"B": "b", "C": "H", "D": "d", "F": "f", "I": "i", "J": "q", "S": "h", "Z": "?"}


def read_position(dat: Path) -> tuple[int, int, int]:
    # Java serialization of BookDataBackup. Its field list differs between
    # player versions, so read it from the class descriptor instead of
    # assuming one layout (audiobook-buddy.md's fixed struct misreads some).
    data = dat.read_bytes()
    name = b"BookDataBackup"
    i = data.index(name) + len(name) + 8 + 1  # serialVersionUID, flags
    (n,) = struct.unpack_from(">H", data, i); i += 2
    fields = []
    for _ in range(n):
        code = chr(data[i]); i += 1
        (ln,) = struct.unpack_from(">H", data, i); i += 2
        fname = data[i:i + ln].decode(); i += ln
        if code in "L[":  # object field: TC_STRING (or TC_REFERENCE) with its type
            i += 3 + struct.unpack_from(">H", data, i + 1)[0] if data[i] == 0x74 else 5
        else:
            fields.append((fname, PRIM[code]))
    assert data[i:i + 2] == b"xp", f"unexpected descriptor end in {dat}"
    i += 2
    vals = dict(zip([f for f, _ in fields],
                    struct.unpack_from(">" + "".join(t for _, t in fields), data, i)))
    return (vals["mCurrentPlaybackTimeCache"], vals["mTotalPlaybackTimeCache"],
            vals["mLastPlaybackTime"])


def read_frontmatter(md: Path) -> dict[str, str]:
    if not md.exists():
        return {}
    lines = md.read_text().splitlines()
    if not lines or lines[0] != "---":
        return {}
    fm = {}
    for line in lines[1:]:
        if line == "---":
            break
        k, _, v = line.partition(":")
        fm[k.strip()] = v.strip().strip('"')
    return fm


def write_baseline(md: Path, pos: int, epub: Path | None) -> None:
    md.write_text(
        "---\n"
        f"last_position_s: {pos}\n"
        f"updated: {date.today().isoformat()}\n"
        f'epub: "{epub or ""}"\n'
        "---\n"
    )


def find_epub(book: Path, epubs: Path) -> tuple[Path | None, str]:
    # Audiobooks use "First Last/<Series> <n>", ePubs "Last, First/...<Series> <0n> - Title.epub".
    names = book.parent.name.split()
    if len(names) < 2:
        return None, f"author '{book.parent.name}' has no first/last name"
    first, last = names[0], names[-1]
    authors = [d for d in epubs.iterdir()
               if d.is_dir() and d.name.startswith(f"{last}, {first}")]
    if len(authors) != 1:
        return None, f"no ePubs dir '{last}, {first}*'"
    m = re.fullmatch(r"(.*?)\s*(\d+)?", book.name)
    series, num = m.group(1), m.group(2)
    pat = re.escape(series) + (rf"\s+0*{int(num)}\b" if num else "")
    hits = [p for p in authors[0].glob("*.epub") if re.search(pat, p.name, re.I)]
    if len(hits) != 1:
        return None, f"{len(hits)} ePub matches for /{pat}/ in {authors[0].name}"
    return hits[0], ""


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--audiobooks", type=Path, default=Path.home() / "Syncthing/Audiobooks")
    ap.add_argument("--epubs", type=Path, default=Path.home() / "Syncthing/ePubs")
    ap.add_argument("--min-diff", type=int, default=600, help="seconds")
    ap.add_argument("--new-book-days", type=float, default=2,
                    help="a book without recap.md played this recently is new: recap from 0")
    args = ap.parse_args()

    work = False
    for dat in sorted(args.audiobooks.glob("*/*/position.sabp.dat")):
        book = dat.parent
        cur, total, played_ms = read_position(dat)
        md = book / RECAP_NAME
        if not md.exists() and time.time() * 1000 - played_ms > args.new_book_days * 86400e3:
            epub, _ = find_epub(book, args.epubs)
            write_baseline(md, cur, epub)
            print(f"BASELINE\t{book}\t{cur}")
            continue
        fm = read_frontmatter(md)
        old = int(fm.get("last_position_s", 0))
        if cur - old < args.min_diff:
            continue
        epub = Path(fm["epub"]) if fm.get("epub") else None
        if epub is None or not epub.exists():
            epub, why = find_epub(book, args.epubs)
            if epub is None:
                print(f"FAIL\t{book}\t{why}")
                continue
        print(f"RECAP\t{book}\t{old}\t{cur}\t{total}\t{epub}")
        work = True
    return 0 if work else 1


if __name__ == "__main__":
    sys.exit(main())
