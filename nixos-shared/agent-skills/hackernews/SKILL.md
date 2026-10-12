---
name: hackernews
description: "Daily HN briefing agent with delta tracking, deep-dive sub-agent pipeline, user-interest-aware filtering, and topic research across HN stories and comments. Use when the user asks about HN, Hacker News, tech news, wants to check/browse HN, get a briefing, see what's new or hot, search stories or comments, find HN discussions on a topic, asks what HN thinks about something, read or analyze HN comments, or do a deep dive on a story."
---

# Hacker News CLI

Top stories, search over stories and comments, and full comment threads, through
[`scripts/hn-cli.sh`](scripts/hn-cli.sh). `--help` lists every flag.

## Commands

```bash
./scripts/hn-cli.sh 50                       # top 50 stories (default 20); --hot: 🔥 score ≥300 or comments ≥100
./scripts/hn-cli.sh -s "nix flakes"          # stories, newest first; --sort popular for relevance; COUNT up to 100
./scripts/hn-cli.sh -s arc42 --in comments   # comments: each hit with its story [id] and a link
./scripts/hn-cli.sh -s arc42 --in comments --compact  # one line per hit, for wide scans
./scripts/hn-cli.sh -s architecture --title-only --since "1 year ago" --min-points 50
./scripts/hn-cli.sh -t ID                    # whole thread, one request, 300 comments
./scripts/hn-cli.sh -t ID -n 800             # bigger budget
./scripts/hn-cli.sh -t ID --since "2026-10-08 07:40"  # only newer comments; ancestors as one-line context
./scripts/hn-cli.sh -t COMMENT_ID            # one comment and its replies, headed by its story
```

- **Search is exact**: typos are not matched and every word is required. `--fuzzy` turns
  Algolia's typo tolerance back on; with it, "arc42" also matches the user "abc42", and 51
  of 58 story hits were noise. Widen an empty search by dropping words or filters first.
- **`-t` fills its budget breadth-first**: every top-level comment in HN rank order, then
  replies level by level. The header's `showing N of M` tells what was left out.
- `-c ID` is the slow per-comment pretty tree (11 s for 20 comments; `-t` reads a whole
  thread in about 1 s). Use `-t` whenever you actually need to read a discussion.
- IDs appear in brackets `[12345678]`; every comment line carries its UTC timestamp.

## Which reference to read

| Task | Read |
| --- | --- |
| "check HN", "what's new", a briefing, the daily file | [references/briefing.md](references/briefing.md) |
| Deep dive on a story, comment mining, artifacts, high-profile commenters | [references/deep-dives.md](references/deep-dives.md) and [deepdive-prompt.md](deepdive-prompt.md) |
| "wrapup" | [references/wrapup.md](references/wrapup.md) |
| Ranking stories against the user's interests (briefings, dives) | [references/interests.md](references/interests.md) |
| Topic research ("HN discussions on X", "what does HN think about Y") | Research mode below |

## Research mode

A topic search, not a briefing.

- The answer goes **in the conversation only**: no `hn-daily.md`, no `kb-index`.
- Search **stories and comments**. Comments often carry the topic when no story does: on
  arc42 + AI agents, every relevant hit was a comment.
- Scan with `--compact` (full hits run 600 characters each: five queries of 25 overflowed
  tool output), then read the promising hits in full.
- Narrow before reading: `--since` for the era that matters (pre-LLM threads answer a
  different question), `--title-only`, `--min-points` to skip 1-point submissions.
- Read only threads that matched with `-t`. For a comment hit deep inside a big thread,
  `-t COMMENT_ID` reads its subthread instead of the whole story.
- Report every thread you cite with points, comment count and year. Say which threads you
  read in full and which you only saw as search hits.

**Script Execution:** Always invoke scripts by absolute path: resolve `./scripts/` against
this SKILL.md's directory (its install location varies by agent, e.g. `~/.claude/skills/` or
`~/.agents/skills/`). All scripts use Nix shebangs, so no dependency installation is needed.
