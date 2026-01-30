---
name: hackernews
description: "Fetches and displays Hacker News stories and comments via CLI. Use when the user asks about HN, Hacker News, tech stories, wants to search HN, or wants to read/analyze HN comments."
---

# Hacker News CLI

Fetch top stories, search, and view comments from Hacker News.

## Tool

**Script**: [`scripts/hn-cli.sh`](scripts/hn-cli.sh) (Nix shebang, self-contained)

## Commands

### List top stories
```bash
./scripts/hn-cli.sh              # Top 20 stories
./scripts/hn-cli.sh 50           # Top 50 stories
./scripts/hn-cli.sh --hot        # Hot stories only (🔥 score ≥300 or comments ≥100)
./scripts/hn-cli.sh --hot 10     # 10 hot stories
```

### Search stories (via Algolia)
```bash
./scripts/hn-cli.sh -s "rust programming"        # Search, recent first (default)
./scripts/hn-cli.sh --search "AI" --sort popular # Search by popularity/relevance
./scripts/hn-cli.sh -s "nix flakes" 50           # Search, show 50 results (max 100)
```

### View comments
```bash
./scripts/hn-cli.sh -c STORY_ID                  # Comments for story (depth 1, max 20)
./scripts/hn-cli.sh --comments STORY_ID -d 3     # Depth 3
./scripts/hn-cli.sh -c STORY_ID -n 100           # Up to 100 comments
```

## Output format

Stories show: rank, title, domain, story ID, points, author, age, comment count, URLs.
Comments show: tree structure with author, time, and text.

Story IDs appear in brackets `[12345678]` — use these for `--comments`.

## Typical workflows

1. **Browse HN**: Run with no args, scan titles
2. **Search for topics**: Use `-s "query"` to find stories on specific topics
3. **Dive into discussion**: Note story ID, run with `-c ID -d 2`
4. **Find hot topics**: Use `--hot` to filter for trending stories
5. **Research a topic**: Search with `-s`, then fetch comments for interesting stories
6. **Summarize for user**: Fetch stories + comments, summarize key points and insights
7. **Briefing mode**: See below

## Briefing Mode

When user asks casually about hacker news stories, use this style:

### Flow
1. Fetch top 20-50 stories, present hot/notable ones in a table
2. User picks stories they want to dig into
3. For each pick: fetch article (via `curl | pandoc`) + comments in parallel
4. Summarize with structure: article TL;DR, key quotes, HN discussion themes in tables
5. Group related stories together
6. When user asks "your take?" — give genuine opinions, not hedged summaries

### Tone
- **HN-native**: direct, slightly cynical, technically literate
- **Not corporate/PR**: have a voice, make judgments
- **Opinionated on request**: distinguish factual summary from editorial take

### Format
- Tables for quick scanning (story | points | comments | topic)
- Headers to separate stories
- Key quotes in blockquotes
- Discussion themes grouped by viewpoint
- Bullet points over paragraphs

## User Interests

Topics that consistently engage this user:

**Technical deep dives with stakes**: Benchmarks (especially flawed ones), security implications, architectural debates where the answer actually matters (e.g., AGENTS.md vs skills, PS2 FPU quirks)

**Drama + substance**: Naming controversies, governance issues, astroturfing, but only when there's real technical substance underneath the drama (not just gossip)

**Linux/Rust ecosystem**: Desktop environment innovation, Rust rewrites, immutable distros, window manager evolution

**AI agents (critical lens)**: Practical architecture, security nightmares, prompt injection risks, skill formation — interested in how they *actually* work and fail, not hype

**Meta-commentary**: HN discussion quality, when communities get things right vs cargo culting, spotting LLM-generated content

**Anti-patterns to highlight**: Security disasters waiting to happen, unfair benchmarks, overhyped tech with no clothes, projects that rebrand constantly

When summarizing: Structure matters. Categories, tables, direct quotes. Cynical takes alongside genuine analysis. Technical accuracy > hype.
