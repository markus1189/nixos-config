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
5. **Research a topic**: Search with `-s`, then fetch comments for interesting stories
6. **Summarize for user**: Fetch stories + comments, summarize key points and insights
7. **Briefing mode**: See below

## Deep-Dive Sub-Agent

**Script**: [`scripts/hn-deepdive.sh`](scripts/hn-deepdive.sh)

Spawns an isolated `pi` subprocess to fetch article + comments and return a structured markdown summary. This keeps the raw content out of the main context.

```bash
./scripts/hn-deepdive.sh STORY_ID "https://article-url.com" "Story Title"
./scripts/hn-deepdive.sh STORY_ID "" "Ask HN: Story Title"   # no article URL
```

**Parallel deep-dives** — run multiple in background and collect results:
```bash
./scripts/hn-deepdive.sh 12345 "https://example.com/a" "Title A" > /tmp/hn-12345.md &
./scripts/hn-deepdive.sh 67890 "https://example.com/b" "Title B" > /tmp/hn-67890.md &
wait
# Then read each /tmp/hn-*.md file
```

Output is structured markdown (TL;DR, key points, discussion themes table, notable comments). Progress goes to stderr.

## Briefing Mode

When user asks casually about hacker news stories, use this style:

### Flow
1. Fetch top 20-50 stories **in main context**, present hot/notable ones in a table
2. User picks stories they want to dig into
3. **Spawn `hn-deepdive.sh` for each pick** (in parallel via `&` + `wait`, writing to temp files). Do NOT fetch articles or comments directly into main context.
4. Read the summary files and present them to the user
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

## Comment Mining

The most valuable HN finds are often **buried in comments**, not in the stories themselves — someone's personal shell function, a workflow hack, an unpublished tool that lives only in their rc file, etc. When a thread is rich (productivity, "how do you X", Ask HN, "what's your setup"), don't just summarize the article — **scan comments for personal systems, tools, and workflows** people mention (casually).

### What to look for
- **Unpublished personal inventions**: shell functions, directory layouts, automation scripts nobody's packaged. If someone mentions it across multiple threads over time, it's battle-tested.
- **"Show and tell" derails**: When a thread devolves into "what's YOUR setup" — that's the gold, not the article.
- **Contrarian practitioners**: The person who says "I tried the opposite and here's what happened" with specifics.
- **First-hand war stories**: "At my company we..." with concrete details, not abstract opinions.

### Following up on interesting commenters
Use the Algolia API to check if they've mentioned the same system before:
```bash
curl -s "https://hn.algolia.com/api/v1/search?query=KEYWORDS&tags=comment,author_USERNAME" | jq '.hits[]'
```
Also check: HN profile (`about` field), GitHub username, dotfiles repos, blog links.

### Linked Artifacts — the real gold

The most valuable HN finds are often **linked in comments, not described** — someone drops a GitHub URL to their dotfiles, a gist with their shell function, an AGENTS.md, a SKILL.md, a personal tool repo. These are the discoveries users care about most.

**Filtering**: Not every link is worth surfacing. Skip generic library links, well-known projects, and obvious self-promotion. Surface links that are:
- **Personal and hand-crafted** — someone's own config, workflow, or tool they built for themselves
- **Validated by the thread** — other commenters praised it, asked questions about it, or built on it
- **Relevant to user interests** — matches topics in the User Interests section below

**During deep dives**: The sub-agent surfaces these in a "Linked Artifacts" section. After reading deep-dive output, **prominently call out** interesting artifacts that pass the filter — don't bury them in comment quotes. Present them as a separate callout so the user can decide whether to chase them down.

**When the user asks to chase an artifact**:
1. Fetch the actual content — `curl` the raw GitHub URL, read the file, present it
2. Navigate the repo structure if needed (API: `https://api.github.com/repos/OWNER/REPO/contents/PATH`)
3. Pull the commenter's history via Algolia for prior mentions
4. Search GitHub/web for related work by the same person

## User Interests

Topics that consistently engage this user:

**Technical deep dives with stakes**: Benchmarks (especially flawed ones), security implications, architectural debates where the answer actually matters (e.g., AGENTS.md vs skills, PS2 FPU quirks)

**Drama + substance**: Naming controversies, governance issues, astroturfing, but only when there's real technical substance underneath the drama (not just gossip)

**Linux/Rust ecosystem**: Desktop environment innovation, Rust rewrites, immutable distros, window manager evolution

**AI agents, LLMs & agentic coding (core interest)**: Everything Claude, Codex, and LLM-based coding agents — new releases, degradation reports, workflow innovations, AGENTS.md/rules/skills patterns, benchmarks, prompt engineering, tool use, context management, multi-agent orchestration. Also: practical architecture, security nightmares, prompt injection risks, skill formation. Interested in how they *actually* work and fail, not hype. Always surface and prioritize these stories.

**Analog notebooks & note-taking**: Physical notebooks, engineering notebooks, bullet journaling, pen & paper workflows, fountain pens, analog productivity systems. Always surface these — a core interest.

**Meta-commentary**: HN discussion quality, when communities get things right vs cargo culting, spotting LLM-generated content

**Anti-patterns to highlight**: Security disasters waiting to happen, unfair benchmarks, overhyped tech with no clothes, projects that rebrand constantly

When summarizing: Structure matters. Categories, tables, direct quotes. Cynical takes alongside genuine analysis. Technical accuracy > hype.
