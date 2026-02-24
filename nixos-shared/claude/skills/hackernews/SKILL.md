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

## Daily State Tracking

Track HN browsing across multiple sessions in a single day using a stateful markdown file.

### State file location
`~/Stuff/YYYY-MM/DD-scratch/hn-daily.md` (uses current date)

### First check of the day
If no `hn-daily.md` exists for today → full briefing mode (see below). After presenting stories:
1. Create the file with a `## Check 1 — HH:MM` section containing the briefing
2. Append a state tail (see format below)
3. Deep dives requested → append inline under the check section, update status to `dived`

### Subsequent checks (file exists)
1. Read `hn-daily.md`, parse the state tail at the bottom
2. Fetch HN top N
3. Compute delta:
   - **New**: stories not in state tail
   - **Movers**: stories where score jumped >50 OR comments jumped >30 since last check
4. Present only the delta (skip unchanged/already-seen stories)
5. User picks deep dives → append inline
6. Append new `## Check N — HH:MM` section
7. Rewrite state tail with merged current snapshot

### State tail format
Always at the very end of the file, after a `---` separator. Fenced with ` ```state ` / ` ``` `. Pipe-delimited, one line per story:

    ---
    ```state
    47122715|Age Verification Trap|1449|1110|dived
    47120899|Ladybird Rust|1172|647|dived
    47133055|enveil|76|38|dismissed
    47131225|Steerling-8B|159|40|noted
    ```

Fields: `story_id|title|score|comments|status`

Status values:
- `new` — appeared, not yet interacted with
- `noted` — user acknowledged / showed interest
- `dived` — deep dive completed
- `dismissed` — user explicitly skipped

The state tail is **rewritten** (not appended) each check — always reflects the current snapshot with updated scores/comments.

### Delta presentation format

```markdown
## Check 2 — 18:00

### New Stories
| # | Story | Pts | 💬 | Topic |
...

### Movers
| Story | Was | Now | Δ pts | Δ 💬 |
...
```

### When user says "check HN" / "what's new on HN"
Always check for an existing `~/Stuff/YYYY-MM/DD-scratch/hn-daily.md` first. If it exists, run in delta mode. If not, run full briefing mode.

## Briefing Mode

When user asks casually about hacker news stories, use this style:

### Flow
1. Check for existing daily state file (see Daily State Tracking above)
2. Fetch top 20-50 stories **in main context**, present hot/notable ones in a table
3. User picks stories they want to dig into
4. **Spawn `hn-deepdive.sh` for each pick** (in parallel via `&` + `wait`, writing to temp files). Do NOT fetch articles or comments directly into main context.
5. Read the summary files and present them to the user
6. Group related stories together
7. When user asks "your take?" — give genuine opinions, not hedged summaries
8. Update the daily state file (create if first check, append if subsequent)

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

### Cited Papers & Research

Academic papers, formal institutional documents, and research cited in comments are **always worth surfacing** — treat them like linked artifacts but with higher signal. Commenters who cite specific papers (not just "I read somewhere that...") are usually practitioners with domain expertise. The sub-agent includes a "Cited Papers & Research" section in its output. After reading deep-dive results, call these out prominently alongside linked artifacts.

**When the user asks to chase an artifact**:
1. Fetch the actual content — `curl` the raw GitHub URL, read the file, present it
2. Navigate the repo structure if needed (API: `https://api.github.com/repos/OWNER/REPO/contents/PATH`)
3. Pull the commenter's history via Algolia for prior mentions
4. Search GitHub/web for related work by the same person

## High-Profile Commenters

When a recognizable person shows up in a thread, their comment carries more weight than an anon's — they're speaking from a track record. Flag them explicitly, don't just quote them as "a commenter."

Use your general knowledge to recognize notable HN usernames — you know who they are. Also catch people who identify themselves in-thread ("I'm the author of X", "I work at Y") or whose linked profile/blog makes them notable.

### How to surface them

- In deep dives: add a **⭐ High-Profile Commenters** section before Notable Comments listing who appeared, their identity, and their key point
- In main context briefings: call it out inline if a notable person is the submitter or a prominent commenter (e.g., "Armin Ronacher is in the thread and disagrees with the framing")
- Weight their comments higher — quote them first, even if the comment isn't the most upvoted
- Note when the article's *author* comments (they often add context not in the article)

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
