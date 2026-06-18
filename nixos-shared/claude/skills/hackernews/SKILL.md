---
name: hackernews
description: "Daily HN briefing agent with delta tracking, deep-dive sub-agent pipeline, and user-interest-aware filtering. Use when the user asks about HN, Hacker News, tech news, wants to check/browse HN, get a briefing, see what's new or hot, search stories, read or analyze HN comments, or do a deep dive on a story."
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

Story IDs appear in brackets `[12345678]` in output — use these for `--comments`.

**Script Execution:** Always use absolute paths when invoking scripts. Resolve `./scripts/` against this skill's directory. Example: `/home/markus/.claude/skills/hackernews/scripts/hn-cli.sh`. All scripts use Nix shebangs so no manual dependency installation is required.

## Typical workflows

1. **Browse HN**: Run with no args, scan titles
2. **Search for topics**: Use `-s "query"` to find stories on specific topics
3. **Dive into discussion**: Note story ID, run with `-c ID -d 2`
4. **Research a topic**: Search with `-s`, then fetch comments for interesting stories
5. **Summarize for user**: Fetch stories + comments, summarize key points and insights
6. **Briefing mode**: See below

## Deep-Dive Sub-Agent

**Prompt template**: [`deepdive-prompt.md`](deepdive-prompt.md)

A deep dive runs as a **subagent** — it fetches the article + comments and returns a
structured markdown summary, keeping the raw content out of the main context.

**How to launch one:**
1. Read [`deepdive-prompt.md`](deepdive-prompt.md).
2. Substitute the placeholders: `{{STORY_ID}}`, `{{ARTICLE_URL}}` (empty for Ask HN / no link),
   `{{STORY_TITLE}}`, and `{{HN_CLI}}` → the absolute path to `scripts/hn-cli.sh` (resolve
   `./scripts/` against this skill's directory, e.g.
   `/home/markus/.claude/skills/hackernews/scripts/hn-cli.sh`). Drop the article-fetch step
   when there's no URL.
3. Spawn a subagent with that filled-in text as its prompt. Its result *is* the summary —
   read it directly, no temp files.

**Parallel deep-dives** — spawn **multiple deep-dive subagents in parallel**; collect each
one's summary when they return. (No `&`/`wait`, no `/tmp` files.)

Output is structured markdown (TL;DR, key points, discussion themes table, ⭐ high-profile
commenters, notable comments, linked artifacts, cited papers, meta).

### Ghost thread auto-follow

If a deep dive returns a **ghost thread** (≤5 comments, thin discussion) that **explicitly links to another HN item** as "the real discussion," automatically launch a second deep-dive subagent on that linked thread — do not ask the user first. Treat it as part of the same request. Present the result under the original story's heading with a note like: *"Ghost thread → auto-followed to [47114579]"*.

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
4. **Write file first**: append `## Check N — HH:MM` section and rewrite state tail before presenting output to user
5. Present only the delta (skip unchanged/already-seen stories)
6. User picks deep dives → run them, **append notes to file before presenting summaries**

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
4. **Launch a deep-dive subagent for each pick** (launch them in parallel — see "Deep-Dive Sub-Agent" above). Do NOT fetch articles or comments directly into main context.
5. Collect each subagent's summary from its final message
6. **Write all file updates first** (state file, deep dive notes appended to daily file) — before writing any conversational output. The text summary the user reads is always last.
7. Present summaries to the user
8. Group related stories together
9. When user asks "your take?" — give genuine opinions, not hedged summaries

### Tone
- **HN-native**: direct, slightly cynical, technically literate
- **Not corporate/PR**: have a voice, make judgments
- **Opinionated on request**: distinguish factual summary from editorial take

## Comment Mining

The most valuable HN finds are often **buried in comments**, not in the stories themselves — someone's personal shell function, a workflow hack, an unpublished tool that lives only in their rc file, etc. When a thread is rich (productivity, "how do you X", Ask HN, "what's your setup"), don't just summarize the article — **scan comments for personal systems, tools, and workflows** people mention (casually).

### What to look for
- **Unpublished personal inventions**: shell functions, directory layouts, automation scripts nobody's packaged. Mentioned across multiple threads = battle-tested.
- **"Show and tell" derails**: When a thread devolves into "what's YOUR setup" — that's the gold, not the article.

### Following up on interesting commenters
Use the Algolia API to check if they've mentioned the same system before:
```bash
curl -s "https://hn.algolia.com/api/v1/search?query=KEYWORDS&tags=comment,author_USERNAME" | jq '.hits[]'
```
Also check: HN profile (`about` field), GitHub username, dotfiles repos, blog links.

### Linked Artifacts — the real gold

The most valuable HN finds are often **linked in comments, not described** — someone drops a GitHub URL to their dotfiles, a gist with their shell function, an AGENTS.md, a SKILL.md, a personal tool repo. These are the discoveries users care about most.

**Filtering**: Surface links that are personal/hand-crafted (someone's own config, dotfiles, tool), validated by the thread (other commenters engaged with it), and relevant to User Interests below. Skip generic libraries and well-known projects.

**During deep dives**: The sub-agent surfaces these in a "Linked Artifacts" section. After reading deep-dive output, **prominently call out** interesting artifacts that pass the filter — don't bury them in comment quotes. Present them as a separate callout so the user can decide whether to chase them down.

### Cited Papers & Research

Academic papers, formal institutional documents, and research cited in comments are **always worth surfacing** — treat them like linked artifacts but with higher signal. Commenters who cite specific papers (not just "I read somewhere that...") are usually practitioners with domain expertise. The sub-agent includes a "Cited Papers & Research" section in its output. After reading deep-dive results, call these out prominently alongside linked artifacts.

## Primary Sources for New Releases

For stories about **new model releases** (GPT, Claude, DeepSeek, Gemini, Llama, etc.): after presenting deep-dive summaries, **proactively offer to chase** the system card, technical report, or paper. Find them via the announcement URL, comments, or known locations (e.g., `deploymentsafety.openai.com`, arxiv, HuggingFace model cards). Distill: architecture, full benchmark grid (not cherry-picked), pricing, safety/alignment findings, and practical gotchas.

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

## Wrap-Up Mode

**Trigger**: user says "wrapup" (or variant) to end the session.

1. Read today's `hn-daily.md`
2. Write `hn-wrapup.md` to the same directory **before** any conversational output
3. Confirm briefly — don't re-summarize

### Structure

```markdown
# HN Wrap-Up — [Weekday, Month Day, Year]

> Top N checked. N deep dives.

## The Day in One Paragraph
[dominant themes, tone, 2–3 standout stories — with voice]

## Deep Dives
### [Emoji] [Title] [[story_id]]
**[pts] pts · [comments] comments**
[Distill, don't paste: what matters from the story + thread, key artifacts/papers/commenters]

## Stories Noted (Not Dived)
| Story | Pts | Why Notable |
(only worth-remembering ones — skip low-signal)

## Threads to Follow Tomorrow
(omit if nothing genuine to follow up on)
```

---

## User Interests

Topics that consistently engage this user:

**Technical deep dives with stakes**: Benchmarks (especially flawed ones), security implications, architectural debates where the answer actually matters (e.g., AGENTS.md vs skills, PS2 FPU quirks)

**Drama + substance**: Naming controversies, governance issues, astroturfing, but only when there's real technical substance underneath the drama (not just gossip)

**Linux/Rust ecosystem**: Desktop environment innovation, Rust rewrites, immutable distros, window manager evolution

**AI agents, LLMs & agentic coding (core interest)**: Everything Claude, Codex, and LLM-based coding agents — new releases, degradation reports, workflow innovations, AGENTS.md/rules/skills patterns, benchmarks, prompt engineering, tool use, context management, multi-agent orchestration. Also: practical architecture, security nightmares, prompt injection risks, skill formation. Interested in how they *actually* work and fail, not hype. Always surface and prioritize these stories.

**Analog notebooks & note-taking**: Physical notebooks, engineering notebooks, bullet journaling, pen & paper workflows, fountain pens, analog productivity systems. Always surface these — a core interest.

**Meta-commentary**: HN discussion quality, when communities get things right vs cargo culting, spotting LLM-generated content

**Developer life — craft, anti-hustle & meaning**: Reflective essays on slowing down, deliberate practice, career meaning, deep focus, and the maker's mindset (e.g. "Slowing the Fuck Down", "You Are Not Your Job", "Men Who Stare at Walls"). This is the introspective, practitioner end — surface it. NOT the same as motivational self-help/grift ("10x your life", productivity-hustle, billionaire-mindset content), which stays an anti-pattern below. The test: does it argue for *less* and *deeper*, written by someone who builds things? Then it's in scope.

**Anti-patterns to highlight**: Security disasters waiting to happen, unfair benchmarks, overhyped tech with no clothes, projects that rebrand constantly, motivational/hustle self-help with no technical substance

When summarizing: Structure matters. Categories, tables, direct quotes. Cynical takes alongside genuine analysis. Technical accuracy > hype.
