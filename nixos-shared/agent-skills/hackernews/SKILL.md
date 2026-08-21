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

### Dump a full thread (deep dives, comment mining)
```bash
./scripts/hn-cli.sh -t STORY_ID                  # Whole comment tree, ONE request (Algolia), up to 300 comments
./scripts/hn-cli.sh --thread STORY_ID -n 800     # Bigger threads
```

`-c` fetches comments one-by-one from Firebase (slow; pretty tree for casual browsing). `-t` gets the entire tree in a single request, plain text, with comment links preserved — prefer it whenever you actually need to read a discussion.

Story IDs appear in brackets `[12345678]` in output — use these for `--comments`/`--thread`.

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
2. Substitute `{{STORY_ID}}`, `{{ARTICLE_URL}}` (empty for Ask HN / no link),
   `{{STORY_TITLE}}`, `{{CHECK_N}}` (which check this dive belongs to), and `{{HN_CLI}}` →
   absolute path to `scripts/hn-cli.sh`, resolving `./scripts/` against this skill's directory
   (e.g. `/home/markus/.claude/skills/hackernews/scripts/hn-cli.sh`). Drop the article-fetch
   step when there's no URL.
3. Spawn a subagent with that filled-in text as its prompt. Its result *is* the summary —
   read it directly, no temp files.

**Parallel deep-dives** — spawn **multiple deep-dive subagents in parallel**; collect each
one's summary when they return. (No `&`/`wait`, no `/tmp` files.)

Output is a `##` heading + metadata line + `###` subsections (TL;DR, Key Points, HN Discussion
Themes, ⭐ High-Profile Commenters, Notable Comments, Linked Artifacts, Cited Papers, Meta) —
append it verbatim, see "Dive metadata contract" below.

### Dive metadata contract

`##` heading, two metadata lines, `###` subsections. Same in **both** `hn-daily.md` and
`hn-wrapup.md`.

```markdown
## Show HN: Huzzah – a novel approach to coding with AI [49378768]

**350 pts · 198 💬 · danielvaughn** · [thread](https://news.ycombinator.com/item?id=49378768) · [danielvaughn.dev](https://www.danielvaughn.dev/posts/huzzah/) · dive @ check 1
`tags: ai-agents, tooling | +pseudocode, source-maps`

### TL;DR
…
```

Every field is in the `hn-cli.sh --thread` header (`350 points · danielvaughn · 2026-08-20 ·
198 comments in tree`, then the article URL). No extra fetch.

- Article link text = bare domain. Omit that link entirely for Ask HN / text-only Show HN.
- `dive @ check N` in the daily file only; drop it in the wrap-up.
- Tags line backticked and alone, so `rg 'tags:.*prompt-injection'` hits headers, not prose.

### Tag vocabulary

`tags: <canonical, ...> | +<free, ...>` — free side max 2, lowercase, hyphenated, specific
(`lean4`, `sondehub`, `cricut`), not a looser restatement of a canonical tag. Canonical side
draws only from:

- **AI/LLM** — `ai-agents` `llm-eval` `benchmarks-flawed` `prompt-injection` `context-mgmt` `model-release`
- **Security** — `security` `supply-chain` `privacy` `surveillance`
- **Stacks** — `rust` `linux` `systems` `databases` `languages` `web` `hardware`
- **Introspective** — `analog` `craft` `career`
- **Rest** — `meta` `drama` `papers` `tooling` `math` `science` `policy`

Promote a free tag into the canonical list once it recurs in 3 dives.

### Ghost thread auto-follow

If a deep dive returns a **ghost thread** (≤5 comments, thin discussion) that **explicitly links to another HN item** as "the real discussion," automatically launch a second deep-dive subagent on that linked thread — do not ask the user first. Treat it as part of the same request. Present the result under the original story's heading with a note like: *"Ghost thread → auto-followed to [47114579]"*.

## Daily File

One file per day, appended across sessions: `~/Stuff/YYYY-MM/DD-scratch/hn-daily.md`
(current date). It is its own state — no separate tracking block.

### Knowledgebase contract (`~/Stuff/.kb/kb-index`)

`kb-index` reads `##` headings and nothing deeper. Levels are load-bearing:

| Level | Content | Indexed |
| --- | --- | --- |
| `#` | file title | as series entry |
| `##` | `Check N — HH:MM`; one per dive, ending in `[story_id]` | yes |
| `###` | briefing categories, New/Movers, dive subsections | no |

- Filenames exact — a rename splits the series.
- Dive subsections use `###`, never bold labels: free in the index, and `treemd -s "Notable
  Comments"` works on a 3,000-line file.
- Run `~/Stuff/.kb/kb-index` after writing (idempotent, ~1s), else today is missing from
  `llms.txt` and the series indexes.

### First check of the day
No `hn-daily.md` for today → full briefing mode (below). Then:
1. Create file: `# HN Daily — Weekday, Month Day, Year`, then `## Check 1 — HH:MM` holding the
   briefing (categories `###`, tables per format below)
2. Dives → append each verbatim (`## Title [story_id]` + metadata line)
3. Run `~/Stuff/.kb/kb-index`

### Subsequent checks (file exists)
1. Read file, collect already-handled ids (dedup below)
2. Fetch top N
3. Delta: **New** = ids not in file; **Movers** = score +>50 or comments +>30 against the
   `Pts`/`💬` columns of the earlier check tables (that's where the previous numbers live)
4. Write `## Check N — HH:MM` **before** presenting
5. Present delta only
6. Dives → append **before** presenting summaries
7. Run `~/Stuff/.kb/kb-index`

### Seen-story dedup

No state block. The tables are the state — briefing titles and dive metadata lines both link
the thread:

```bash
grep -oE 'item\?id=[0-9]+' ~/Stuff/YYYY-MM/DD-scratch/hn-daily.md | sort -u
```

Match `item?id=`, never bare 7–9 digit numbers: dive bodies cite comment ids as `[49379070]`,
which a numeric grep counts as seen stories and silently hides real ones.

Disposition goes in the Note column (`skipped — job ad`). Dived stories have their own `##`.

### Briefing table format

One table per category, categories at `###`:

```markdown
| Story | Pts | 💬 | ID | Note |
| --- | --- | --- | --- | --- |
| [Show HN: Huzzah – a novel approach to coding with AI](https://news.ycombinator.com/item?id=49378768) | 260 | 145 | 49378768 | pseudocode as source of record; author answers every critique |
| [Malicious Rust crate Arrayref runs a build-time payload](https://news.ycombinator.com/item?id=49374269) | 446 | 388 | 49374269 | 🎯 dive candidate — live supply-chain incident |
| [Sixtyfour (YC P25) Is Hiring](https://news.ycombinator.com/item?id=49377248) | 1 | 0 | 49377248 | skipped — job ad |
```

- Title links to the HN thread; `hn-cli.sh` prints that URL for every story.
- No rank column (meaningless after the fetch). ID column stays — it feeds `--thread`.
- Note = one clause of reasoning: the angle, `🎯` if dive-worthy, or why skipped. Empty is
  fine; restating the title is noise.

### Delta presentation format

```markdown
## Check 2 — 18:00

### New
| Story | Pts | 💬 | ID | Note |
| --- | --- | --- | --- | --- |
| [Codex on AWS Bedrock bug causing 10x charges](https://news.ycombinator.com/item?id=49383326) | 156 | 61 | 49383326 | billing bug, not a model story |

### Movers
| Story | Was | Now | Δ |
| --- | --- | --- | --- |
| [Aaron Swartz prosecuted while Meta scrapes freely](https://news.ycombinator.com/item?id=49379550) | 1295/288 | 1702/390 | +407/+102 |
```

`Was`/`Now` are `pts/comments` pairs.

### When user says "check HN" / "what's new on HN"
Check for `~/Stuff/YYYY-MM/DD-scratch/hn-daily.md` first: exists → delta mode, absent → full
briefing mode.

## Briefing Mode

When user asks casually about hacker news stories, use this style:

### Flow
1. Check for today's daily file (see Daily File above)
2. Fetch top 20-50 stories **in main context**, present hot/notable ones in a table
3. User picks stories they want to dig into
4. **Launch a deep-dive subagent for each pick** (launch them in parallel — see "Deep-Dive Sub-Agent" above). Do NOT fetch articles or comments directly into main context.
5. Collect each subagent's summary from its final message
6. **Write all file updates first** (briefing tables, dive sections appended to the daily file), then run `~/Stuff/.kb/kb-index` — before writing any conversational output. The text summary the user reads is always last.
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

**During deep dives**: The sub-agent surfaces these in a "Linked Artifacts" section (🎯 = most chase-worthy). After every deep-dive batch, aggregate them into a dedicated **🔗 Artifacts & Papers** block in the response — its own section, never buried inside comment quotes. The block always appears; if no dive surfaced anything, say so in one line.

**Chase proactively**: For 1–3 artifacts per batch that match User Interests, don't ask — fetch the README/gist/file immediately (read-only) and attach a 2–3 sentence verdict: what it actually is, whether it's worth adopting. Ask first only when a chase would become a rabbit hole (cloning repos, long papers, multi-repo spelunking).

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
3. Run `~/Stuff/.kb/kb-index`
4. Confirm briefly — don't re-summarize

### Structure

```markdown
# HN Wrap-Up — [Weekday, Month Day, Year]

> Top N checked. N deep dives.

## The Day in One Paragraph
[dominant themes, tone, 2–3 standout stories — with voice]

## [Emoji] [Title] [[story_id]]

**[pts] pts · [comments] 💬 · [submitter]** · [thread](https://news.ycombinator.com/item?id=STORY_ID) · [domain.tld](ARTICLE_URL)
`tags: canonical, tags | +free, form`

[Distill, don't paste: what matters from story + thread, key artifacts/papers/commenters]

(one `##` per dive, no wrapper heading; `[story_id]` required — `.kb/series/hn-wrapup.md`
indexes these. Metadata + tags = the same contract as the daily file: copy across, don't
re-derive. Subsections `###` if the entry is long enough to need them.)

## Stories Noted (Not Dived)
| Story | Pts | 💬 | Why Notable |
(linked titles, as in the briefing tables; skip low-signal)

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
