# Hacker News — daily file and briefings

## Daily File

One file per day, appended across sessions: `$(stuff-today --default)/hn-daily.md`, i.e.
`~/Stuff/YYYY-MM/DD-<host>/hn-daily.md` for the current date (days before 2026-09-27 used
`DD-scratch/`). Not `~/Stuff/Today/`: that follows `cdt NAME`, the daily file must not. It is its own state — no separate tracking block.

### Knowledgebase contract (`kb-index`)

`kb-index` reads `##` headings and nothing deeper. Levels are load-bearing:

| Level | Content | Indexed |
| --- | --- | --- |
| `#` | file title | as series entry |
| `##` | `Check N — HH:MM`; one per dive, ending in `[story_id]` | yes |
| `###` | briefing categories, New/Movers, dive subsections | no |

- Filenames exact — a rename splits the series.
- Dive subsections use `###`, never bold labels: free in the index, and `treemd -s "Notable
  Comments"` works on a 3,000-line file.
- Run `kb-index` after writing (idempotent, ~10s), else today is missing from
  `llms.txt` and the series indexes.

### First check of the day
No `hn-daily.md` for today → full briefing mode (below). Then:
1. Create file: `# HN Daily — Weekday, Month Day, Year`, then `## Check 1 — HH:MM` holding the
   briefing (categories `###`, tables per format below)
2. Dives → append each verbatim (`## Title [story_id]` + metadata line)
3. Run `kb-index`

### Subsequent checks (file exists)
1. Read file, collect already-handled ids (dedup below)
2. Fetch top N
3. Delta: **New** = ids not in file; **Movers** = score +>50 or comments +>30 against the
   `Pts`/`💬` columns of the earlier check tables (that's where the previous numbers live)
4. Write `## Check N — HH:MM` **before** presenting
5. Present delta only
6. Dives → append **before** presenting summaries
7. Run `kb-index`

### Seen-story dedup

No state block. The tables are the state — briefing titles and dive metadata lines both link
the thread:

```bash
grep -oE 'item\?id=[0-9]+' "$(stuff-today --default)/hn-daily.md" | sort -u
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
Check for `$(stuff-today --default)/hn-daily.md` first: exists → delta mode, absent → full
briefing mode.

## Briefing Mode

When user asks casually about hacker news stories, use this style:

### Flow
1. Check for today's daily file (see Daily File above)
2. Fetch top 20-50 stories **in main context**, present them per [Presenting the briefing](#presenting-the-briefing)
3. User picks stories they want to dig into
4. **Launch a deep-dive subagent for each pick** (launch them in parallel — see [deep-dives.md](deep-dives.md)). Do NOT fetch articles or comments directly into main context.
5. Collect each subagent's summary from its final message
6. **Write all file updates first** (briefing tables, dive sections appended to the daily file), then run `kb-index` — before writing any conversational output. The text summary the user reads is always last.
7. Present summaries to the user
8. Group related stories together
9. When user asks "your take?" — give genuine opinions, not hedged summaries

### Presenting the briefing

The chat summary is where stories get lost, not the file: a story the user cares about,
buried mid-bullet among five other links, is one they never see.

- **Every story matching a [User Interest](interests.md) gets its own table row** with
  a Note giving the angle, in the chat as in the file.
- **Interest beats rank.** A stack hit at #50 with 13 pts outranks a 600-pt story outside
  the interests. Never demote a match to a footnote, a "worth knowing" bullet or the
  "rest is in the file" line because its numbers are small.
- **Prose may only group non-matches** (obituaries, archives, general news): at most 2 links
  per bullet. More → a table.
- Before sending, walk the fetched list once: every interest match is a row, or its absence
  is explained.

### Tone
- **HN-native**: direct, slightly cynical, technically literate
- **Not corporate/PR**: have a voice, make judgments
- **Opinionated on request**: distinguish factual summary from editorial take

