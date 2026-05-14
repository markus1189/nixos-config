---
name: finding-past-cc-sessions
description: Searches local Claude Code session transcripts under ~/.claude/projects to find past conversations by topic, date, or project. Use when the user asks to find, recall, look up, or re-read a previous Claude Code session, transcript, or chat history.
context: fork
---

# Finding Claude Code Sessions

The user has thousands of past Claude Code session transcripts on disk
(`~/.claude/projects/<cwd-encoded>/<session-uuid>.jsonl`). This skill provides
two CLI tools for searching them and reading them back.

## Reporting findings concisely

This skill runs in a forked context — only the final reply reaches the parent
conversation. Keep the reply tight so the parent's context stays clean:

- Return the top 1–3 matches: UUID (or 8-char prefix), title, cwd, date.
- Add a one-line gist per match — *why* it matches the query.
- Include `cc-show` excerpts only when the parent explicitly asked to *read*
  a session. Otherwise hand back UUIDs and let the parent request
  `cc-show UUID` itself.
- Do not paste raw TSV, full transcripts, or the entire hit list. Filter,
  summarize, hand back UUIDs.

## When to invoke

Any user request to recall or read a previous Claude Code session:

- "find our conversation about kanata"
- "where did we discuss the OTel setup"
- "what did we do last week about backports"
- "show me that session where I asked about emacsclient"
- "read the session where we set up X"

If unsure, prefer to use the skill — the cost of a missed hit is the user
re-explaining context; the cost of a spurious search is one second.

## Tools

Both scripts in `./scripts/`. Nix shebangs handle dependencies.

### `cc-find` — search

```
./scripts/cc-find [OPTIONS] PATTERN
```

Pipeline: `find` candidate JSONLs → `rg` filter by PATTERN → enrich each match
with `ai-title` + first substantive user prompt + timestamp → rank by relevance
tier (PATTERN matches title/prompt > matches only in tool output) then recency.

Output is TSV: `TIMESTAMP \t UUID \t CWD \t TITLE \t FIRST_USER_PROMPT`.
Pipe through `column -t -s $'\t'` for aligned columns when showing to the user.

Common flags:

| Flag | Purpose |
|---|---|
| `--cwd PATTERN` | Substring-match the project directory (e.g. `--cwd nixpkgs`). |
| `--since DATE` | Only sessions modified after DATE (any GNU date string: `2025-10-01`, `'2 weeks ago'`). |
| `--until DATE` | Only sessions modified before DATE. |
| `--limit N` | Default 10. Raise to see tier-B (content-only) matches. |
| `-s` | Case-sensitive PATTERN. Default is case-insensitive. |

Examples:

```bash
./scripts/cc-find easyeffects
./scripts/cc-find --cwd nixpkgs 'backport.*claude-code'
./scripts/cc-find --since '2 weeks ago' kanata
./scripts/cc-find -s 'export VISUAL|VISUAL='
```

### `cc-show` — display

```
./scripts/cc-show UUID_OR_PREFIX [--full|--tools|--raw]
```

Prints a session's content. UUID may be a unique prefix (≥4 chars).

| Mode | Shows |
|---|---|
| (default) | User + assistant text with timestamps. |
| `--full` | Adds thinking blocks and tool_use/tool_result content. |
| `--tools` | Compact action log: tool_use calls only. |
| `--raw` | Dump raw JSONL. |

Pipe through `less -R` for long sessions.

## Search heuristics

These are non-obvious; apply them when picking a PATTERN:

1. **Distinctive nouns beat common terms.** Search `kanata`, `easyeffects`,
   `OTel` — not `keyboard`, `audio`, `telemetry`. The corpus is large; rare
   words are precise.

2. **Anchor common English words used as identifiers.** `VISUAL` matches
   "visual annotations" everywhere. Use `-s 'export VISUAL|VISUAL='` instead.
   Same trick for `EDITOR`, `PATH`, `SHELL`, `DEBUG`.

3. **For "claude" / "claude-code" itself**, search the *discriminating* word
   (`OTel`, `backport`, `migration`, `skill`) rather than the ubiquitous one.
   Every transcript mentions claude-code.

4. **Pre-filter when you have a hint.** `--cwd nixos-config`, `--cwd nixpkgs`,
   `--since '1 month ago'`. These often cut the search 10×.

5. **The user's query vocabulary may differ from the auto-generated title.**
   "backport" vs. title "Update claude-code package"; "migration" vs. title
   "Fix double escape conflict between kanata and Claude Code". The tier
   system handles this: tier-A (title/prompt hits) is shown first, tier-B
   (content-only hits) follows. If the obvious hit isn't in the top results,
   try a different word or `--limit 30`.

6. **Subagent transcripts are excluded** (they live under
   `<session-uuid>/subagents/` and aren't standalone sessions).

## When the scripts aren't enough

Drop down to raw `rg` + `jq` for queries the scripts don't directly support:

```bash
# Sessions that invoked a specific Skill / MCP / tool
rg -l '"name":"Skill"' ~/.claude/projects/*/*.jsonl

# Sessions on a specific git branch
rg -l '"gitBranch":"feature/foo"' ~/.claude/projects/*/*.jsonl

# Sessions that hit a specific API error
rg -l '"status":522' ~/.claude/projects/*/*.jsonl
```

Each JSONL line has `type`, `sessionId`, `cwd`, `gitBranch`, `timestamp`.
Useful `type` values: `user`, `assistant` (with `.message.content[]` items
of `text` / `thinking` / `tool_use` / `tool_result`), `ai-title`,
`attachment`, `last-prompt`, `file-history-snapshot`.

## Performance

~5–6k sessions, ~2 GB. Most queries finish in 1–2 seconds with no index.
Stage 1 (`find`) is filesystem-fast; stage 2 (`rg`) parallelises across files;
stage 3 (`jq` enrichment) runs only on the match set, typically <100 files.

---

**Script Execution:** Scripts should be executed from the skill directory.
All scripts use Nix shebangs so no manual dependency installation is required.
