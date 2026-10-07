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

Both scripts live in `scripts/` next to this SKILL.md; call them by that
path (below written as `scripts/cc-find`, resolve it against this file's
directory, not your cwd). Nix shebangs handle dependencies.

### `cc-find` — search

```
scripts/cc-find [OPTIONS] PATTERN
```

Pipeline: `find` candidate JSONLs → `rg -c` match counts → keep a bounded set
(newest + most matches) → enrich with `ai-title` + first substantive user
prompt + timestamp → rank by tier (PATTERN in title/first prompt > only deeper
in the transcript), then newest first. The calling session
(`$CLAUDE_CODE_SESSION_ID`) is excluded unless `--include-self`. `--help`
documents all flags and tips.

Output is TSV: `TIMESTAMP \t UUID \t CWD \t TITLE \t FIRST_USER_PROMPT`.
Pipe through `column -t -s $'\t'` for aligned columns when showing to the user.

Common flags:

| Flag | Purpose |
|---|---|
| `--cwd PATH` | Substring of the session's working directory (`nixpkgs`, `repos/nixpkgs`, or an absolute path). |
| `--since DATE` | Only sessions modified after DATE (any GNU date string: `2025-10-01`, `'2 weeks ago'`). |
| `--until DATE` | Only sessions modified before DATE. |
| `--limit N` | Default 10. Raise to see tier-0 (content-only) matches. |
| `-s` | Case-sensitive PATTERN. Default is case-insensitive. |
| `--include-self` | Keep the calling session in the results. |

Examples:

```bash
scripts/cc-find easyeffects
scripts/cc-find --cwd nixpkgs 'backport.*claude-code'
scripts/cc-find --since '2 weeks ago' kanata
scripts/cc-find -s 'export VISUAL|VISUAL='
```

### `cc-show` — display

```
scripts/cc-show UUID_OR_PREFIX [--full|--tools|--raw] [--grep PATTERN [-C N]] [--tail N]
```

Prints a session's content. UUID may be a unique prefix (≥4 chars).

| Mode | Shows |
|---|---|
| (default) | User + assistant text with timestamps. |
| `--full` | Adds thinking blocks and tool_use/tool_result content. |
| `--tools` | Compact action log: tool_use calls only. |
| `--raw` | Dump raw JSONL. |
| `--grep PATTERN` | Only messages matching PATTERN (case-insensitive awk ERE) plus `-C N` messages of context (default 2). |
| `--tail N` | Only the last N messages (lines with `--tools`). Applied before `--grep`. |

Sessions can be huge: read part of one with `--grep` / `--tail` rather than
dumping it whole.

## Search heuristics

1. **Skill names make poor queries.** They occur in every transcript via the
   injected skill listing, so a skill-name PATTERN matches everything.

2. **"claude" / "claude-code" are in every transcript too**; search the
   discriminating word (`OTel`, `backport`) instead.

3. **The user's query vocabulary may differ from the auto-generated title.**
   "backport" vs. title "Update claude-code package"; "migration" vs. title
   "Fix double escape conflict between kanata and Claude Code". The tier
   system handles this: tier 1 (title/prompt hits) is shown first, tier 0
   (content-only hits) follows. If the obvious hit isn't in the top results,
   try a different word or `--limit 30`.

4. **Subagent transcripts are excluded** (they live under
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

Measured 2026-10-07: 6,051 sessions, 9.3 GB (`du -sh ~/.claude/projects`).
Typical queries take 2–3 s with no index, including broad ones (`nix`);
the stage-3 enrichment is bounded to ~8×`--limit` files.
