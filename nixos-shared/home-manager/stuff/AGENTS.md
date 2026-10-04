# Welcome to ~/Stuff

Date-organized scratch and working directory tree.

This file is generated: it is a read-only symlink from nixos-config
(`nixos-shared/home-manager/stuff/AGENTS.md`), identical on every host.
Change it there and rebuild; an edit here cannot stick.

## Structure

```
~/Stuff/
  YYYY-MM/          ← month dirs, created automatically
    DD-name/         ← daily working dirs (e.g. 17-some-project-or-topic/)
    DD-<host>/       ← default dir per machine (27-p1g8/, 27-p1/, 27-nuc/), created automatically
    DD-scratch/      ← the same, before 2026-09-27 (one machine, no host in the name)
  Today             ← symlink to today's working dir
```

## Guidelines for Agents

- **Don't create or rename `YYYY-MM/`, `DD-<host>/` or `DD-scratch/` dirs** — these are managed automatically
- **Don't write `Today` yourself.** `stuff-today` is its only writer (run by `cdt`, Emacs
  and a daily timer); `stuff-today NAME` creates `DD-NAME` and repoints `Today` at it
- **Fixed per-day files go in `$(stuff-today --default)`**, today's `DD-<host>/`, not `Today`:
  `Today` follows `cdt NAME`. The host in the name keeps machines from writing the same file
  (`nixos-p1` is tagged `p1`)
- **Only notes and code reach the other machines.** Syncthing syncs `~/Stuff` between p1g8,
  p1 and nuc (and to the phone S26U, receive only), opt-in by extension: `md org py sh nix hs ts el toml yaml yml`. Everything else
  (html, json, pdf, images, dumps) stays on the host that wrote it, and so do junk dirs
  (`node_modules`, `*venv*`, `.git`, `jadx*`, …) even for their `.md`, as do dirs named like a
  synced file (`repo.gradle.org/`, `x.md/`). Other types sync only below a `_sync-all-files/`
  dir (a report with its assets): keep it small, no scrapes, captures or downloads. The lists
  live in nixos-config (`nixos-shared/stuff-patterns.nix`); `.stignore` is generated, don't edit it
- **No two names that differ only in case** (`SOLVE.py` next to `solve.py`): the phone
  has case-insensitive storage, holds only one of them and reports the other as failed
- **Never put secrets in a synced type**: tokens, cookies and credentials go in `.env`,
  `*.cookies`, `*.json` or similar, which stay local (never below `_sync-all-files/`)
- **Deleting a dir deletes only its synced files elsewhere.** A host that still holds local
  non-synced files in it keeps the dir with those files in it
- **Start at [`llms.txt`](llms.txt)** — the generated navigation preamble (months, recent activity, per-month indexes)

## Markdown tooling (treemd + fd)

`treemd` (markdown navigator/extractor), `fd` (file finder) and `rg` are on PATH:

- `treemd -l FILE` — list all headings (with `#` level markers)
- `treemd --tree FILE` — heading tree
- `treemd -s "Section Name" FILE` — extract one section
- `cat FILE | treemd -l -` — read from stdin
- `fd -t f "^pattern$" ~/Stuff` — find files (skips hidden dirs like `.kb/`)

Prefer these over `grep`/`sed` for navigating or slicing large structured markdown (the big reference docs, transcripts, and the dated `hn-daily.md` files). Example — cross-day index of every HN story heading:

```
fd -t f "^hn-daily\.md$" ~/Stuff | sort | while read f; do treemd -l "$f" | grep -E "\[[0-9]{7,9}\]"; done
```

## Knowledgebase index & search

The archive indexes itself — no database, no frontmatter required.

- **`llms.txt`** (root) — navigation preamble: month list, files touched in the last 7 days,
  links to each month's `INDEX.md`, and pointers to the tools below.
- **`YYYY-MM/INDEX.md`** — per-month index: every doc grouped by day dir, with a title and
  heading outline derived from `treemd`.
- **`.kb/series/<name>.md`** — cross-day index for any basename that recurs in ≥5 day dirs.

All three are **generated** by `kb-index` — treat them as read-only (edits get overwritten).

```
kb-index          # rebuild llms.txt + all INDEX.md + series (≈10s; idempotent, only rewrites changed files)
```

The generator is deterministic: it derives titles/outlines from headings only, and stamps
freshness from the newest source file's mtime — so re-running with no content change is a
no-op. Run it after adding or substantially editing notes.

## Weekly retro (`kb-retro-scan` + `/mh:retro-week`)

`/mh:retro` appends per-session findings to `YYYY-MM/DD-<host>/wrap-up-log.md`. A single
session can't see across sessions, so recurring mistakes and unapplied findings pile up unnoticed.
`/mh:retro-week` is the weekly pass that reads them; `kb-retro-scan` is its miner.

```
kb-retro-scan                    # last 7 days
kb-retro-scan --days 14
kb-retro-scan --since 2026-07-01 # explicit window (reproducible)
```

Emits LEDGER (every open finding of any age + everything decided in window), RECURRENCE
(all-time slug frequency; **>=3 hits = escalate to a rule change**), COUNTS (follow-through rate).

Division of labour — the two files it reads are the only structured ones:

- `wrap-up-log.md` — **append-only event log**, owned by `/mh:retro`. Never edit it except to
  backfill a missing `slug` (the 2026-07-16 log predates the column and was backfilled).
- `retro-week.md` — **decisions projection**, owned by `/mh:retro-week`, in the day dir it was
  run in. Holds the `| slug | first seen | hits | decision |` table. A finding exits via
  `APPLIED` / `KILLED (reason)` / `CARRY/n`; at `CARRY/3` it's apply-or-kill. Keep the filename
  exact — 5 day dirs makes it a series, and `.kb/series/retro-week.md` indexes it for free.

⚠️ **`kb-retro-scan` reads those two files and nothing else. Do not extend it into day dirs, and
do not grep day dirs for open threads yourself** — it looks easy and it silently fails three ways.
Proven on the Schornstein chain (`2026-06/30` → `2026-07/14` → `2026-07/17`, one thread, three dirs):

- **Freeform headings** — the newest file holds its live state under `## Verdict`; a
  `Nächste Schritte` pattern misses it completely.
- **Stale checkboxes** — June's unchecked `- [ ]` items were resolved in the July file. Counting
  open boxes yields a confidently false number.
- **No chaining** — nothing in path, filename or heading links the three files. Only the prose does.

Threads need a reader, not a parser: **`wrap-up-log.md` is a database, day dirs are prose.**
Get thread candidates from `llms.txt` (recent 7 days) and `INDEX.md` (titles), then *read* them.

**Full-text search** is just `rg`:

```
rg "search term" ~/Stuff                 # full-text across the archive
rg -l "term" --glob '*.md' ~/Stuff       # just the filenames that match
fd -e md . ~/Stuff/2026-06 | xargs rg "term"   # scope by month/day first
```

## Backup

`backup-stuff` mirrors `~/Stuff` to Google Drive (`Ablage/Backup/Stuff`) with `rclone sync`.
It runs on one designated host only and refuses elsewhere: a mirror from a host missing some
months would delete them from the backup. `backup-stuff --dry-run` first.
