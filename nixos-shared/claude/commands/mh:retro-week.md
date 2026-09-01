---
description: Weekly retro over ~/Stuff — audit follow-through on wrap-up findings, escalate recurrences, review open threads
---

# Weekly Retro

A retro the user does, not a report you write. You mine and you draft; **every apply/kill/carry
decision is theirs**. Present, ask, record. Never decide on their behalf and never apply anything
they did not name.

Run at the end of a week. Output: `~/Stuff/YYYY-MM/DD-scratch/retro-week.md` in **today's** day dir.
The filename is exactly `retro-week.md` — `.kb/kb-index` auto-detects a series from a basename
repeating across day dirs, so consistent naming buys a free cross-week index at `.kb/series/`.
Do not creatively rename it.

## The Split — parse the database, read the prose

> **`wrap-up-log.md` is a database. Day dirs are prose.**

`.kb/retro-scan` parses the database: `·`-delimited, closed category set, stable slugs. It is
right every time and you should not second-guess its arithmetic.

Day dirs are prose and **no pattern reads them correctly**. This is settled, not open:

- Headings are freeform. The live state of the Schornstein thread sat under `## Verdict`;
  a `Nächste Schritte` scan misses the newest file in the chain entirely.
- Checkbox state goes stale. June's unchecked `- [ ]` items were resolved in a July file.
  Counting open boxes produces a number that is confidently false.
- Threads span day dirs. Schornstein is `2026-06/30` → `2026-07/14` → `2026-07/17`, one thread,
  three dirs, two months. Nothing in path, filename or heading links them. Only the text does.

So: **read the candidate files.** Do not grep for headings and report the hits as threads.

## Step 1 — Scan the ledger

```
~/Stuff/.kb/retro-scan            # last 7 days; --days N or --since YYYY-MM-DD to change
```

Emits LEDGER (every open finding of any age, plus everything decided in window), RECURRENCE
(all-time slug frequency), COUNTS (follow-through rate). Facts only.

Then read the **previous** `retro-week.md` (`.kb/series/retro-week.md` indexes them; else `fd`).
A retro that cannot remember the last retro is journaling. Carry its open decisions forward and
note anything it promised that did not happen.

If the scan reports `unslugged`, those findings predate the slug column and cannot join to
decisions. Backfill a slug (reuse an existing one if it is the same problem) before proceeding.

## Step 2 — Follow-through audit

Walk every open finding. Each one **must exit the open state**:

- **`APPLIED <date>`** — the fix is on disk.
- **`KILLED <date> (reason)`** — not worth doing. **This is a success, not a failure.** A finding
  is either worth a change or it is noise; deciding it is noise closes the loop. Only `no` forever
  is failure. Push for kills — an honest kill beats a polite carry every time.
- **`CARRY/n`** — deliberately deferred, with a counter.

**At `CARRY/3`, stop offering a third option.** Three weeks of deferral is a decision that has
been made and not admitted. Apply it or kill it.

Group by tier so the cheap ones move fast: 🟢 findings are localized and reversible, offer them
as a batch (`apply green`). 🟡 findings need a real decision each.

## Step 3 — Recurrence and escalation

A slug at **>=3 hits is a rule change, not a note.** The per-session `/mh:retro` structurally
cannot see this — it has no memory across sessions. Catching it is the main reason this retro exists.

Escalation means: the fix is not another log line, it is a delta to `~/.claude/CLAUDE.md`, an
`AGENTS.md`, or a skill. Name the exact file and the exact wording. If a repeat has already
survived three retros, the proposed fix is not working — say so and change the approach rather
than restating it louder.

## Step 4 — Thread board (life scope, not just agent-meta)

Scope is everything in `~/Stuff`: house, admin, Urlaub, hardware, the lot. Not only your own failures.

Candidates are already generated — do not re-derive them:

- `~/Stuff/llms.txt` → **Recently updated (last 7 days)**
- `~/Stuff/YYYY-MM/INDEX.md` → every title and outline for the month

Filter out `hn-daily` / `hn-wrapup` (263 files; they will drown everything). Then **read** the
candidates and chain them into threads **by topic, across day dirs**. For each thread:

- What is its **current** state? Trust the newest file in the chain, not the oldest.
- Is it actually open, or does an old file just look open?
- Is it **stalled**? Nothing for 3+ weeks with an unresolved question is a thread to revive or kill.
- Does it need a real-world action with a date (a call, a mail, a deadline)?

Prefer 5 real threads read properly over 20 heading matches listed mechanically.

## Step 5 — Write it

`~/Stuff/YYYY-MM/DD-scratch/retro-week.md`. The **decisions table is the artifact** — the ledger
lives here, and `wrap-up-log.md` stays an immutable append-only event log that you never edit
(except to backfill a missing slug). Retro owns decisions; wrap-up owns events.

```markdown
# Retro — Week NN (YYYY-MM-DD)

## TL;DR
[2-3 sentences: what closed, what escalated, what is stalled.]

## Ledger
| slug | first seen | hits | decision |
|---|---|---|---|
| gap-assert-before-verify | 2026-07-16 | 3 | APPLIED 2026-07-17 |
| auto-blind-review | 2026-07-17 | 1 | CARRY/1 |
| friction-osm-retail | 2026-07-16 | 1 | KILLED 2026-07-17 (one-off, not a pattern) |

## Escalations
- `slug` (N hits) → [exact file + exact change]

## Threads
- **[Thread name]** — files: `2026-06/30-…` → `2026-07/17-…`
  Status: [current state, from the newest file]
  Next: [concrete action, with a date if it has one] · or **stalled 3w — revive or kill?**

## Numbers
follow-through: N% (applied+killed / window) · open: N · carried: N · escalations: N
[One line vs last week. Rate should climb, open should not grow forever, repeats should die.]

## 🎭 Freestyle
[One honest unstructured paragraph. What actually went wrong this week, what you keep
avoiding, what the numbers do not say. No format.]
```

Then run `~/Stuff/.kb/kb-index` so the new file lands in the month index and the series.

## Applying Fixes

Default is **suggest-only**. Apply only what the user explicitly names, by slug, exactly as
`/mh:retro` does — slugs are the selection handle (`apply green`, `apply gap-foo auto-bar`,
`apply green except friction-baz`).

**Delta discipline**: express every change as a small additive delta to a specific section. Never
regenerate or "clean up" a whole instruction file — rewriting erodes hard-won specifics over
successive runs (context collapse, ACE / arxiv 2510.04618). Preserve existing wording; add a
bullet, do not rephrase the list.

⚠️ Files under `~/.claude/` are **symlinks into the nix store**. Edit the source in the
nixos-config repo, then rebuild. Never edit the symlink.

Record the outcome in the ledger table honestly. `APPLIED` means the edit is on disk — if the
source is edited but a rebuild is still pending to deploy it, say exactly that.
