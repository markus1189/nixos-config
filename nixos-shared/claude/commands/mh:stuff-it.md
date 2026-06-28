---
description: Distill this conversation into one high-signal markdown note in ~/Stuff, then re-index
---

Distill this conversation into one self-contained, high-signal note in
my `~/Stuff` KB. Distill, don't paste — keep decisions, facts,
numbers, commands, code, paths, gotchas, verdicts; drop the chat,
narration, and tool sludge (a dead end stays only as a one-liner if
instructive). Don't invent; don't fake verification.

Focus / slug hint (optional, never block on it):
<focus>$ARGUMENTS</focus>

**Write to** `~/Stuff/Today/<slug>.md` (slug = short, lowercase,
hyphenated, specific). First `fd -e md . ~/Stuff/Today`; extend a
related note instead of duplicating. Don't create dated dirs; don't
edit `llms.txt`/`INDEX.md`.

**House style:** `# Title` → _italic provenance line_ (date, source,
method, caveats) → `## TL;DR` → dense body (`##`, bullets,
language-tagged code, tables for anything comparative) →
bottom-line/verdict → `---` footer (what was left behind). `✓` marks
re-verified claims; quotes keep original language. These are personal
notes — em dashes and emoji markers are fine (de-AI rule is for
outbound prose only).

**Then** run `~/Stuff/.kb/kb-index` and report the path + one-line
title.

If nothing durable is worth keeping, say so instead of manufacturing
filler.
