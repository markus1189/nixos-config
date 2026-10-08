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
hyphenated, specific). First look for related notes: `fd -e md .
~/Stuff/Today`, then `rg -l -i '<term>' ~/Stuff --glob '*.md'` for
2–3 distinctive key terms of the topic (KB-wide, not just today).
Extend a related note instead of duplicating. If a new note supersedes or
complements an existing one, cross-link both ways with relative
markdown links (a bare name in backticks is not a link). Don't
create dated dirs; don't edit `llms.txt`/`INDEX.md`.

**House style:** `# Title` → _italic provenance line_ (date, source,
method, caveats) → `## TL;DR` → dense body (`##`, bullets,
language-tagged code, tables for anything comparative) →
bottom-line/verdict → `---` footer (what was left behind). `✓` marks
re-verified claims; quotes keep original language. These are personal
notes — em dashes and emoji markers are fine (de-AI rule is for
outbound prose only).

**Then** run `kb-index` and report the path + one-line
title.

If nothing durable is worth keeping, say so instead of manufacturing
filler. Interview me before assuming anything about the content
that the conversation hasn't already settled (scope, emphasis, what
to keep, slug): as many rounds as it takes, each question with a
recommended default. Don't ask about what the chat already decided.

Only I decide. Your recommendations, and my follow-up questions on
one, are not decisions: record them as options or recommendations
unless I explicitly chose. Unsure whether I did? Ask.
