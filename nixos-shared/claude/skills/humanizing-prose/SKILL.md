---
name: humanizing-prose
description: "Guides writing prose that avoids common AI/LLM tells, and reviews existing text for AI tells with concrete reformulation suggestions. Use when the user asks to 'humanize' text, 'de-AI' writing, review text for AI tells, write naturally, avoid sounding like AI, make text sound human, or mentions AI detection concerns."
---

# Humanizing Prose

Avoid and fix the patterns that make text recognizable as AI-generated.

## Two Modes

### Writing Mode
When composing prose, internalize the tells catalog and avoid them from the start. Do not merely swap AI words for synonyms — write with varied rhythm, specific details, authentic voice, and no significance inflation.

### Review Mode
When reviewing existing text, scan for tells systematically, then output a structured report:

```
## AI Tells Review

**Overall Assessment**: [Clean / Minor tells / Significant tells / Heavily AI-flavored]

**Vocabulary tells found**: [list with line references]
**Phrase tells found**: [list with line references]
**Structural tells**: [description]
**Tone issues**: [description]

### Suggested Rewrites
[For each flagged passage: original → suggested fix, with brief explanation]
```

## Core Rules (When Writing)

1. **Vary sentence length** — mix short punchy sentences with longer ones. Fragments are fine. Uniformity is the biggest structural tell.
2. **No hedging preambles** — delete "It's important to note," "It's worth mentioning," etc. Just state the point.
3. **No significance inflation** — don't call things pivotal, crucial, groundbreaking unless the user's content genuinely warrants it. Prefer concrete consequences over abstract importance claims.
4. **No participle analysis** — never append "...highlighting its significance" or "...contributing to the broader ecosystem" to a factual statement.
5. **No negative parallelism** — avoid "It's not X, it's Y" and "Not just X, but Y." These are the single most statistically common ChatGPT tell.
6. **No balanced-clause diplomacy** — don't default to "While X, Y is also important." Take a position or state facts plainly.
7. **Specific > abstract** — replace claims of impact with what actually happened. Numbers, names, consequences.
8. **Skip unnecessary transitions** — "Additionally," "Furthermore," "Moreover," are AI tells. Just start the next sentence. Or use "And" / "But."
9. **Have a voice** — allow opinions, asides, directness, even mild informality where appropriate. Monotone politeness is a tell.
10. **Don't over-structure** — not everything needs headers and bullet points. Flowing paragraphs are often more natural.
11. **No em dashes** — always replace em dashes (—) with alternative punctuation: commas, periods, colons, semicolons, or parentheses. Em dashes are a strong AI tell.

## Reference Files

- **Full tells catalog** with categorized examples: [references/tells-catalog.md](references/tells-catalog.md)
- **Reformulation guide** with before/after examples: [references/reformulation-guide.md](references/reformulation-guide.md)

Consult these references when doing a detailed review or when uncertain about a specific pattern.

## Vocabulary Watchlist (Quick Reference)

Avoid or use sparingly: *delve, crucial, pivotal, foster, bolster, underscore, highlight, emphasize, showcase, garner, intricate, meticulous, vibrant, enduring, profound, landscape* (abstract), *tapestry* (abstract), *testament, interplay, cornerstone, leverage, navigate, ecosystem, framework, groundbreaking, renowned, nestled, diverse array, boasts, seamlessly, notably, comprehensive*

These aren't banned — context matters. "Pivotal" in a basketball article is fine. A paragraph with *pivotal*, *crucial*, *fostering*, and *underscores* is not.
