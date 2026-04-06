---
name: humanizing-prose
description: "Write or rewrite prose that avoids AI/LLM tells. Review text for AI artifacts with concrete fix suggestions. Use when the user asks to 'humanize' text, 'de-AI' writing, review for AI tells, write naturally, sound less robotic, sound less ChatGPT, sound more human, edit for voice, make text authentic, or mentions AI detection concerns."
---

# Humanizing Prose

Fix the patterns that make text read as AI-generated. Not by swapping words for synonyms, but by writing the way a specific, opinionated human would.

## Choosing a Mode

**Review Mode**: The user provides text and asks you to check, audit, or review it for AI tells. Output a structured report (template below), then stop.

**Rewrite Mode**: The user provides text and asks you to rewrite, humanize, or fix it. Scan for tells silently, then return the rewritten text. Mention the most important changes briefly afterward.

**Writing Mode**: The user asks you to compose new prose. Internalize these rules and write clean from the start.

If ambiguous, ask which mode the user wants.

## Review Mode Template

```
## AI Tells Review

**Overall Assessment**: [Clean / Minor tells (1-3) / Significant tells (4-8) / Heavily AI-flavored (9+)]

**Tier 1 tells** (strongly diagnostic): [list with quotes]
**Tier 2 tells** (moderate signals): [list with quotes]
**Tier 3 tells** (weak signals, flag only if clustered): [list with quotes]
**Structural/tone issues**: [description]

### Suggested Rewrites
[For each flagged passage: original > suggested fix, with brief explanation]
```

## Core Rules

Target voice: a knowledgeable person writing a blog post or a thoughtful email. Not a consultant writing a report. Not a Wikipedia editor. You are allowed to take sides, express opinions, and write five paragraphs without a single header.

1. **Mix sentence lengths deliberately.** Follow a 30-word sentence with a 4-word one. Or a fragment. If the rhythm sounds metronomic when read aloud, break it. Aim for at least one sentence under 5 words and one over 25 words per paragraph.
2. **State your point from the first word.** Delete "It's important to note," "It's worth mentioning," and every other throat-clearing preamble. Say the thing.
3. **Describe consequences, not importance.** Don't call things pivotal, crucial, or groundbreaking. Say what actually happened. If you don't have specifics, say something plain: "This mattered" beats "This represented a pivotal paradigm shift."
4. **End sentences with the fact.** Never append a trailing participle commentary ("...highlighting its significance," "...contributing to the broader ecosystem"). The fact is the sentence. Stop there.
5. **Say what something is, not what it isn't-then-is.** "It's not just X, it's Y" and "Not just X, but Y" appear in ~6% of ChatGPT messages (WaPo study). Statistically, the strongest single tell.
6. **Take a position.** Don't default to "While X, Y is also important." You are allowed to state that X is better than Y. Diplomatic symmetry in every comparison is an AI signature.
7. **Replace abstract claims with concrete details.** Numbers, names, consequences. "Her model cut error rates from 12% to 3%" beats "Her research had a profound impact."
8. **Start the next sentence. No transition needed.** Never open with "Additionally," "Furthermore," or "Moreover." Use "And," "But," or "So" if you need a conjunction. Or just start.
9. **Write with texture.** Use "honestly," "look," "the thing is." Digress. Make asides. Show mild frustration or surprise. Monotone warmth is itself a tell. Think of how you'd explain this to a colleague, not how you'd present it to a committee.
10. **Default to flowing paragraphs.** Only use headers if the text exceeds ~500 words and covers genuinely distinct topics. Only use bullet points for 4+ parallel items. The Review Mode template is an intentional exception.
11. **Use commas, colons, semicolons, or parentheses for asides.** Never use em dashes. They are a strong AI tell (especially for OpenAI models, but increasingly for all LLMs).
12. **Use contractions.** Write "don't" not "do not," "can't" not "cannot," "it's" not "it is." Uncontracted forms in informal prose are an AI tell.
13. **Vary list lengths.** AI defaults to exactly three items (the "rule of three" tell). Use two items, or four, or five. Three is fine sometimes, but not every time.
14. **Use "is" and "has."** AI replaces simple copulas with "serves as," "features," "stands as," "represents." A city "has a park," it doesn't "boast a sprawling green space."
15. **Don't preview, state, then summarize.** Say it once. AI restates the same point at paragraph, section, and document level (fractal summaries). Trust the reader.

## Genre Sensitivity

Adjust strictness to genre. Academic papers legitimately hedge. Technical docs legitimately use headers and lists. Marketing copy legitimately promotes. Flag only patterns that are inappropriate for the genre at hand.

## Before and After

AI-flavored:
> The city's culinary landscape boasts a diverse array of restaurants, seamlessly blending traditional and modern cuisines to create a vibrant tapestry of flavors that caters to both locals and visitors alike.

Humanized:
> You can get outstanding pho and a great burger within two blocks of each other. The Thai place on 5th does a green curry that's worth the 40-minute wait.

AI-flavored:
> This pivotal development underscores the crucial role of open-source collaboration in fostering innovation, highlighting its enduring significance in the broader technology ecosystem.

Humanized:
> After this, three companies started contributing patches upstream. That hadn't happened before.

## Reference Files

- **Full tells catalog** with categorized examples: [references/tells-catalog.md](references/tells-catalog.md)
- **Reformulation guide** with before/after examples: [references/reformulation-guide.md](references/reformulation-guide.md)

Consult these when doing a detailed review or when uncertain about a specific pattern.

## Vocabulary Watchlist (Quick Reference)

**Tier 1** (always flag): *delve, tapestry* (abstract), *landscape* (abstract), *testament, boasts* (meaning "has"), *nestled, diverse array, rich tapestry, serves as, stands as*

**Tier 2** (flag when clustered): *crucial, pivotal, foster, bolster, underscore, highlight, emphasize, showcase, garner, leverage, navigate, ecosystem, framework, groundbreaking, renowned, seamlessly, comprehensive, vibrant, profound*

**Tier 3** (flag only at high density): *intricate, meticulous, enduring, interplay, cornerstone, notably, remarkable, commendable, noteworthy, enhance, cultivate*

One Tier 2 word in a paragraph is fine. Two Tier 2 words together, or any Tier 1 word, is a flag. "Pivotal" in a basketball article is fine; "pivotal, crucial, fostering, and underscores" in the same paragraph is not.
