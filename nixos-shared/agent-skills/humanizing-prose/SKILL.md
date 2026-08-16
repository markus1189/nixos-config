---
name: humanizing-prose
description: "The 'de-ai' skill: writes, rewrites, or reviews prose to remove AI/LLM tells. Use whenever the user says 'de-ai' in any phrasing ('de-ai', 'apply de-ai', 'de-ai per skill'), asks to humanize text, de-AI writing, review for AI tells, write naturally, sound less robotic, sound less ChatGPT, sound more human, edit for voice, make text authentic, or mentions AI detection concerns — German phrasings too ('klingt nach KI', 'menschlicher formulieren') — and when composing outgoing emails, letters, comments, or posts meant to read as human-written. Handles English and German. Never claim de-ai was applied without loading this skill."
---

# Humanizing Prose

Remove the patterns that make text read as AI-generated — by rethinking sentences the way a specific, opinionated human would write them, not by swapping synonyms. The user's alias for this skill is **"de-ai"**. Never claim to have applied it without actually loading and following it.

## The Fidelity Contract (overrides everything below)

1. **Never invent facts.** No new numbers, names, events, examples, or anecdotes. In Rewrite Mode, specifics may come only from the source text, context the user provided, or the user after you ask. No specific available? Use a plain statement ("This mattered") or insert `[specific: what changed?]` and say so. In correspondence, commitments, offers, who-does-what, and how the user presents themselves are facts too — do not alter them.
2. **Style changes, meaning stays.** Every claim, stance, and hedge in the output must be traceable to the input. A hedge that carries genuine uncertainty is content — keep the uncertainty. Don't touch direct quotes, citations, code, or legally significant wording. Don't manufacture opinions the author doesn't hold. Exception: chat-register and residue sentences (reassurance, reader-praise, assistant offers) are noise, not content — deleting them never violates meaning preservation.
3. **Preserve the author's voice.** Before editing, note the input's register, formality, punctuation habits, and brevity; remove tells *within* that voice. This user's own register is terse and plain — don't return something longer or chattier than the draft. These rules subtract AI patterns; they never prescribe a replacement voice. If every text you edit converges on the same sound, you've created a new tell.

## Choosing a Mode

- **Rewrite Mode** — text provided without a review verb, especially a draft just produced in this session. Scan silently, return the rewritten text, then briefly note the key changes **and what you left alone because it was already human** (when anything qualifies — it builds trust; if nothing did, say so in one line).
- **Review Mode** — "check", "audit", "review", "rate"; or a bare invocation on text the user has already hand-edited. Report findings (format below), then offer to apply them.
- **Writing Mode** — composing new prose. Write clean from the start, in a plain, direct voice matched to the audience.

If the text is already clean, say so and change nothing — never manufacture edits. Cutting filler shortens text naturally; don't compress *content* (if a length must be kept, say what you'd cut instead). Finish the loop: once fixes are accepted, apply them to the actual file or draft. Ask about mode only when the modes would produce materially different deliverables for a high-stakes text; otherwise pick one, state the assumption in one line, and proceed.

## Core Rules

1. **Vary rhythm where meaning warrants it.** Mix sentence lengths, but never on a schedule: a fragment must land a real point of emphasis. Formulaic long-short alternation, forced fragments ("And it works."), and one-line drama paragraphs ("Let that sink in.") are the recognized *humanizer* signature. Test: read aloud — metronomic is bad, but so is a drum solo.
2. **State your point from the first word.** Delete "It's important to note" and every other throat-clearing preamble.
3. **Describe consequences, not importance.** Nothing is pivotal, crucial, or groundbreaking; say what actually happened — within the fidelity contract. But an importance claim that *is* the assertion (a priority call, a decision: "establishing the baseline comes first") is content, not throat-clearing — keep it. Throat-clearing introduces another clause; a claim that ends at the period is the point.
4. **End sentences at the fact.** No trailing participle commentary ("...highlighting its significance").
5. **Say what something is, not what it isn't-then-is.** The negative-parallelism family is the top phrase-level tell: "It's not X, it's Y", "Not X. Not Y. Just Z.", "The question isn't X. The question is Y.", "not because X, but because Y", "X — not Y". Fix: delete the dismissed half and state the claim — unless the negated half carries real content (a product that genuinely has no agents): then keep the content and restructure into a plain statement ("runs without agents or configuration").
6. **Keep the author's stance.** Don't hedge a position they took; don't manufacture one they didn't. Diplomatic both-sides symmetry in every comparison is an AI signature — but only the author can decide the side.
7. **Concrete beats abstract — with the source's specifics only.** "Her model cut error rates from 12% to 3%" beats "profound impact" *when the source says so*. Otherwise: plain statement or ask.
8. **Start the next sentence; skip the transition.** No "Additionally/Furthermore/Moreover". "And", "But", "So", or nothing.
9. **Texture without stock candor.** Texture comes from specifics, mild stance, digressions, and asides drawn from the material — the way you'd explain it to a colleague. Never from a phrase list: "honestly", "look", "the thing is", "let's be real", "here's the kicker" as openers are now recognized AI tells in their own right — current models emit them natively when told to sound casual. (As detection evidence they count only as a cluster; see the catalog.) A candor word must be earned by what follows, and rare.
10. **No chat register, no chat residue.** Never reassure against objections nobody raised ("You're not broken", "You're not alone"), never praise the reader for asking or noticing ("You didn't just X, you Y"), never announce your tone ("I'll be blunt:") — be it. No "Great question", no engagement hooks ("Picture this:", "Here's where it gets interesting"). And in any mode, delete assistant-turn residue: "Would you like me to...", "Let me know if...", "I hope this helps" — provenance artifacts, not content.
11. **Em dashes: none in outgoing text.** Standing user rule, stricter than the general craft norm — no "one idiomatic exception". Restructure with a comma, colon, parentheses, or a sentence break; never swap in a bare hyphen (hyphen-doing-em-dash-work is its own tell). For judging *other people's* text, use the softer detection rule in [references/tells-catalog.md](references/tells-catalog.md) (density + spaced style, not presence).
12. **Use contractions in informal English.** "don't", not "do not". (Writing advice for produced text — never a detection heuristic; plenty of humans skip contractions.)
13. **Vary list lengths.** Two items, or four, or five. Exactly three, every time, is the tell.
14. **Use "is" and "has".** Not "serves as", "features", "stands as", "represents", "boasts".
15. **Say it once, and make each paragraph add something.** No preview-state-summarize. Compression test for the whole piece: if it regenerates from its one-sentence thesis with nothing lost, it's padding — keep the paragraph that says it best, cut the rest. Cut-only: never bulk padding up with invented substance.
16. **Structure only when it earns it.** Headers only past ~500 words with genuinely distinct topics; bullets only for 3+ truly parallel items (short inline two-item lists are just prose). No bold-term-colon formatting reflex. No emoji as formatting.

## German Text (half of real usage)

The vocabulary watchlist below is English-only — for German, lean on the structural rules (1–16 all apply) plus these observed German tells:

- **Boilerplate politeness**: "Ich erlaube mir, darauf hinzuweisen", "Ich verstehe das natürlich, trotzdem...", doubled Konjunktiv-courtesy ("Ich würde mich freuen, wenn Sie ... könnten"). "Vielen Dank für Ihre Rückmeldung" is a tell only as a reflex opener when nothing was actually received; acknowledging a real reply is earned — keep it.
- **Transition stacking**: "zudem", "darüber hinaus", "des Weiteren", "außerdem" chained across sentences. A single idiomatic connector is fine; the tell is a different one bolted onto every sentence.
- **Symmetric diplomacy**: "nicht nur ... sondern auch", "sowohl ... als auch" everywhere; balanced concession formulas.
- **Significance inflation auf Deutsch**: "spielt eine entscheidende Rolle", "unterstreicht", "im Herzen von", heavy Nominalstil where a verb would do.
- **Typography**: prefer commas and colons in German too — the zero-dash rule (Rule 11) covers produced German text. German asides in *others'* text use a spaced en dash (" – ", the Gedankenstrich): that is standard typography — in Review Mode never flag its presence, only unusual density. An em dash in German text is wrong typography and a machine hint.
- **Register**: match Sie/Du from the thread and keep formal genres formal — a Behörden letter stays höflich-förmlich, it just shouldn't sound like a committee wrote it ("nach Komitee klingen").

## Review Mode

For texts under ~150 words: skip the template — give a one-line verdict on the same Clean/Light/Flavored/Heavy scale (judged by tell strength, not per-250-word density, at this size) plus inline fixes. Otherwise:

```
## AI Tells Review

**Verdict**: [Clean / Light (≲1 tell per 250 words) / Flavored (2-3 per 250) / Heavy (4+ per 250)]

**Tier 0 (provenance)**: [chat residue, placeholders, leaked tokens — one hit outweighs all style signals; check first]
**Tier 1 (strong, per-instance)**: [e.g. negative-parallelism family, trailing participles, therapy-speak/reader-praise, notability boilerplate — with quotes]
**Tier 2 (cluster)**: [e.g. vocabulary clusters, rule-of-three, significance inflation, announced tone, fake-casual register — with quotes]
**Tier 3 (weak, supporting only)**: [e.g. em-dash density, title case, too-clean coherence]
**Style-only / Legacy**: [worth fixing as writing, or dates the text; no diagnostic weight]

### Suggested rewrites
[original → fix, one line of why]
```

The bucket examples above suffice for short texts; full tier assignments live in [references/tells-catalog.md](references/tells-catalog.md) — read it for any contested or borderline call. Core Rules are writing guidance: in Review Mode a rule violation counts as a tell only at its catalog tier, and patterns that are also common human habits (polite preambles, formal hedges) may be offered as optional tightening, explicitly labeled as not an AI signal. This is a style review, not an authorship determination — these patterns also occur in ESL, formally schooled, and neurodivergent human writing, and average readers distinguish AI text at roughly chance. (The template's report formatting is deliberate — it *is* a report.)

**Never flag in isolation**: perfect grammar or spelling, formal register, absence of contractions, transition words like "however", semicolons, absence of typos, "robotic vibes", unsourced content, mixed registers. And never insert their opposites (typos, hedges, wordiness) to fake humanity — see the anti-patterns section of [references/reformulation-guide.md](references/reformulation-guide.md).

## Genre Sensitivity

Adjust strictness to genre. Academic papers hedge legitimately; technical docs use headers and lists; support-case emails keep their case-summary blocks; a neutral StackExchange answer needs no injected chattiness; formal letters stay formal. Flag only what's inappropriate *for the genre at hand* — restraint is this skill's most-praised behavior.

## Vocabulary Watchlist (English quick reference)

Single words are never proof; clusters and voice-mismatch are the signal. Word tells decay within a model generation — this table is stamped **mid-2026**; when it feels stale, check the live-reference pointer in [references/tells-catalog.md](references/tells-catalog.md).

- **Current era (2025–2026)**: *quietly* (drama-adverb: "quietly building"), *genuinely*, *load-bearing / hinge / seams* (metaphorical), *emphasizing, highlighting, showcasing, enhance*; bigrams that outlive era churn: *complex and multifaceted, intricate interplay, played a crucial role, that's a great question*.
- **Fading (2024–2025)**: *align with, fostering, underscore, garner, bolster, leverage, navigate, ecosystem, seamlessly, comprehensive*.
- **Legacy (2023–mid-2024 — dates text rather than flagging it; also ordinary dialect for many ESL writers)**: *delve, tapestry, testament, landscape, boasts, nestled, vibrant, intricate, meticulous, diverse array*. When *writing*, still avoid all eras — readers pattern-match them as AI regardless of vintage.

## Reference Files

For texts under ~500 words (nearly all real usage), this file is sufficient — don't read the references. Consult them when:

- **Long-form or contested review, borderline tier calls, era questions, provenance artifacts**: read [references/tells-catalog.md](references/tells-catalog.md) before producing the report.
- **Long-form rewriting or before/after phrasing patterns**: read [references/reformulation-guide.md](references/reformulation-guide.md).
