# Reformulation Guide

BAD/GOOD pairs for fixing tells, keyed to the Core Rules in SKILL.md. One law governs every technique here: **specifics may only come from the source text, provided context, or the user** — see the fidelity contract in SKILL.md. Examples below that show concrete numbers assume those facts were available; the technique demonstrates *form*, never license to invent.

## Contents
- [Vocabulary Fixes](#vocabulary-fixes)
- [Structure Fixes](#structure-fixes)
- [Tone Fixes](#tone-fixes)
- [Sentence-Level Techniques](#sentence-level-techniques)
- [Anti-Patterns: How Not to Humanize](#anti-patterns-how-not-to-humanize)

## Vocabulary Fixes

Don't swap AI words for synonyms. Rethink the sentence entirely.

```
BAD:  "This pivotal development underscores the crucial role of..."
LAZY: "This important development shows the key role of..."  (still AI-sounding)
GOOD: "After this, the team couldn't ignore..."              (specific, human)
```

Replace abstract significance claims with concrete consequences — *when you know them*. What actually happened? What changed? Say that instead.

```
BAD:  "The framework fosters a vibrant ecosystem of innovation."
GOOD: "Teams started sharing code between projects, something that hadn't happened before."
      (only if the source establishes this — otherwise: "Teams actually use it," or ask)
```

If no specific is available, degrade to a plain statement rather than inventing one:

```
BAD:   "Her research had a profound impact on the field."
GOOD (facts known):   "Her model cut error rates from 12% to 3%."
GOOD (facts unknown): "Her research changed how the field works." — or insert
       [specific: what changed after her paper?] and ask the user.
```

### Fix copula replacements (Rule: use "is" and "has")

```
BAD:  "The library serves as a central hub for the community."
GOOD: "The library is the center of the community."
```

```
BAD:  "The park features a sprawling network of walking trails."
GOOD: "The park has twelve miles of walking trails."  (the number came from the source)
```

## Structure Fixes

### Vary rhythm where the meaning warrants it

A short sentence or fragment should land an actual point of emphasis — not perform humanity on a schedule.

```
BAD:  "The system processes incoming requests efficiently. It validates each
       request against the schema. It then routes valid requests to handlers."
GOOD: "Incoming requests hit the validator first. Schema check. If it passes,
       the router takes over; otherwise the request dies right there."
```

Caution (SKILL.md Rule 1): mechanical long-short alternation, forced fragments ("And it works."), and one-line dramatic paragraphs ("Let that sink in.") are the recognized *humanizer* signature. If your edit gives every paragraph the same punchy heartbeat, you've replaced one uniformity with another.

### Kill the textbook pattern

Not every paragraph needs topic, support, conclusion. Some paragraphs can be one sentence. Some can start with the punchline. Some can trail off into a question.

### Vary transitions

Instead of "Additionally," "Furthermore," "Moreover,":
- Just start the next point. No transition needed.
- Use "And" or "But" to start a sentence.
- Reference the previous idea specifically: "That constraint led to..."
- Use contrast or surprise: "Except it didn't work."

### Remove unnecessary structure

Ask: does this really need bullet points, or would a paragraph be more natural? Would a human actually use headers here, or just keep talking?

### Break the rule of three

If you have two things to say, say two. If four, say four. Three is fine sometimes. Not every time.

```
BAD:  "The platform offers speed, reliability, and scalability."
GOOD: "The platform is fast and it stays up."
```

### Say it once (fractal summaries, one-point dilution)

Don't preview, state, then summarize. And apply the compression test to the whole document: if the piece could be regenerated from its one-sentence thesis with nothing lost, it's padding. The fix is cut-only:

```
BAD:  Three paragraphs each restating "consistency beats intensity" with new metaphors.
GOOD: Keep the paragraph that says it best. Cut the other two. Never bulk up
      padding with invented evidence to make it look substantive.
```

## Tone Fixes

### Texture without stock phrases

Per SKILL.md Rule 9: texture comes from specifics, mild stance, digressions, and asides drawn from the material — never from stock candor markers ("honestly", "look", "the thing is", "let's be real", "here's the kicker"), which current models emit natively and readers now parody. A candor word must be earned by what follows, and rare.

```
BAD (old AI):  "It's important to note that this approach has both advantages
                and disadvantages."
BAD (new AI):  "Honestly? This approach slaps. But here's the kicker: it has
                real trade-offs."
GOOD:          "This works, mostly. The two failure modes both involve the cache."
```

### Don't announce your tone — have it

```
BAD:  "I'm going to be straight with you: the design is broken."
GOOD: "The design is broken."
```

### Don't manage the reader's feelings

Delete therapy-speak and reader-praise from informational prose. The text should answer the question, not reassure the asker.

```
BAD:  "You're not wrong to worry about this — noticing it already puts you
       ahead of most teams. Take a breath: the migration is manageable."
GOOD: "The migration takes about two days and the risky part is the schema change."
```

### Kill the hedging

Most hedging phrases add zero information. Delete them and start with the actual point. (But see the fidelity contract: a hedge that expresses the *author's genuine uncertainty* — "we suspect, but haven't confirmed" — is content. Keep the uncertainty; you may still rephrase it plainly.)

```
BAD:  "It's worth mentioning that the API has rate limits."
GOOD: "The API has rate limits."
```

```
BAD:  "It should be noted that this only works on Linux."
GOOD: "This only works on Linux."
```

### Remove the sales pitch

If you catch yourself inflating significance, ask: would the reader roll their eyes? If yes, cut it.

```
BAD:  "This innovative solution leverages cutting-edge technology to deliver
       a seamless, best-in-class experience."
GOOD: "It works. Setup takes five minutes."
```

### Use contractions (in your own informal prose)

```
BAD:  "It does not support Windows. You will need to use WSL."
GOOD: "It doesn't support Windows. You'll need WSL."
```

This is writing advice for text you produce. It is not a detection rule — plenty of humans write without contractions.

## Sentence-Level Techniques

### Replace participle analysis with specifics — or just stop

```
BAD:  "The town has 12,000 residents, creating a vibrant community."
GOOD: "The town has 12,000 residents."  (the "-ing" clause added nothing)
```

Or, when the source supplies something informative:
```
GOOD: "The town has 12,000 residents, about half of whom work at the refinery."
```

### Break negative parallelism (all variants)

Delete the dismissed half; state the actual claim.

```
BAD:  "It's not just a tool, it's a paradigm shift."
GOOD: "Three teams changed their workflow after adopting it."  (from the source)
```

```
BAD:  "No hardware. No fees. Just growth."
GOOD: "There's nothing to install and no monthly fee."
```

```
BAD:  "The question isn't whether to migrate. The question is when."
GOOD: "We should migrate this year."
```

### Remove em dashes by restructuring, never by laundering

For outgoing text, SKILL.md Rule 11 applies: no em dashes at all, no "one idiomatic exception". The fix is a real restructure — comma, colon, parentheses, or sentence break. What you must never do is the mechanical swap:

```
BAD (AI cadence):   "It's not just fast — it's transformative — and that
                     changes everything — for everyone."
BAD (laundered):    "It's not just fast - it's transformative - and that..."
                     (hyphens doing em-dash work scream post-processing)
GOOD:               "It's fast. Whether that changes your workflow depends
                     on how often you hit the old bottleneck."
```

(Judging someone else's text: use the softer density-based rule in [tells-catalog.md](tells-catalog.md), not presence. German text: see SKILL.md's German section for the Gedankenstrich rules.)

## Anti-Patterns: How Not to Humanize

Each of these "fixes" degrades the text and most are now recognized tells in their own right:

- **Never inject typos, misspellings, grammatical errors, lowercase affectation, or fake "edit:" markers.** Modern detectors normalize surface noise, readers read it as authenticity performance, and it makes the writing worse.
- **Never mechanically strip or swap punctuation** (em dashes → hyphens, deleting every semicolon). Visible AI-avoidance is its own signature; match the author's actual habits instead (outgoing em dashes are still removed per SKILL.md Rule 11 — by restructuring, never by swapping).
- **Never inject stock candor or slang** ("honestly", "look", "no cap", "chef's kiss") to simulate voice.
- **Never add hedges, superlatives, or wordy constructions** because the do-not-flag list says humans use them. They're compatible with human writing, not ingredients of it.
- **Never pad with invented specifics, examples, or anecdotes** to make text feel grounded. Fidelity first; a plain true sentence beats a vivid invented one.
- **Never impose one house voice on every text.** If your edits make all documents sound like the same casual blogger, you have created a new tell — and detection vendors train on exactly that corpus.
