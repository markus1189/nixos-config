# Reformulation Guide

## Contents
- [Vocabulary Fixes](#vocabulary-fixes)
- [Structure Fixes](#structure-fixes)
- [Tone Fixes](#tone-fixes)
- [Sentence-Level Techniques](#sentence-level-techniques)

## Vocabulary Fixes

Don't swap AI words for synonyms. Rethink the sentence entirely.

```
BAD:  "This pivotal development underscores the crucial role of..."
LAZY: "This important development shows the key role of..."  (still AI-sounding)
GOOD: "After this, the team couldn't ignore..."              (specific, human)
```

Replace abstract significance claims with concrete consequences. What actually happened? What changed? Say that instead.

```
BAD:  "The framework fosters a vibrant ecosystem of innovation."
GOOD: "Teams started sharing code between projects, something that hadn't happened before."
```

```
BAD:  "She garnered widespread recognition for her groundbreaking work."
GOOD: "Her paper got picked up by three major labs within a month."
```

### Fix copula replacements

AI avoids "is" and "has." It writes "serves as," "features," "stands as," "represents" instead. Use the plain verb.

```
BAD:  "The library serves as a central hub for the community."
GOOD: "The library is the center of the community."
```

```
BAD:  "The park features a sprawling network of walking trails."
GOOD: "The park has twelve miles of walking trails."
```

## Structure Fixes

### Break the uniformity

Vary sentence length deliberately. Follow a long sentence with a short one. Or a fragment.

```
BAD:  "The system processes incoming requests efficiently. It validates each
       request against the schema. It then routes valid requests to handlers."
GOOD: "Incoming requests hit the validator first. Schema check. If it passes,
       the router takes over; otherwise the request dies right there."
```

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

AI defaults to three items in every list. If you have two things to say, say two. If you have four, say four. Three is fine sometimes. Not every time.

```
BAD:  "The platform offers speed, reliability, and scalability."
GOOD: "The platform is fast and it stays up."
```

### Don't preview, state, then summarize

Say it once. AI restates the same point at three levels: the intro previews it, the body states it, the conclusion summarizes it. Trust the reader.

```
BAD:  "Error handling is critical. The system handles errors by...
       In summary, robust error handling ensures..."
GOOD: "The system catches panics at the boundary and logs them. Unrecoverable
       errors return a 500 with a correlation ID."
```

## Tone Fixes

### Add rough edges

Humans have opinions. They digress. They use "honestly," or "look," or "the thing is." They trail off. They make asides. Real writing has texture.

```
BAD:  "It's important to note that this approach has both advantages and disadvantages."
GOOD: "This works, mostly. The catch is..."
```

```
BAD:  "While challenges remain, the future outlook is promising."
GOOD: "There are still problems nobody's solved. Whether the new approach
       helps remains to be seen."
```

### Kill the hedging

Most hedging phrases add zero information. Delete them and start with the actual point.

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

### Use contractions

Uncontracted forms in informal or semi-formal prose are an AI tell. Write like you'd talk.

```
BAD:  "It does not support Windows. You will need to use WSL."
GOOD: "It doesn't support Windows. You'll need WSL."
```

## Sentence-Level Techniques

### Replace participle analysis with specifics

```
BAD:  "The town has 12,000 residents, creating a vibrant community."
GOOD: "The town has 12,000 residents."  (the "-ing" clause added nothing)
```

Or replace with something actually informative:
```
GOOD: "The town has 12,000 residents, about half of whom work at the refinery."
```

### Break negative parallelism

```
BAD:  "It's not just a tool, it's a paradigm shift."
GOOD: "It changed how three teams work."  (concrete, not dramatic)
```

### Use specific details instead of abstract claims

The strongest technique in this guide. Every time you want to say something is "significant" or "impactful," ask: significant *how*? Then say that.

```
BAD:  "Her research had a profound impact on the field."
GOOD: "Her model cut error rates from 12% to 3%, and two companies shipped
       products based on it within a year."
```

### Use "is" and "has" freely

```
BAD:  "The city boasts a thriving arts scene that serves as a cultural beacon."
GOOD: "The city has a good arts scene."
```
