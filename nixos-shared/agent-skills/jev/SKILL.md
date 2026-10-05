---
name: jev
description: "Runs cheap, fast typed judgments (yes/no, pick-one, rubric score) over text with Jev, TypeSafe's decision model, via the OpenRouter Decisions API, so bulk semantic reading happens in a script instead of in context. Use when the user mentions Jev, TypeSafe, System One, noul or typesafe/jev-1.13, or wants many items triaged, filtered, classified, routed, ranked, deduplicated or checked against criteria (feeds, logs, tickets, search results, comments, file lists). Not for generating text, reasoning, counting or arithmetic, and not for building TypeSafe into an application."
---

# Jev

Jev answers typed questions about a `state` you send it. It never writes prose. Cost is ~$0.04 per 1M input tokens, output is free, and a call takes ~0.3–0.8 s, so a judgment costs less than reading the item yourself.

**State leaves the machine** (OpenRouter → TypeSafe). If the user hasn't named Jev in this session, ask once before sending their private data. Never send secrets, tokens or credentials.

## Run it

```bash
./scripts/jev.py < request.json                                    # one request {state, questions}
./scripts/jev.py --each items.jsonl --questions q.json > out.jsonl # same questions per line; state = {"item": <line>}
./scripts/jev.py --each items.jsonl --questions q.json --context rubric.md   # shared text → state.context
./scripts/jev.py --dry-run ...                                     # print bodies; no key, no network
```

- Exit codes: 0 means ok, 1 means some requests failed or came back missing an asked question (those output lines carry `error`), 2 means a usage, input, key or billing problem, which stops the batch.
- The cost and the resolved model are printed on stderr. `--help` shows the defaults.
- Shrink the input with `rg` or code first; Jev judges whatever is left. Read only the shortlist from `out.jsonl`.
- `--each` output lines are `{"i": N, "answers"|"error": ...}`, where `i` is the 0-based index among the non-blank input lines; join back on it.

| type | asks | `criteria` | answer |
|---|---|---|---|
| `noul` | is this true? | optional `{"true": "...", "false": "..."}` | `noul` = P(yes), no confidence |
| `choice` | which one? (≤255 options) | `{"option": "description", ...}` | `choice`, `probabilities`, `confidence` |
| `score` | where on a rubric? | ordered array low→high, 2–10 levels | `score` (expected 0-based index), `probabilities`, `confidence` |

Every question needs `instructions`. A string works; use an object or array when the question needs definitions or contrasts. Example `q.json`:

```json
{"irreversible": {"type": "noul", "instructions": "Could running `item.cmd` destroy data or shared state that cannot be trivially restored?"},
 "team": {"type": "choice", "instructions": "Which team should handle `item.text`?",
          "criteria": {"billing": "charges, refunds", "tech": "bugs, outages, login", "none": "none of these"}}}
```

## Writing questions

- Ask one narrow judgment per question. Split "is this good" into the properties you actually care about.
- Question IDs are never sent to the model. Put the full meaning in `instructions`, including whether world knowledge may be used, and point into state with backticked paths such as `` `item.title` ``.
- For a choice, list every legal option plus `none`. For a score, describe a concrete situation at each level, with no numbers and no "more than the previous level". Phrase a noul so that "yes" is the thing you're looking for, consider writing its `false` criterion as the near miss rather than the plain opposite, and avoid double negatives and multi-hop logic.
- Questions in one request are answered independently. If one depends on another's answer, send a second request.

## Reading answers

- For a noul, ≥0.7 is yes and ≤0.3 is no. In between means *uncertain*, so read those items yourself. 0.5 means "can't tell", not "medium". P(X) and P(not X) asked separately don't sum to 1 (0.80–0.95 measured), so ask the side you act on.
- A low-`confidence` score is a flat distribution: a precise-looking 1.53 can mean nothing. High confidence isn't accuracy on contested items or numeric state.
- Values are rounded to 0.01, so break ties in code.
- Thresholds are uncalibrated. When the outcome matters, label 20–50 items yourself and compare.
- Before concluding, read the selected originals **and** a sample of the rejected and uncertain items.

## Don't

- Don't use it to count, do arithmetic, compare dates or produce text.
- Don't gate untrusted input with it: injected instructions in `state` move the answers, and planted false facts move them far more.
- Don't pad state. Context rot is documented, and state plus questions are capped at 32k tokens.
- Don't pack several items into one request: scores shift by position, in either direction, by up to ~0.37. Use `--each`.
- Don't trust answers for non-English or specialist domains (e.g. German accounting) without a labelled check. Write the instructions in English even when the state isn't.
- OpenRouter doesn't enforce every documented limit: a 1-level score is accepted and returns `confidence: 1`.

## Deeper

- [Docs index](https://docs.typesafe.ai/llms.txt); append `.md` to any page. Failure modes are listed on [model-jaggedness/jev-1.13](https://docs.typesafe.ai/model-jaggedness/jev-1.13.md).
- For building TypeSafe into an application, use the official skill [typesafe-ai/skills](https://github.com/typesafe-ai/skills/blob/main/skills/typesafe-ai/SKILL.md).

**Script Execution:** Always invoke scripts by absolute path: resolve `./scripts/` against this SKILL.md's directory. All scripts use Nix shebangs, so no dependency installation is needed.
