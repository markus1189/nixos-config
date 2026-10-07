---
name: jev
description: "Runs cheap, fast typed judgments (yes/no, pick-one, rubric score) over text with decision models: Jev (TypeSafe, via OpenRouter) and Clef (Cloudflare, via Requesty), alone or side by side, so bulk semantic reading happens in a script instead of in context. Use when the user mentions Jev, TypeSafe, Clef, Cloudflare's decision model, System One, noul, typesafe/jev-1.13 or sference/clef, wants to compare decision models or judge screenshots and images, or wants many items triaged, filtered, classified, routed, ranked, deduplicated or checked against criteria (feeds, logs, tickets, search results, comments, file lists). Not for generating text, reasoning, counting or arithmetic, and not for building TypeSafe into an application."
---

# Jev

Decision models answer typed questions about a `state` you send them. They never write prose. Output is free and a call takes ~0.3–1.2 s, so a judgment costs less than reading the item yourself. Jev is the default; everything below was measured on Jev unless it says Clef.

| `--model` | backend (inferred; `--via` overrides) | USD / 1M input | context |
|---|---|---|---|
| `jev` = `typesafe/jev-1.13` | OpenRouter `/systemone`, zero data retention enforced | 0.042 | 32k |
| `clef` = `sference/clef` | Requesty EU router (the org enforces EU residency) | 0.24 | 64k |

**State leaves the machine.** Jev: OpenRouter → TypeSafe, neither trains on nor retains it. Clef: Requesty → sference, with no per-request retention guarantee (OpenRouter has no zero-retention endpoint for Clef). If the user hasn't named the model in this session, ask once before sending their private data. Never send secrets, tokens or credentials.

## Run it

```bash
./scripts/jev.py < request.json                                    # one request {state, questions}
./scripts/jev.py --each items.jsonl --questions q.json > out.jsonl # same questions per line; state = {"item": <line>}
./scripts/jev.py --each items.jsonl --questions q.json --context rubric.md   # shared text → state.context
./scripts/jev.py --each items.jsonl --questions q.json --model jev,clef      # compare: same items to both
./scripts/jev.py --dry-run ...                                     # print {url, body}; no key, no network
```

- Exit codes: 0 means ok, 1 means some requests failed or came back missing an asked question (those output lines carry `error`), 2 means a usage, input, key or billing problem, which stops the batch.
- The cost and the resolved model are printed on stderr. `--help` shows the defaults.
- Shrink the input with `rg` or code first; Jev judges whatever is left. Read only the shortlist from `out.jsonl`.
- `--each` output lines are `{"i": N, "answers"|"error": ...}`, where `i` is the 0-based index among the non-blank input lines; join back on it.
- Compare mode keys `answers` (and `error`) by model and adds `disagree`: the question ids where a noul falls on opposite sides of 0.5, the choices differ, or scores divided by their top level index differ by ≥ 0.25. stderr reports the rate per question. `jq 'select(.disagree|length>0)'` gives the items worth reading.

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

## Clef

- More decisive than Jev: on 8 items whose title contradicted the body, Clef's urgent-yes answers were 0.94–0.98 where Jev's were 0.69–0.96, with the same verdicts. Jev's thresholds below are not validated for Clef; label items before trusting a cutoff.
- ~5× Jev's cost per item (8 items: USD 0.00078 vs 0.00016).
- State is sent as JSON text, not an object; Clef still followed `` `item.body` `` paths on all 8 decoy items.
- `sference/clef` takes at most 16 questions per request; 17 fail with a bare `Validation failed` 400. Split larger question sets.
- Only `sference/clef` is approved for the `api/requesty/systemone` key; `cloudflare/clef` and `cloudflare/clef-flash` answer 403 until approved in the Requesty Model Library.
- Images (≤4, PNG/JPEG/WebP) work only on `cloudflare/clef` via OpenRouter, which has no zero-retention endpoint for it: `--model cloudflare/clef --via openrouter --no-zdr --image shot.png`, or per item `"_images": ["a.png"]` in the JSONL (paths relative to the cwd). sference declares no image input, whatever Requesty's `supports_vision` says. `--no-zdr` still enforces no training on the data; ask before sending private images.

## Writing questions

- Ask one narrow judgment per question. Split "is this good" into the properties you actually care about.
- Put every question about an item in one `q.json`, including ones that only matter for some items: the item is paid for once, and answers don't change with what else is asked.
- Question IDs are never sent to the model. Put the full meaning in `instructions`, including whether world knowledge may be used, and point into state with backticked paths such as `` `item.title` ``.
- For a choice, list every legal option plus `none`. Jev leans towards the first option, so for a choice that matters, reorder the options and check the answer holds. For a score, describe a concrete situation at each level, with no numbers and no "more than the previous level". Phrase a noul so that "yes" is the thing you're looking for, consider writing its `false` criterion as the near miss rather than the plain opposite, and avoid double negatives and multi-hop logic.
- Questions in one request are answered independently. If one depends on another's answer, send a second request.

## Reading answers

- For a noul, ≥0.7 is yes and ≤0.3 is no. In between means *uncertain*, so read those items yourself. 0.5 means "can't tell", not "medium". P(X) and P(not X) asked separately don't sum to 1 (0.80–0.95 measured), so ask the side you act on.
- A low-`confidence` score is a flat distribution: a precise-looking 1.53 can mean nothing. A score between levels is a position, not a magnitude; to combine scores, divide each by its top level index first. High confidence isn't accuracy on contested items or numeric state.
- Values are rounded to 0.01, so break ties in code.
- Thresholds are uncalibrated. When the outcome matters, label 20–50 items yourself and compare.
- Before concluding, read the selected originals **and** a sample of the rejected and uncertain items.

## Don't

- Don't use it to count, do arithmetic, compare dates or produce text.
- Don't gate untrusted input with it: injected instructions in `state` move the answers, and planted false facts move them far more.
- Don't pad state. Context rot is documented, and state plus the longest question are capped at 32k tokens: keep an item under ~100k characters and truncate longer ones in code.
- Don't pack several items into one `state`: scores shift by position, in either direction, by up to ~0.37. Use `--each`. TypeSafe's own pattern for comparing one state against many candidates (dedupe, rerank) puts each candidate in its own question's `instructions` object instead; that pays for a large shared state once, but its position effects are unmeasured here.
- Don't trust answers for non-English or specialist domains (e.g. German accounting) without a labelled check. Write the instructions in English even when the state isn't.
- OpenRouter doesn't enforce every documented limit: a 1-level score is accepted and returns `confidence: 1`.

## Deeper

- [Docs index](https://docs.typesafe.ai/llms.txt); append `.md` to any page. Failure modes are listed on [model-jaggedness/jev-1.13](https://docs.typesafe.ai/model-jaggedness/jev-1.13.md).
- For building TypeSafe into an application, use the official skill [typesafe-ai/skills](https://github.com/typesafe-ai/skills/blob/main/skills/typesafe-ai/SKILL.md).

**Script Execution:** Always invoke scripts by absolute path: resolve `./scripts/` against this SKILL.md's directory. All scripts use Nix shebangs, so no dependency installation is needed.
