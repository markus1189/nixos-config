---
name: jev
description: "Runs cheap, fast typed judgments (yes/no, pick-one, rubric score) over text and images with decision models, Jev (TypeSafe) and Clef (Cloudflare), alone or side by side, so bulk semantic reading happens in a script instead of in context. Use when the user mentions Jev, TypeSafe, Clef, Cloudflare's decision model, System One, noul, typesafe/jev-1.13 or sference/clef, wants to compare decision models or judge screenshots and images, find the relevant lines in a long file, pick a value among candidates, rerank, dedupe or check claims against a source, or wants many items triaged, filtered, classified, routed, ranked, deduplicated or checked against criteria (feeds, logs, tickets, search results, comments, file lists). Not for generating text, reasoning, counting or arithmetic, and not for building TypeSafe into an application."
---

# Jev and Clef (decision models)

Decision models answer typed questions about a `state` you send them. They never write prose. Output is free, so a judgment costs less than reading the item yourself. Facts below were measured on Jev unless marked Clef.

| `--model` | route (inferred from the id; `--via` overrides) | USD / 1M input | context | images |
|---|---|---|---|---|
| `jev` = `typesafe/jev-1.13` | OpenRouter, zero data retention, no training | 0.042 | 32k | no |
| `~typesafe/jev-preview`, or `jev --via typesafe` | `--no-zdr`: TypeSafe's own API, no training, retention possible (ZDR is enterprise-only) | 0.042 | 32k | no |
| `clef` = `sference/clef` | Requesty EU router, no retention guarantee | 0.24 | 64k | no |
| `cloudflare/clef`, `cloudflare/clef-flash` | `--via openrouter --no-zdr`: OpenRouter, no training, retention possible | 0.24 / 0.09 | 64k | ≤4 |

**Pick:** Jev by default: cheapest, ~0.3–0.8 s a call, and the only route with zero retention. Clef when the user asks for it or an item won't fit 32k tokens. `cloudflare/clef --via openrouter --no-zdr` for anything with images; no alias takes images, and images can't go to Jev, so they can't be compared against it. `--model jev,clef` when you have no labels and want the contested items. `--via typesafe` only when OpenRouter itself fails (gateway errors, credits). To test a preview build against your thresholds, first send one request with `--model '~typesafe/jev-preview' --no-zdr` alone and read its `model` (still `jev-1.13.0` on 2026-10-08; a compare run's summary merges both models' ids); only if it differs, run `--model jev,~typesafe/jev-preview --no-zdr`.

**State leaves the machine.** Only Jev on its default OpenRouter route has zero retention; Jev via Requesty or TypeSafe's own API is refused unless `--no-zdr` accepts that. Before sending the user's private data anywhere else, get their OK once per session unless they named that route, and say that retention isn't guaranteed. Images lose their metadata (EXIF, GPS) before sending unless `--no-downscale`. Never send secrets, tokens or credentials.

## Run it

```bash
./scripts/jev.py < request.json                                    # one request {state, questions[, model]}; a model there must match any --model
./scripts/jev.py --each items.jsonl --questions q.json > out.jsonl # one JSON value per line (quote plain text: "…"); state = {"item": <value>}
./scripts/jev.py --each items.jsonl --questions q.json --context rubric.md   # shared JSON or text → state.context
./scripts/jev.py --each items.jsonl --questions q.json --model jev,clef      # compare: same items to both
./scripts/jev.py --each shots.jsonl --questions q.json --model cloudflare/clef --via openrouter --no-zdr --image legend.png
./scripts/jev.py --dry-run ...                                     # one {url, body} line per item × model, in input order; no key, no network
```

- Output: a single request prints one indented object, `{model, answers, usage}` or `{error}` (with `--model a,b`, the compare shape below without `i`). `--each` prints JSONL `{"i": N, "answers"|"error": ...}`, where `i` is the 0-based index among the non-blank input lines; join back on it.
- Compare mode keys `answers` and `error` by full model id and adds `disagree`: the question ids where a noul falls on opposite sides of 0.5, the choices differ, or scores divided by their top level index differ by ≥ 0.25. The noul rule is deliberately loose, to surface candidates. Lines where a model failed carry `error` and no `disagree`, so select `.error` too or you lose them. stderr gives the counts per question.
- Exit 0: all answered. Exit 1: some lines carry `error`. Codes `0` (transport), 408, 429, 500, 502–504, 520, 524, 529 and OpenRouter's in-flight 402 were already retried, so rerun those `i` later. The rest fail again until you change the item, the questions or the model: other HTTP statuses (400, 413; 404 usually means a wrong model or route), `waf_403` or a 403 with metadata (that item's content was blocked), `missing_answers`, `bad_answer`, `no_answers`, `bad_response`, `error`, `image` (bad `_images` entry).
- Exit 2: nothing was sent (usage, input, missing key, or a refusal ending in "retry with --do-it-anyway"), or a 401/402/403 stopped the batch (bad key, no credits, model not approved). Requests already in flight still finish, so after fixing the cause rerun every line that carries `error`, not only `"code": "skipped"`.
- `--dry-run` shows what would be sent; it can't catch server-side rejections such as a 403 for an unapproved model.
- Shrink the input with `rg` or code first; the model judges whatever is left. Read the shortlist, then the samples Reading answers asks for.

| type | asks | `criteria` | answer |
|---|---|---|---|
| `noul` | is this true? | optional `{"true": "...", "false": "..."}` | `noul` = P(yes), no confidence |
| `choice` | which one? (Jev: ≤255 options) | `{"option": "description", ...}` | `choice`, `probabilities`, `confidence` |
| `score` | where on a rubric? | ordered array low→high (Jev: 2–10 levels; OpenRouter accepts 1 and returns a meaningless `confidence: 1`) | `score` (expected 0-based index), `probabilities`, `confidence` |

Every question needs `instructions`. A string works; use an object or array when the question needs definitions or contrasts. Example `q.json`:

```json
{"irreversible": {"type": "noul", "instructions": "Could running `item.cmd` destroy data or shared state that cannot be trivially restored?"},
 "team": {"type": "choice", "instructions": "Which team should handle `item.text`?",
          "criteria": {"billing": "charges, refunds", "tech": "bugs, outages, login", "none": "none of these"}}}
```

## Shapes

| task | run |
|---|---|
| where in one long text (log, doc, diff) is X ([recipe](https://docs.typesafe.ai/cookbooks/semantic_find.md)) | one request: state = the lines prefixed `L1:`…; a choice whose criteria map each line id to `null` (≤255 lines, else narrow to a window first), plus a noul "does any line answer X?", since a choice ranks some line first even when none fits |
| pull a value (date, URL, amount, name) ([recipe](https://docs.typesafe.ai/cookbooks/pre_parsed_value_extraction_cookbook.md)) | over-find candidates with `rg`/regex, then a choice over them plus `none`; copy the pick verbatim. It can't pick a value you didn't list. Dates ([recipe](https://docs.typesafe.ai/cookbooks/date_extraction_cookbook.md)): a choice for how it's written (absolute, relative, absent), then one per part (month, day, year, relative anchor), assembled in code |
| best matches for a query ([recipe](https://docs.typesafe.ai/cookbooks/rerank_typesafe.md)) | shortlist with `rg`, then `--each` over the candidates with the query in `--context` and a noul "Does `item` answer `context`?"; sort on it |
| dedupe ([recipe](https://docs.typesafe.ai/cookbooks/entity_alignment.md)) | candidate pairs from code, one `{"a": …, "b": …}` line each; a score whose middle level is "related, possibly not the same" (read those), a noul per field, numbers compared in code. A wrong merge usually costs more than a miss, so cut high |
| pick one of many (skills, files, categories) ([recipe](https://docs.typesafe.ai/cookbooks/skill_suggestion.md), [taxonomy](https://docs.typesafe.ai/cookbooks/hierarchical_classification.md), [parent fallback](https://docs.typesafe.ai/cookbooks/classification_using_confidence.md)) | request 1: a choice over all names with short descriptions plus a noul "is any needed?"; request 2: the top 3 with full text, a noul each, drop all if every one is low. Deep taxonomies: keep the 3 best paths at each depth, ranked by the geometric mean of their probabilities; report the parent when `confidence` is low |
| check claims against a source ([recipe](https://docs.typesafe.ai/cookbooks/citation_check.md)) | match quotes exactly in code first; per claim, the cited section as state and a choice `supported`/`unsupported`/`contradicted`; read the low-confidence ones |

Options in one choice compete and Jev leans towards the first, so a single-request ranking says where to look, not the verdict. The recipes are Python SDK code: their question wording carries over to `q.json`, their thresholds were measured on their data. More in the [cookbook index](https://docs.typesafe.ai/cookbooks.md).

## Clef

- More decisive than Jev: on 8 items whose title contradicted the body, Clef's urgent-yes answers were 0.94–0.98 where Jev's were 0.69–0.96, with the same verdicts, so Jev's cutoffs don't carry over.
- Requesty gets state as JSON text, not an object; Clef still followed `` `item.body` `` paths on all 8 decoy items.
- `sference/clef` takes at most 16 questions; more are refused before sending. Split them.
- On Requesty only `sference/clef` is approved for the `api/requesty/systemone` key; `cloudflare/clef[-flash]` there get a 403 that stops the batch.
- Images: `--image shot.png` for every request, plus, per item, `"_images": ["a.png"]` in an object line (paths relative to the cwd; the key is removed from `item`, and `null` is an item error). sference declares no image input, whatever Requesty's `supports_vision` says.
- Images are shrunk before sending: to 1024 px, then to JPEG (transparency flattened onto white) until the request fits 360 KiB, state text included (for `--image`, the longest item's). Each change is reported: stderr for `--image`, a `downscaled` list in the item's line for `_images`. Cost stops growing at 1024 px, and the server refuses with 413 before inference somewhere between 376 and 402 KB of image, far below the documented 4 MiB. `--no-downscale` sends files unchanged.
- `--image` sets over 4 images, or whose images alone exceed the byte budget, are refused; anything that goes over only with an item's text or `_images` is sent with a stderr warning, as are per-item `_images` over the limits, so one item can't sink the batch.

**Refusals** that rest on provider facts (images per model, `--no-zdr` for Clef via OpenRouter, image count and bytes, sference's question cap) end in "retry with --do-it-anyway". Force only when you think the provider changed. The flag never drops ZDR. A forced request that succeeds names the stale check to fix in `~/repos/nixos-config/nixos-shared/agent-skills/jev/scripts/jev.py`; for image refusals it asks for a control question first, since Requesty answers while silently dropping images.

## Writing questions

- Ask one narrow judgment per question: the observable fact that decides it, not the conclusion. To rejoin wrapped lines, "Does L_i pick up mid-sentence?" kept list items apart where "same paragraph?" left them uncertain; to route, ask whether `item` asks to act on files, accounts, devices or services rather than only explain. Split "is this good" into the properties you actually care about.
- Ask each disqualifier ("any serious violation fails") as its own noul and combine conditions with and/or in code: a weighted score lets strengths offset a violation.
- For tags that can apply together, ask one noul per tag: a choice's probabilities sum to 1, so true tags compete and only one wins.
- Put every question about an item in one `q.json`, including ones that only matter for some items: the item is paid for once. Write the premise into such a question ("If `item` reports a bug, how severe…") and use its answer, and its uncertainty, only where the premise holds. Questions are answered independently, so one can't use another's answer; chain a second request for that.
- Question IDs are never sent to the model. Put the full meaning in `instructions`, including whether world knowledge may be used, and point into state with backticked paths such as `` `item.title` ``.
- For a choice, list every legal option plus `none`. Jev leans towards the first option, so for a choice that matters, reorder the options and check the answer holds. For a score, describe a concrete situation at each level, with no numbers and no "more than the previous level". Phrase a noul so that "yes" is the thing you're looking for, consider writing its `false` criterion as the near miss rather than the plain opposite, and avoid double negatives and multi-hop logic.
- If a score or choice keeps splitting between neighbouring options on items you think are clear, make each `criteria` entry an object with the same keys on every entry (what it covers, `not_for`, a few examples like your real items). Judge the change on labelled items, not on `confidence`.

## Reading answers

- For a noul, ≥0.7 is yes and ≤0.3 is no. In between means *uncertain*, so read those items yourself. 0.5 means "can't tell", not "medium". P(X) and P(not X) asked separately don't sum to 1 (0.80–0.95 measured), so ask the side you act on. Shift the band by what errors cost: when a missed yes is expensive, read lower values too; when acting on a false yes is, demand more.
- A choice's `confidence` is `(p_max − 1/n)/(1 − 1/n)` (measured). Low means no clear winner: read the item, or also act on the runner-up in `probabilities` (e.g. copy the second team).
- A low-`confidence` score is a flat distribution: a precise-looking 1.53 can mean nothing. A score between levels is a position, not a magnitude; to combine scores, divide each by its top level index first. High confidence isn't accuracy on contested items or numeric state.
- Jev rounds values to 0.01, and repeated calls jitter (one borderline noul spanned 0.43–0.53 over 15 calls), so break ties in code.
- Thresholds are uncalibrated. When the outcome matters, label 20–50 items yourself and compare.
- Before concluding, read the selected originals **and** a sample of the rejected and uncertain items.

## Don't

- Don't use it to count, do arithmetic or compare dates.
- Don't gate untrusted input with it: injected instructions in `state` move the answers, and planted false facts move them far more.
- Don't pad state. Context rot is documented, and state plus the longest question are capped (Jev 32k tokens, Clef 64k; Jev also caps state plus all questions at 64k): for Jev keep item plus `--context` under ~100k characters and truncate longer ones in code.
- Don't pack several items into one `state` (a pair you judge together, as in dedupe, is one item): Jev's scores shift by position, in either direction, by up to ~0.37. Use `--each`. TypeSafe's own pattern for comparing one state against many candidates (dedupe, rerank) puts each candidate in its own question's `instructions` object instead; that pays for a large shared state once, but its position effects are unmeasured here.
- Don't trust answers for non-English or specialist domains (e.g. German accounting) without a labelled check. Write the instructions in English even when the state isn't.

## Deeper

- Jev: [docs index](https://docs.typesafe.ai/llms.txt), append `.md` to any page; failure modes on [model-jaggedness/jev-1.13](https://docs.typesafe.ai/model-jaggedness/jev-1.13.md). For building TypeSafe into an application, use the official skill [typesafe-ai/skills](https://github.com/typesafe-ai/skills/blob/main/skills/typesafe-ai/SKILL.md).
- Clef: [Cloudflare model page](https://developers.cloudflare.com/workers-ai/models/clef/), [Requesty decisions](https://docs.requesty.ai/features/decisions.md).

**Script Execution:** Always invoke scripts by absolute path: resolve `./scripts/` against this SKILL.md's directory. All scripts use Nix shebangs, so no dependency installation is needed.
