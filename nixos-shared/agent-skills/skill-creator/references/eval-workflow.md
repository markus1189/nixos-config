# Eval Workflow

Full evaluation and benchmarking pipeline for skills. Use this reference when running test cases, grading outputs, aggregating benchmarks, or optimizing skill descriptions.

## Table of Contents

1. [Overview](#overview)
2. [When to Use Evals](#when-to-use-evals)
3. [Communicating with the User](#communicating-with-the-user)
4. [Running Test Cases](#running-test-cases)
5. [Drafting Assertions](#drafting-assertions)
6. [Capturing Timing Data](#capturing-timing-data)
7. [Grading and Aggregation](#grading-and-aggregation)
8. [Launching the Viewer](#launching-the-viewer)
9. [Reading Feedback](#reading-feedback)
10. [Improvement Philosophy](#improvement-philosophy)
11. [Blind Comparison](#blind-comparison)
12. [Description Optimization](#description-optimization)
13. [Platform Notes](#platform-notes)

---

## Overview

The eval loop: **draft → test → review → improve → repeat**.

1. Write or update the skill
2. Create test prompts (2-3 realistic user queries)
3. Spawn with-skill + baseline runs in parallel via subagents
4. While runs execute, draft assertions
5. Grade outputs, aggregate into benchmark, launch viewer
6. User reviews outputs and leaves feedback
7. Improve skill based on feedback, repeat

## When to Use Evals

**Use evals** for skills with objectively verifiable outputs: file transforms, data extraction, code generation, fixed workflow steps. These benefit from quantitative assertions and benchmark comparisons.

**Skip evals** for skills with subjective outputs: writing style, creative work, design. These are better evaluated qualitatively through direct user feedback.

Suggest the appropriate default based on the skill type, but let the user decide.

## Communicating with the User

Pay attention to context cues for the user's skill level. The skill creator is used by people across a wide range of familiarity with coding jargon.

- "evaluation" and "benchmark" are borderline but OK for most users
- For "JSON" and "assertion", look for cues that the user knows these terms before using them without a brief explanation
- When in doubt, briefly explain terms with a short inline definition

## Running Test Cases

### Workspace Structure

Put results in `<skill-name>-workspace/` as a sibling to the skill directory:

```
<skill-name>-workspace/
└── iteration-N/
    └── eval-<ID>/
        ├── eval_metadata.json
        ├── with_skill/
        │   └── run-1/
        │       ├── outputs/
        │       ├── grading.json
        │       └── timing.json
        └── without_skill/
            └── run-1/
                ├── outputs/
                └── grading.json
```

### Spawning Runs

For each test case, spawn **two subagents in the same turn** — one with the skill, one without. Launch everything at once so it all finishes around the same time.

**With-skill run:**
```
Execute this task:
- Skill path: <path-to-skill>
- Task: <eval prompt>
- Input files: <eval files if any, or "none">
- Save outputs to: <workspace>/iteration-N/eval-ID/with_skill/outputs/
- Outputs to save: <what the user cares about>
```

**Baseline run** (depends on context):
- **New skill**: no skill at all — same prompt, save to `without_skill/outputs/`
- **Improving existing skill**: snapshot old version first (`cp -r <skill-path> <workspace>/skill-snapshot/`), point baseline at snapshot, save to `old_skill/outputs/`

### eval_metadata.json

Write one per eval directory. Give each eval a descriptive name.

```json
{
  "eval_id": 0,
  "eval_name": "descriptive-name-here",
  "prompt": "The user's task prompt",
  "assertions": []
}
```

## Drafting Assertions

While runs are in progress, draft quantitative assertions for each test case. Don't wait — use this time productively.

Good assertions:
- Are objectively verifiable
- Have descriptive names that read clearly in the benchmark viewer
- Test meaningful outcomes (not just surface compliance)

Bad assertions:
- Check things that are trivially satisfied
- Force quantitative measurement onto subjective qualities
- Are so specific they only apply to one test case

Update `eval_metadata.json` and `evals/evals.json` with assertions once drafted. See [schemas.md](schemas.md) for the full evals.json schema.

## Capturing Timing Data

When each subagent task completes, the notification contains `total_tokens` and `duration_ms`. Save immediately — this data is not persisted elsewhere.

```json
{
  "total_tokens": 84852,
  "duration_ms": 23332,
  "total_duration_seconds": 23.3
}
```

Save to `timing.json` in each run directory. Process notifications as they arrive.

## Grading and Aggregation

### Grading

Once all runs complete:

1. **Grade each run** — spawn a grader subagent (or grade inline) using [agents/grader.md](../agents/grader.md). The grader evaluates each assertion against outputs and saves `grading.json` in each run directory.

   The `expectations` array must use fields `text`, `passed`, and `evidence` — the viewer depends on these exact names.

   For assertions that can be checked programmatically, write and run a script rather than eyeballing it.

2. **Aggregate into benchmark** — run from the skill-creator directory:
   ```bash
   ./scripts/aggregate_benchmark.py <workspace>/iteration-N --skill-name <name>
   ```
   Produces `benchmark.json` and `benchmark.md` with pass_rate, time, and tokens per configuration (mean +/- stddev and delta). See [schemas.md](schemas.md) for the exact schema the viewer expects.

3. **Analyst pass** — read benchmark data and surface patterns the aggregate stats might hide. See [agents/analyzer.md](../agents/analyzer.md) (the "Analyzing Benchmark Results" section) for what to look for: non-discriminating assertions, high-variance evals, time/token tradeoffs.

## Launching the Viewer

Launch with both qualitative outputs and quantitative data:

```bash
nohup python <skill-creator-path>/eval-viewer/generate_review.py \
  <workspace>/iteration-N \
  --skill-name "my-skill" \
  --benchmark <workspace>/iteration-N/benchmark.json \
  > /dev/null 2>&1 &
VIEWER_PID=$!
```

For iteration 2+, pass `--previous-workspace <workspace>/iteration-<N-1>` to show previous outputs and feedback as context.

**Cowork / headless environments:** Use `--static <output_path>` to write a standalone HTML file instead of starting a server. Feedback downloads as `feedback.json` when the user clicks "Submit All Reviews" — copy it into the workspace directory afterward.

Always use `generate_review.py` — don't write custom HTML.

### What the User Sees

**Outputs tab**: One test case at a time with prompt, rendered outputs, previous outputs (iteration 2+), formal grades (if graded), and a feedback textbox that auto-saves.

**Benchmark tab**: Stats summary — pass rates, timing, token usage per configuration, per-eval breakdowns, analyst observations.

Navigation via prev/next buttons or arrow keys. "Submit All Reviews" saves all feedback to `feedback.json`.

## Reading Feedback

When the user is done reviewing, read `feedback.json`:

```json
{
  "reviews": [
    {"run_id": "eval-0-with_skill", "feedback": "the chart is missing axis labels", "timestamp": "..."},
    {"run_id": "eval-1-with_skill", "feedback": "", "timestamp": "..."}
  ],
  "status": "complete"
}
```

Empty feedback = user thought it was fine. Focus improvements on test cases with specific complaints.

Kill the viewer when done: `kill $VIEWER_PID 2>/dev/null`

## Improvement Philosophy

1. **Generalize from feedback, don't overfit.** The skill will be used across many prompts — if it only works for the test cases, it's useless. Rather than fiddly overfit changes or oppressively constrictive MUSTs, try different metaphors or patterns. It's cheap to try.

2. **Keep the prompt lean.** Remove things that aren't pulling their weight. Read the transcripts, not just final outputs — if the skill makes the model waste time on unproductive work, cut those parts.

3. **Explain the why.** LLMs have good theory of mind. Rather than ALWAYS/NEVER in all caps, explain the reasoning so the model understands why something matters. That's more effective than rigid structures.

4. **Look for repeated work across test cases.** If all test runs independently wrote similar helper scripts, bundle that script in `scripts/`. This saves every future invocation from reinventing the wheel.

### Iteration Loop

After improving the skill:
1. Apply improvements
2. Rerun all test cases into `iteration-<N+1>/`, including baseline runs
3. Launch the viewer with `--previous-workspace` pointing at the previous iteration
4. Wait for user review
5. Read feedback, improve again, repeat

Keep going until the user is happy, feedback is all empty, or you're not making meaningful progress.

## Blind Comparison

For rigorous A/B comparison between two skill versions:

1. Run both versions on the same test cases
2. Give outputs to the blind comparator ([agents/comparator.md](../agents/comparator.md)) — it judges quality without knowing which skill produced which output
3. Run the post-hoc analyzer ([agents/analyzer.md](../agents/analyzer.md)) to understand why the winner won and generate improvement suggestions

This is optional and most users won't need it. The human review loop is usually sufficient.

## Description Optimization

Automated optimization of the `description` field for triggering accuracy. Do this after the skill is functionally complete.

### Step 1: Generate Trigger Eval Queries

Create ~20 eval queries — a mix of should-trigger and should-not-trigger. Save as JSON:

```json
[
  {"query": "the user prompt", "should_trigger": true},
  {"query": "another prompt", "should_trigger": false}
]
```

Queries must be realistic — concrete, specific, with detail (file paths, personal context, column names, URLs). Mix lengths and focus on edge cases.

**Good**: `"ok so my boss just sent me this xlsx file (its in my downloads, called something like 'Q4 sales final FINAL v2.xlsx') and she wants me to add a column..."`

**Bad**: `"Format this data"`, `"Extract text from PDF"`

For **should-trigger** (8-10): different phrasings, uncommon use cases, cases where this skill competes with another but should win.

For **should-not-trigger** (8-10): near-misses that share keywords but need something different. Don't make them obviously irrelevant.

### Step 2: Review with User

Present the eval set using the HTML template at `assets/eval_review.html`:

1. Read the template
2. Replace placeholders: `__EVAL_DATA_PLACEHOLDER__` (JSON array, no quotes), `__SKILL_NAME_PLACEHOLDER__`, `__SKILL_DESCRIPTION_PLACEHOLDER__`
3. Write to temp file and open in browser
4. User edits queries, toggles should-trigger, then clicks "Export Eval Set"
5. Read the downloaded `eval_set.json` from `~/Downloads/`

### Step 3: Run the Optimization Loop

```bash
./scripts/run_loop.py \
  --eval-set <path-to-trigger-eval.json> \
  --skill-path <path-to-skill> \
  --model <model-id-powering-this-session> \
  --max-iterations 5 \
  --verbose
```

This handles the full loop automatically: splits 60/40 train/test, evaluates (3 runs per query), calls Claude with extended thinking to propose improvements, re-evaluates, iterates up to 5 times. Opens an HTML report showing per-iteration results. Returns JSON with `best_description` selected by test score.

### How Skill Triggering Works

Skills appear in Claude's `available_skills` list with name + description. Claude only consults skills for tasks it can't easily handle alone — simple one-step queries may not trigger even with a perfect description match. Complex, multi-step, or specialized queries reliably trigger when the description matches.

Eval queries should be substantive enough that Claude would benefit from consulting a skill. Simple queries like "read file X" are poor test cases.

### Step 4: Apply the Result

Take `best_description` from the output and update the skill's SKILL.md frontmatter. Show the user before/after and report scores.

## Platform Notes

### Claude.ai

No subagents available. Adapt the workflow:
- Run test cases yourself one at a time (less rigorous but a useful sanity check)
- Skip baseline runs
- Present results directly in conversation instead of using the browser viewer
- Skip quantitative benchmarking
- Skip description optimization (requires `claude` CLI)

### Cowork

Subagents work but some differences:
- Use `--static <output_path>` for the eval viewer (no browser/display)
- "Submit All Reviews" downloads `feedback.json` as a file
- Description optimization works (uses `claude -p` via subprocess)
- If subagent timeouts are severe, run test prompts in series
