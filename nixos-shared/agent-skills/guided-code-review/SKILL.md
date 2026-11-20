---
name: guided-code-review
description: "Start a guided code review"
context: fork
disable-model-invocation: true
effort: xhigh
---

# Guided Code Review

Interactive code review process where the agent walks through changes in logical chunks, the reviewer steers with feedback, and findings accumulate into a structured summary.

## Process

### Phase 1: Determine Scope

1. Identify commits to review:
   - `git log --oneline --grep 'TICKET-ID'` or diff against a branch
   - `git diff --stat base...HEAD` for a file-level overview of the whole range (net change, not per commit)
2. If the user provides a story/ticket description, ingest it:
   - Extract acceptance criteria
   - Note if it's a subtask, not all ACs may apply to this changeset
   - If no story provided, proceed without and review on code merit alone

### Phase 2: Build Mental Model

1. From story ACs and/or commit messages, predict what *should* have changed:
   - Which layers (domain, logic, controllers, tests)?
   - Which API endpoints affected?
   - What error handling, metrics, docs expected?
2. Identify external artifacts worth cross-checking:
   - OpenAPI specs, event schemas, DB migrations, etc
   - Fetch them if accessible, otherwise state as missing with the potential value
3. Present the mental model briefly to the reviewer

### Phase 3: Map the Changes

Present a proposed **chunk structure** before diving in:

- Group changed files into logical chunks (not alphabetical, not per-commit)
- Suggested default ordering you can deviate from: **domain types → core logic → controllers/API surface → external contracts (http/events/db) → plumbing → tests**
- Plumbing (pure parameter threading with zero business logic) always comes after the code it connects
- Flag chunks that may need splitting (e.g., two unrelated endpoints both touched)

Present the map and **wait for the reviewer to approve, reorder,
merge, or skip areas** before proceeding, ask via the question tool.

### Phase 4: Chunk-by-Chunk Walkthrough

For each chunk in the agreed map:

1. **Read the relevant files** at the current revision
2. **Present the chunk**: what changed, why (inferred from context), and how it connects to the story/goal
3. **Assess** against review dimensions
4. **Wait for reviewer feedback** via the question tool before proceeding to the next chunk

#### Reviewer Steering Commands

- **"note X for later"** / **"note that"** → accumulate the finding, tag by dimension, carry to final summary
- **"next"** / **"continue"** → advance to next chunk
- **"skip"** → skip current chunk entirely
- **"which of those are not just plumbing?"** → identify and separate boilerplate from logic
- **Direct observations** (e.g., "I'm missing integration tests for X") → acknowledge, accumulate, continue

#### Chunk Splitting

Split a chunk further when:

- It's too large to present coherently in one message
- It contains independent concerns (e.g., two unrelated endpoints)
- The reviewer asks to zoom in on a subset

### Phase 5: Final Summary

After all chunks are reviewed, produce a structured summary:

```markdown
# Code Review Summary: [TICKET-ID] [Short Description]

## ✅ What Was Implemented
[Brief description of what the changeset achieves, what has been verified]

## ❌ Gaps vs. Story Requirements
[Only if story was provided and has applicable ACs]
[Each gap with: what's missing, impact, which AC it relates to]

## ⚠️ Robustness Concerns
[Edge cases, silent failures, error handling gaps, etc]

## 🏗️ Design Concerns
[Coupling, proliferation, abstraction issues]

## 📝 Code Quality
[Naming, readability, style issues (with concrete suggestions), unclear parts]

## 🧪 Test Gaps
[Missing tests/types, untested edge cases, convention violations]
[Include concrete examples of what tests should look like]

## 📊 Observability Gaps
[Missing or incomplete: metrics, logging, alerting, ...]

## 📄 API or Documentation Gaps
[Missing spec updates, undocumented error codes, stale docs, ...]
```

Omit empty sections. If story was a subtask, note which ACs this changeset covers vs. which are deferred.

#### TODOs / Follow-ups

If any findings warrant action, end with a concrete list:

```markdown
## 📋 TODOs / Follow-ups
- [ ] [Actionable item with enough context to act on]
- [ ] ...
```

Only include if there are actual action items. Each item should be self-contained (no "see above").

## Key Rules

- **Never dump everything at once:** always chunk and wait for the reviewer
- **The reviewer knows the codebase:** don't explain conventions, the reviewer will point out violations
- **Accumulate all noted items:** every "note X" must appear in the final summary
- **Map findings to story ACs:** when a story is available
- **Don't penalize subtasks:** for unaddressed ACs just note scope
