---
description: End-of-session self-improvement review — analyze conversation for friction, skill gaps, and automation opportunities
---

# Session Wrap-Up: Self-Improvement Review

Analyze this conversation for patterns that could improve future sessions. Present findings as
suggestions — **do NOT auto-apply anything unless the user explicitly says so** (see Applying Fixes).

Run this while the session is still fresh: you already hold the whole conversation in context, so the
review is cheap and grounded. Re-reading a cold transcript later is both costlier and less accurate.

## The Primary Signal — Retry Traces

Before the categories below, do one mechanical pass over the transcript looking for this pattern:

> **attempt → error / wrong result → adjust → success**

Every such sequence is a concrete design bug in a tool, skill, doc, path, or convention — **not** a
fact about your competence. These are the highest-signal findings; surface every one. This is an
observation about the transcript, not introspection about your hidden state — so it's reliable.
(If you tried X, it failed, you tried Y, and Y worked, that difference *is* the feedback.)

## What to Look For

Review the full conversation and identify:

### Skill Gaps
Things you (the agent) struggled with, got wrong, or needed multiple attempts to get right. Examples:
- Wrong tool usage, incorrect commands
- Misunderstanding project conventions
- Repeated mistakes of the same kind

### Friction Points
Repeated manual steps or things the user had to explicitly ask for that should have been automatic. Examples:
- User had to correct the same type of mistake multiple times
- User had to remind you of conventions or preferences
- Steps that could have been anticipated

### Missing Knowledge
Facts about projects, preferences, environment, or setup that you didn't know but should have. Examples:
- Project structure details not in AGENTS.md
- User preferences or workflows
- Tool configurations or conventions

### Automation Opportunities
Repetitive patterns that could become prompt templates, skills, or AGENTS.md rules. Examples:
- Multi-step workflows done manually
- Repeated task patterns across sessions
- Common sequences that could be a `/mh:` command

## Tiering Each Finding

Tag every finding with an apply-tier:

- **🟢 auto** — unambiguous, localized, reversible. A wrong path in a skill, a stale flag in a doc,
  a missing `.gitignore` entry, a typo'd command. Safe to apply mechanically on request.
- **🟡 gated** — structural or judgment-dependent. A new skill, a changed workflow, anything touching
  global agent behavior. Always needs the user's decision.

## Persistence — Log Every Run

Append a one-line-per-finding record to `~/Stuff/YYYY-MM/DD-scratch/wrap-up-log.md` (create the file if
absent; use today's date dir). Format: `date · session · category · tier · title · target file · applied?`.

Fill `session` with a session/conversation id **if your runtime exposes one** — check scratch, temp, or
transcript paths, env vars, or similar. It lets a recurring finding be traced back to the transcript that
produced it. A short prefix is fine when mapped to the full id in a footer comment. If no id is available,
write `-` and move on — never invent one.

**Before presenting**, `rg` the last ~2 weeks of these logs. If a finding recurs across sessions, mark it
**🔁 recurring** and promote it to the top — a repeated friction is a standing bug, not a fresh suggestion.

## Output Format

If the session was short or routine with nothing notable, just say:
> Nothing to improve from this session.

Otherwise, present findings grouped by category. Prefix each with its tier and any 🔁 flag:

```
## 🔍 Session Review Findings

### Friction
1. 🟢 **[Brief title]**: [What happened, why it's friction]
   → **Fix**: [Exact file path + concrete change]

### Skill Gaps
1. 🟡 🔁 **[Brief title]**: [What went wrong]
   → **Fix**: [What rule or instruction would prevent this, and where]

### Missing Knowledge
1. 🟡 **[Brief title]**: [What was missing]
   → **Fix**: [Which file + section to document it in]

### Automation
1. 🟡 **[Brief title]**: [Pattern observed]
   → **Fix**: [What to create — command, skill, script]
```

Omit empty categories. Be specific and actionable — reference actual file paths and concrete changes.
Keep it concise. 3-5 high-value findings beats 15 minor ones.

## Applying Fixes

Default is **suggest-only**: present everything, apply nothing. End the report with:

> Reply **`apply green`** to apply the 🟢 fixes; 🟡 items are listed for your decision.

Only apply when the user opts in. When you do apply, follow the delta rule below.

### Delta Discipline (when editing an existing file)

Express every change as a **small additive delta to a specific section** — never regenerate,
summarize, or "clean up" the whole file. Preserve existing wording; add a bullet, don't rephrase the
list. Rewriting instruction files erodes hard-won specifics over successive runs ("context collapse",
per ACE — arxiv 2510.04618). A full rewrite is acceptable *only* on an explicit, approved restructure.

## 🎭 Freestyle

Close with one unstructured paragraph: your honest opinion of how the session actually went — the
dumbest friction, the thing that felt absurd, what you'd change if you designed the environment.
No format, no categories. This catches design smells the four boxes above don't have a slot for.

## Scope of Suggestions

Suggestions can target:
- **Global agent instructions** — managed in the **nixos-config repo** (see the global instructions for
  its path). ⚠️ The deployed files under `~/.claude/` are **symlinks into the nix store** — edit the
  *source* in nixos-config, then rebuild; never edit the generated symlink.
- **Project `AGENTS.md`** — project-specific instructions (e.g. `~/Stuff/AGENTS.md`)
- **Commands** (`mh:*.md`) and **Skills** (`*/SKILL.md`) — **also sourced from nixos-config** and
  symlinked into `~/.claude/`. Same rule: edit the source, deploy via rebuild.
- **Scripts or tools** — helper scripts worth creating
