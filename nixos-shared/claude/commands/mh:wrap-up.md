---
description: End-of-session self-improvement review — analyze conversation for friction, skill gaps, and automation opportunities
---

# Session Wrap-Up: Self-Improvement Review

Analyze this conversation for patterns that could improve future sessions. Present findings as suggestions — do NOT auto-apply anything.

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

## Output Format

If the session was short or routine with nothing notable, just say:
> Nothing to improve from this session.

Otherwise, present findings grouped by category:

```
## 🔍 Session Review Findings

### Friction
1. **[Brief title]**: [What happened, why it's friction]
   → **Suggestion**: [Where to fix it and how — AGENTS.md rule, prompt template, skill, etc.]

### Skill Gaps
1. **[Brief title]**: [What went wrong]
   → **Suggestion**: [What rule or instruction would prevent this]

### Missing Knowledge
1. **[Brief title]**: [What was missing]
   → **Suggestion**: [Where to document it — which AGENTS.md, which section]

### Automation
1. **[Brief title]**: [Pattern observed]
   → **Suggestion**: [What to create — prompt template, skill, script]
```

Omit empty categories. Be specific and actionable — reference actual file paths and concrete changes.

## Scope of Suggestions

Suggestions can target:
- **`~/.pi/agent/AGENTS.md`** — Global agent instructions
- **Project `AGENTS.md`** — Project-specific instructions
- **`~/.pi/agent/prompts/mh:*.md`** — New prompt templates
- **`~/.claude/skills/*/SKILL.md`** — New or updated skills
- **Scripts or tools** — Helper scripts worth creating

Keep it concise. 3-5 high-value findings beats 15 minor ones.
