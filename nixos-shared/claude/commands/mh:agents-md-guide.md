# AGENTS.md / CLAUDE.md Best Practices Guide

> **Purpose**: This document provides evidence-based guidelines for writing, reviewing, and improving AGENTS.md (or CLAUDE.md) files. Use this as a reference when creating or evaluating agent instruction files.

---

## Sources & Authority

| Source | Type | Key Contribution |
|--------|------|------------------|
| [Anthropic Engineering Blog](https://www.anthropic.com/engineering/claude-code-best-practices) | Official | CLAUDE.md structure, tuning, iteration |
| [AGENTS.md Specification](https://agents.md) | Open Standard | Cross-tool compatibility, 60k+ repos |
| [OpenAI Codex Prompting Guide](https://developers.openai.com/cookbook/examples/gpt-5/codex_prompting_guide) | Official | Autonomy, tool use, boundaries |
| [GitHub Blog - 2,500+ repos analysis](https://github.blog/ai-and-ml/github-copilot/how-to-write-a-great-agents-md-lessons-from-over-2500-repositories/) | Empirical | What works in practice |
| [GitHub Copilot Docs](https://docs.github.com/en/copilot/how-tos/configure-custom-instructions) | Official | Repository instructions format |

---

## Six Core Areas (Required Coverage)

An effective AGENTS.md must cover these six areas. Files missing multiple areas are incomplete.

### 1. Commands
```markdown
## Commands
- Build: `pnpm build`
- Test: `pnpm test`
- Lint: `pnpm lint --fix`
- Typecheck: `pnpm typecheck`
- Dev server: `pnpm dev`

Always run `pnpm install` after pulling changes.
```

**Rules:**
- Include flags and options, not just tool names
- Commands must be tested and verified to work
- Use "always" language for prerequisites
- Document command order dependencies

### 2. Testing Instructions
```markdown
## Testing
- Run all tests: `pnpm test`
- Run single test: `pnpm test -- path/to/test.spec.ts`
- Run with coverage: `pnpm test --coverage`

Tests must pass before commits. Add tests for new functionality.
```

**Rules:**
- Explain how to run specific tests
- Document test file location patterns
- State testing requirements for PRs

### 3. Project Structure
```markdown
## Project Structure
- `src/` - Application source code
- `src/components/` - React components
- `src/hooks/` - Custom React hooks
- `src/utils/` - Utility functions
- `tests/` - Test files (mirrors src/ structure)
- `docs/` - Documentation
- `.github/workflows/` - CI/CD pipelines
```

**Rules:**
- Document where different types of code live
- Explain any non-obvious directory purposes
- Note configuration file locations

### 4. Code Style (with Examples)
```markdown
## Code Style
- Use ES modules (import/export), not CommonJS (require)
- Destructure imports: `import { useState } from 'react'`
- Prefer functional components with hooks
- Use TypeScript strict mode

### Naming Conventions
- Functions: camelCase (`getUserData`, `calculateTotal`)
- Components: PascalCase (`UserCard`, `DataTable`)
- Constants: UPPER_SNAKE_CASE (`API_URL`, `MAX_RETRIES`)

### Example
```typescript
// ✅ Good - descriptive names, proper error handling
export async function fetchUserById(id: string): Promise<User> {
  if (!id) throw new Error('User ID required');
  const response = await api.get(`/users/${id}`);
  return response.data;
}

// ❌ Bad - vague names, no validation, no types
async function get(x) {
  return await api.get('/users/' + x).data;
}
```

**Rules:**
- Show concrete code examples (good vs bad)
- One real snippet beats three paragraphs
- Document naming conventions explicitly
- Reference existing patterns in codebase

### 5. Git Workflow
```markdown
## Git Workflow
- Branch naming: `feature/description` or `fix/description`
- Commit style: Conventional commits (`feat:`, `fix:`, `docs:`, `chore:`)
- Always rebase on main before creating PR
- Squash commits when merging
```

**Rules:**
- Document branch naming conventions
- Specify commit message format
- Explain PR process and requirements

### 6. Boundaries
```markdown
## Boundaries
- ✅ **Always do:** Run typecheck after changes, follow existing patterns, add tests
- ⚠️ **Ask first:** Adding dependencies, modifying CI config, database schema changes
- 🚫 **Never do:** Commit secrets/API keys, edit `node_modules/`, modify `.env.production`, remove failing tests without fixing
```

**Rules:**
- Use three-tier system: Always / Ask First / Never
- "Never commit secrets" is the most common helpful constraint
- Be specific about protected files and directories
- Document destructive operations to avoid

---

## Tech Stack Specification

**Bad (too vague):**
```markdown
This is a React project.
```

**Good (specific with versions):**
```markdown
## Tech Stack
- **Runtime:** Node.js 20.x
- **Framework:** React 18.2 with TypeScript 5.4
- **Bundler:** Vite 5.x
- **Styling:** Tailwind CSS 3.x
- **Package Manager:** pnpm 9.x
- **Testing:** Vitest + React Testing Library
```

---

## What Works (Evidence-Based Patterns)

### 1. Emphasis for Critical Instructions
Use emphasis markers to improve adherence:
- "IMPORTANT: ..."
- "YOU MUST ..."
- "NEVER ..."
- "ALWAYS ..."

### 2. Nested Files for Monorepos
```
root/
├── AGENTS.md              # General project instructions
├── packages/
│   ├── frontend/
│   │   └── AGENTS.md      # Frontend-specific
│   └── backend/
│       └── AGENTS.md      # Backend-specific
```
The closest AGENTS.md to the edited file takes precedence.

### 3. Concise and Iterable
- Keep under 2 pages
- No rigid format required
- Iterate based on agent mistakes
- Human-readable natural language

### 4. Validated Commands
Every command listed should be:
- Tested by running it
- Documented with prerequisites
- Noted with expected runtime if long

---

## What to Avoid (Anti-Patterns)

### 1. Vague Personas
```markdown
# ❌ Bad
You are a helpful coding assistant.

# ✅ Good
You are a test engineer who writes tests for React components using Vitest and React Testing Library, follows the examples below, and never modifies source code in `src/`.
```

### 2. Task-Specific Instructions
```markdown
# ❌ Bad - too specific to one task
Fix the login bug in auth.ts by checking the token expiry.

# ✅ Good - project-general guidance
Authentication code lives in `src/auth/`. All auth changes require unit tests and must not break existing session handling.
```

### 3. Contradictory Rules
Ensure rules don't conflict. Review for consistency.

### 4. Missing Validation Steps
```markdown
# ❌ Bad - commands without verification
Run `npm run build` to build.

# ✅ Good - tested with notes
Run `npm run build` to build (outputs to `dist/`, ~30s).
If you see "heap out of memory", run with `NODE_OPTIONS=--max-old-space-size=4096`.
```

### 5. Secrets in File
**NEVER** include in AGENTS.md:
- API keys
- Passwords
- Tokens
- Production credentials
- Private URLs

### 6. Excessive Length
Don't dump entire documentation. This is a prompt—treat it like one.

### 7. Format Inconsistency
```markdown
# ❌ Bad - mixed styles
- Item one
* Item two
>> Important note

# ✅ Good - consistent
- Item one
- Item two
- **Important:** Note here
```

### 8. Overloaded Sections
Keep each section focused on its purpose. Don't put skills in Rules, or commands in Project Structure.

---

## Review Checklist

When reviewing an AGENTS.md file, verify:

### Content Coverage
- [ ] Commands section with specific flags/options
- [ ] Tech stack with version numbers
- [ ] Project structure documented
- [ ] Code style with concrete examples
- [ ] Git workflow (branches, commits, PRs)
- [ ] Boundaries (Always/Ask/Never tiers)

### Quality Checks
- [ ] Commands have been tested and work
- [ ] No secrets, API keys, or credentials
- [ ] Under 2 pages / reasonably concise
- [ ] Consistent formatting throughout
- [ ] Task-agnostic (not specific to one task)
- [ ] Examples show good AND bad patterns
- [ ] Critical instructions use emphasis (IMPORTANT, NEVER, etc.)
- [ ] No contradictory rules

### For Monorepos
- [ ] Root AGENTS.md covers general guidance
- [ ] Subdirectory AGENTS.md files for specific packages
- [ ] No duplication between levels

---

## Template

```markdown
# [Project Name]

## Overview
[One paragraph: what this project does, primary purpose]

**Tech Stack:** [Language] [Version], [Framework] [Version], [Key Tools]

## Commands
- Install: `[package manager] install`
- Build: `[command]`
- Test: `[command]`
- Lint: `[command]`
- Dev: `[command]`
- Typecheck: `[command]`

[Prerequisites or order dependencies]

## Project Structure
- `src/` - [description]
- `tests/` - [description]
- `docs/` - [description]
- [other key directories]

## Code Style
[Key conventions - imports, naming, patterns]

### Naming Conventions
- Functions: [convention]
- Components/Classes: [convention]
- Constants: [convention]

### Example
```[language]
// ✅ Good
[good example]

// ❌ Bad
[bad example]
```

## Testing
- Run all: `[command]`
- Run single: `[command]`
- [Coverage, requirements]

## Git Workflow
- Branches: `[pattern]`
- Commits: `[format]`
- [PR requirements]

## Boundaries
- ✅ **Always:** [required actions]
- ⚠️ **Ask first:** [need confirmation]
- 🚫 **Never:** [forbidden actions]
```

---

## Improvement Process

1. **Start minimal** - Cover the six core areas briefly
2. **Test with agent** - Use the file in actual agent sessions
3. **Note failures** - When agent makes mistakes, identify what guidance was missing
4. **Add specific guidance** - Address the failure with concrete instructions
5. **Iterate** - The best files grow through use, not upfront planning

---

## File Naming & Location

| Tool | Primary File | Alternatives |
|------|--------------|--------------|
| Claude Code | `CLAUDE.md` (root) | `~/.claude/CLAUDE.md` (global) |
| OpenAI Codex | `AGENTS.md` (root) | `.github/AGENTS.md` |
| GitHub Copilot | `.github/copilot-instructions.md` | `AGENTS.md` |
| Cursor | `.cursorrules` | `AGENTS.md` |
| Universal | `AGENTS.md` | Works across most tools |

**Recommendation:** Use `AGENTS.md` for maximum cross-tool compatibility, with `CLAUDE.md` as a symlink if needed.

---

## Quick Reference: Effective Patterns

| Pattern | Example |
|---------|---------|
| Specific commands | `pnpm test -- --watch --coverage` not `run tests` |
| Versioned stack | `React 18.2, TypeScript 5.4` not `React, TypeScript` |
| Three-tier boundaries | Always ✅ / Ask ⚠️ / Never 🚫 |
| Code examples | Show good AND bad with comments |
| Emphasis | `IMPORTANT:`, `NEVER`, `ALWAYS` |
| Tested commands | Verified to work, with prerequisites noted |
| Concise | Under 2 pages, natural language |

---

*This guide synthesizes official documentation from Anthropic, OpenAI, GitHub, and empirical analysis of 60,000+ repositories. Apply these patterns to create AGENTS.md files that meaningfully improve agent performance.*
