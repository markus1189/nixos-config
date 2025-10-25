Enable Ultrathink

Please analyze this codebase and create a CLAUDE.md file, which will be given to future instances of Claude Code to operate in this repository.

## Purpose & Philosophy

CLAUDE.md is automatically loaded into every Claude Code session. Think of it as a "README for AI agents" - a dedicated place to provide context and instructions that help AI work effectively on your project. This file should evolve with your project and be version-controlled.

**Core principles:**
- **Concise over comprehensive**: Every word consumes tokens. Keep under 2 pages when possible. Target 300-500 lines for typical projects.
- **Scannable structure**: Use clear headings, visual hierarchy, and categorization so key information can be found quickly
- **Specific over generic**: "Run `./gradlew :driver-sync:test`" not "run tests"
- **Actionable over descriptive**: What to DO, not just what to know
- **Project-specific only**: No generic best practices that apply to all codebases
- **Complete implementations**: Prohibit TODOs and mock functions in examples
  - ❌ `# TODO: Add database setup commands`
  - ✅ `docker-compose up -d postgres && npm run db:migrate`

## Required Content

### 1. Essential Development Commands

Commands a developer needs immediately:
- **Build**: Primary build command and module-specific builds (for monorepos)
- **Test**: Full test suite, single test, specific test file/class/method
- **Lint/Format**: Code quality and formatting commands
- **Setup**: Any initialization needed before development (install deps, start services)
- **Run**: How to start the application (if applicable)

Example format:
```bash
# Build all modules
./gradlew build

# Run specific test method
./gradlew :driver-sync:test --tests "*SpecificTestClass.testMethod"
```

### 2. High-Level Architecture

Information that requires reading multiple files to understand:
- **Module organization**: How modules/packages relate and depend on each other
  - For large monorepos, **categorize modules** (e.g., "Core modules", "Language-specific extensions", "Specialized/Support modules")
  - This grouping makes the structure scannable and helps orient quickly
- **Dependency flow**: Explicit dependency chains (e.g., "driver implementations → driver-core → bson")
- **Request/data flow**: How information moves through the system (e.g., "requests flow X → Y → Z")
- **Key abstractions**: Core interfaces, base classes, or patterns used throughout
- **Responsibility mapping**: Which modules handle which concerns
- **Non-obvious structure**: Generated code, multi-language support, plugin systems

**Focus on the "big picture"** - avoid listing files that are easily discovered.

### 3. Monorepo Considerations

For repositories containing multiple projects or modules:
- **Root CLAUDE.md**: Shared conventions, repository-wide commands, cross-module architecture
- **Module CLAUDE.md**: Module-specific build/test/architecture only
- Avoid duplication between root and module files
- Link between files when helpful: "See root CLAUDE.md for repository-wide conventions"

### 4. Critical Project-Specific Information

- **Environment requirements**: Specific versions, unusual dependencies, required services
- **Testing requirements**: Databases, external services, specific flags needed
- **Gotchas and quirks**: Non-standard layouts, unexpected behaviors, known issues
- **Code conventions**: Project-specific patterns (not generic style guides)
- **Repository etiquette**: Branch naming, commit message format, merge vs rebase

## Discovery Strategy

Follow this sequence for efficient exploration:

1. **Build configuration** (build.gradle, pom.xml, package.json, Cargo.toml, etc.)
   - Reveals modules, dependencies, available tasks

2. **Existing documentation** (README.md, CONTRIBUTING.md, docs/, AGENTS.md)
   - May contain critical context already written

3. **AI assistant configs** (.cursor/rules/*.mdc, .cursorrules, .github/copilot-instructions.md)
   - Team's established preferences and standards

4. **CI/CD configuration** (.github/workflows/, .gitlab-ci.yml, etc.)
   - Shows full dev/test/build/deploy pipeline

5. **Test organization** (test/, __tests__/, tests/)
   - Reveals test structure, utilities, and requirements

6. **Main entry points** (main.go, index.ts, app.py, etc.)
   - Understanding how the application bootstraps

## Keeping CLAUDE.md Current

Update when:
- **Build system or tooling changes**: New test runner, different build commands, dependency management updates
- **Major architectural refactoring**: Module restructure, new design patterns, data flow changes
- **New critical dependencies or services**: Required databases, external APIs, development services
- **Repository workflow changes**: Branch strategy updates, commit conventions, PR requirements
- **Onboarding friction**: New developers repeatedly ask the same questions

Treat CLAUDE.md as living documentation - outdated instructions are worse than no instructions.

## Quality Checklist

Before finalizing CLAUDE.md, verify:

- [ ] All commands have been tested/verified from build files
- [ ] No generic advice that applies to any codebase
- [ ] No repetition of information
- [ ] Architecture describes component relationships, not just existence
- [ ] Examples use actual project commands/paths, not placeholders
- [ ] Token-efficient: concise but actionable
- [ ] No made-up sections - only verified information
- [ ] Module/task context provided with numbered steps where helpful
- [ ] Clean markdown structure: consistent heading levels, proper lists, logical visual hierarchy
- [ ] Modules are categorized (for monorepos) to improve scannability

## What NOT to Include

❌ Generic best practices ("write tests", "handle errors", "use descriptive names")
❌ Security reminders about API keys/secrets
❌ Every file or directory (focus on non-obvious structure)
❌ Requests to refer to external documentation
❌ Style preferences better suited for linters
❌ Information easily discovered by ls/tree commands
❌ TODO comments or incomplete examples

## File Format

**Required prefix:**
```markdown
# Agent Documentation

This file provides guidance to coding agents when working with code in this repository.
```

**Recommended structure:**
```markdown
# Agent Documentation

This file provides guidance to coding agents when working with code in this repository.

## Project Structure
[High-level architecture overview]

For monorepos with many modules, categorize them for scannability:
**Core modules:**
- `module-a` - Brief description
- `module-b` - Brief description

**Language-specific extensions:**
- `module-c` - Brief description

**Supporting/specialized modules:**
- `module-d` - Brief description

**Dependency flow:** [Show explicit chains, e.g., "drivers → core → base"]

## Development Requirements
[Critical setup info: Java version, required services, submodules, etc.]

## Building and Testing

### Full Build
[Commands for building entire project]

### Module-Specific Builds
[Commands for individual modules in monorepo]

### Running Tests
[Test commands: full suite, specific class, specific method, patterns]
[Test organization: where tests live, frameworks used, tags/categories]

### Code Quality
[Lint, format, static analysis commands]

## Common Build Issues
[Known gotchas with concrete solutions - highly valuable for autonomous problem-solving]

## Architecture Overview
[Component relationships, key flows, implementation details for common modification patterns]

## [Other Project-Specific Sections]
[E.g., "Testing Requirements", "Publishing and Releases", "Repository Workflow"]

## Code Conventions
[Project-specific patterns, API stability annotations, error handling, etc.]
```

## For Existing CLAUDE.md Files

When a CLAUDE.md already exists:

1. **Audit for generic content**: Remove anything that applies to all projects
2. **Verify commands**: Check they match current build configuration
3. **Add missing essentials**: Development commands, architecture, quirks
4. **Remove redundancy**: Consolidate repeated information
5. **Improve scannability**:
   - Categorize modules logically (Core/Language-specific/Specialized)
   - Use consistent heading hierarchy
   - Add visual structure with clear sections and lists
6. **Token optimization**: Can it be more concise while staying actionable?

Suggest specific additions, removals, or restructuring with examples.

## Output Format

**Always write the complete CLAUDE.md file** - whether creating new or updating existing.

For existing files:
1. Read the current CLAUDE.md
2. Audit it against the guidelines above
3. Write the improved version with all necessary updates applied
4. Briefly summarize what changed (additions, removals, updates)

Do not just suggest improvements - apply them directly by writing the updated file.
