# Claude Code Configuration

This directory contains Claude Code configuration that is automatically symlinked to `~/.claude/` via NixOS home-manager, making it version-controlled and shareable across machines.

## Directory Structure

```
nixos-shared/claude/
├── agents/              # Custom subagents
├── commands/            # Custom slash commands
├── docs/                # User documentation
├── output-styles/       # Custom output styles
├── skills/              # Custom skills (auto-invoked capabilities)
├── sounds/              # Notification sounds for hooks
├── claude-code-statusline.sh  # Statusline script
├── CLAUDE-global.md     # Global instructions (symlinked to ~/.claude/CLAUDE.md)
└── CLAUDE.md           # This file (project-specific docs)
```

## Auto-Symlinking

The home-manager configuration in `nixos-shared/home-manager/claude-code/default.nix` automatically discovers and symlinks all markdown files from this directory to `~/.claude/`:

- `agents/*.md` → `~/.claude/agents/`
- `commands/*.md` → `~/.claude/commands/`
- `docs/*.md` → `~/.claude/user-docs/`
- `output-styles/*.md` → `~/.claude/output-styles/`
- `skills/*.md` → `~/.claude/skills/`
- `CLAUDE-global.md` → `~/.claude/CLAUDE.md`

**No manual symlinking needed** - just add markdown files to the appropriate directory and rebuild home-manager:

```bash
home-manager switch
```

---

## Custom Agents

### What Are Agents?

Agents are specialized AI assistants that Claude Code can delegate tasks to. Each operates with its own context window and can have specific tools and expertise.

### When to Use Agents

- **Specialized expertise**: Research, code review, testing strategies
- **Isolation needed**: Tasks requiring fresh context
- **Repeated workflows**: Standardize common delegation patterns

### File Format

Agents require YAML frontmatter with these fields:

```markdown
---
name: agent-name
description: When and why to invoke this agent
tools: Bash, Read, Write, Grep, Glob  # Optional, inherits all if omitted
model: sonnet  # Optional: sonnet, opus, haiku, or inherit
---

You are a specialized agent for [purpose].

## Your Mission

[Define the agent's role and approach...]

## Process

1. [Step one]
2. [Step two]
...

## Output Format

[Specify what the agent should return...]
```

### Adding a New Agent

1. **Create the file** in `nixos-shared/claude/agents/`:
   ```bash
   # Example: code-reviewer.md
   ```

2. **Define frontmatter and system prompt**:
   - **name**: Unique identifier (lowercase-with-hyphens)
   - **description**: Critical - describes when Claude should invoke this agent
   - **tools**: Optional list of allowed tools (comma-separated)
   - **System prompt**: Clear instructions for the agent's role and process

3. **Apply configuration**:
   ```bash
   home-manager switch
   ```

4. **Test the agent**:
   - Use the Task tool to invoke manually
   - Or let Claude invoke automatically when tasks match the description

### Example Structure

See `agents/command-explorer.md` for a complete example with:
- Detailed mission statement
- Step-by-step research process
- Structured output format
- Safety guidelines

---

## Output Styles

### What Are Output Styles?

Output styles modify Claude's system prompt to adapt its behavior beyond software engineering. They enable different interaction modes while preserving core capabilities.

### When to Use Them

- **Learning mode**: Educational approach with explanations
- **Rubber duck debugging**: Guide through questions rather than direct answers
- **Domain-specific**: Adapt Claude for writing, analysis, or other domains

### File Format

Output styles use this structure:

```markdown
---
description: Brief description shown when selecting this style
---

# Style Name

[Custom system prompt instructions...]

## Behavior Guidelines

[Specific interaction patterns...]

## Response Structure

[How Claude should structure responses...]
```

### Adding a New Output Style

1. **Create the file** in `nixos-shared/claude/output-styles/`:
   ```bash
   # Example: rubber-duck.md
   ```

2. **Define the style**:
   - Write frontmatter with `description` field
   - Define custom system prompt instructions
   - Specify interaction patterns and tone

3. **Apply configuration**:
   ```bash
   home-manager switch
   ```

4. **Activate the style**:
   ```bash
   /output-style select rubber-duck
   ```

### Example Structure

See `output-styles/rubber-duck.md` for a complete example with:
- Clear behavioral guidelines
- Question strategies for different scenarios
- Response structure patterns
- Tone and approach specifications

---

## Slash Commands

### What Are Slash Commands?

Slash commands are user-invoked shortcuts for frequently-used prompts. They're stored as markdown files and support arguments.

### When to Use Them

- **Frequent workflows**: Git commits, documentation updates
- **Standardized prompts**: Code reviews, bug analysis
- **Team coordination**: Share common workflows in project `.claude/commands/`

### File Format

Commands are markdown files with optional frontmatter:

```markdown
---
description: What this command does (shown in autocomplete)
allowed-tools: Bash(git add:*), Bash(git status:*)  # Optional
argument-hint: [optional-arg]  # Optional
model: sonnet  # Optional
---

Your prompt instructions here.

Use $ARGUMENTS for all args or $1, $2, etc. for specific positions.
```

### Adding a New Slash Command

1. **Create the file** in `nixos-shared/claude/commands/`:
   ```bash
   # Example: mh:commit.md creates /mh:commit command
   # Colons in filename create namespaced commands
   ```

2. **Write the prompt**:
   - Add frontmatter if you need constraints
   - Write clear instructions
   - Use `$ARGUMENTS` or `$1`, `$2` for parameters

3. **Apply configuration**:
   ```bash
   home-manager switch
   ```

4. **Use the command**:
   ```bash
   /mh:commit fix authentication bug
   # $ARGUMENTS will be "fix authentication bug"
   ```

### Special Features

- **File references**: Use `@filename` to include file contents
- **Bash execution**: Prefix with `!` to run shell commands directly
- **Namespacing**: Use subdirectories or colons in filename for organization

### Example Commands

See existing commands in `commands/`:
- `mh:elfeed-search.md` - Search elfeed database
- `mh:update-claude-code.md` - Update Claude Code package
- `mh:prime-context.md` - Analyze current working state

---

## Custom Skills

### What Are Skills?

Skills are **model-invoked** capabilities that Claude autonomously uses when relevant. Unlike slash commands (user-invoked), Claude decides when to apply skills based on their descriptions.

### When to Use Skills

- **Extend capabilities**: Add domain-specific expertise
- **Standardize workflows**: Ensure consistent approaches across team
- **Reduce repetition**: Avoid re-explaining common patterns
- **Composable tasks**: Skills can work together for complex operations

### File Format

Skills require YAML frontmatter and follow this structure:

```markdown
---
name: Skill Name (using gerund form, e.g., "Processing PDFs")
description: What the skill does AND when to use it (max 1024 chars, third person)
allowed-tools: Read, Write, Bash  # Optional, restricts tool access
---

# Overview

[Brief explanation of the skill's purpose]

## When to Use This Skill

[Specific triggers and scenarios]

## Workflow

1. [Step one]
2. [Step two]
...

## Best Practices

[Guidelines for effective use]

## Examples

[Concrete examples demonstrating usage]
```

### Adding a New Skill

1. **Create directory** in `nixos-shared/claude/skills/`:
   ```bash
   mkdir -p nixos-shared/claude/skills/my-skill
   ```

2. **Create SKILL.md** (required):
   - **Name**: Gerund form (e.g., "Analyzing logs", "Processing images")
   - **Description**: CRITICAL - must specify what it does AND when to invoke
   - **Body**: Keep under 500 lines; use progressive disclosure

3. **Add supporting files** (optional):
   ```bash
   my-skill/
   ├── SKILL.md        # Required: main skill definition
   ├── reference.md    # Optional: detailed documentation loaded on-demand
   ├── examples.md     # Optional: additional examples
   └── scripts/        # Optional: utility scripts
       └── helper.sh
   ```

4. **Apply configuration**:
   ```bash
   home-manager switch
   ```

5. **Test the skill**:
   - Make a request that matches the skill's description
   - Claude should autonomously invoke the skill when relevant
   - Check that it composes with other skills as expected

### Best Practices

**Keep skills focused**: One capability per skill
**Progressive disclosure**: Main SKILL.md as table of contents, reference files loaded on-demand
**Conciseness is critical**: Every token must justify its cost
**Specific descriptions**: Include concrete triggers, not just general capabilities
**Test across models**: Verify with Haiku, Sonnet, and Opus

**Example structure:**
```markdown
# Overview
Brief summary of capability

## When to Invoke
- Trigger condition 1
- Trigger condition 2

## Quick Start
[Most common usage pattern]

## Detailed Workflows
See reference.md for advanced patterns

## Reference Files
- reference.md: Detailed documentation
- examples.md: Advanced examples
```

### Skill vs Agent vs Command?

**Skills** (model-invoked):
- Claude decides when to use
- Always active and available
- Best for: Workflows, capabilities, expertise

**Agents** (delegated):
- Separate context window
- Invoked explicitly or by description match
- Best for: Specialized tasks, research, isolation

**Commands** (user-invoked):
- You explicitly call them with `/command`
- Direct control over execution
- Best for: Frequent prompts, shortcuts, team workflows

---

## Statusline Configuration

The statusline displays session information (model, version, git status, cost, context, etc.) using `claude-code-statusline.sh`.

### Color Palette

Standardized RGB values ensure consistency:

```bash
readonly RED="255;120;120"      # Model name
readonly ORANGE="255;180;100"   # Version
readonly GREEN="120;220;120"    # Git branch/status
readonly BLUE="100;180;255"     # Project directory
readonly PURPLE="180;140;255"   # Cost
readonly CYAN="100;200;200"     # Agent metrics
readonly PINK="255;140;180"     # Transcript ID
```

Context segment uses dynamic colors based on token usage:
- Green: Low (<100k tokens)
- Orange: Medium (100k-150k tokens)
- Red: High (>150k tokens)

### Development Guidelines

**Always run shellcheck after changes:**
```bash
shellcheck nixos-shared/claude/claude-code-statusline.sh
```

**Requirements:**
- Use `printf` for escape sequences (not `echo`)
- Quote all variable expansions
- Use `readonly` for constants
- Follow existing naming patterns

### Testing

```bash
cat /tmp/input.json | ./nixos-shared/claude/claude-code-statusline.sh
```

The script expects JSON input with fields like `model.display_name`, `version`, `transcript_path`, cost information, and context metrics.

---

## Configuration Workflow

### Making Changes

1. **Edit files** in `nixos-shared/claude/`
2. **Apply configuration**:
   ```bash
   home-manager switch
   ```
3. **Verify** the changes are active:
   ```bash
   ls -la ~/.claude/agents/
   ls -la ~/.claude/commands/
   # etc.
   ```

### Testing New Extensions

- **Agents**: Use Task tool or wait for automatic invocation
- **Output Styles**: `/output-style select style-name`
- **Commands**: `/command-name arguments`
- **Skills**: Make requests matching skill descriptions

### Sharing Configurations

All configurations in `nixos-shared/claude/` are:
- **Version controlled** via git
- **Shared across machines** via NixOS config
- **Team shareable** when committed to repository

Project-specific configurations (like `.claude/commands/` in a project repo) take precedence over user-level configurations.
