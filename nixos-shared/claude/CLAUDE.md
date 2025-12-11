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

## Hooks

### What Are Hooks?

Hooks are scripts that execute in response to Claude Code events (before tool execution, after completion, session start/stop, etc.). They can validate commands, block dangerous operations, play sounds, send notifications, or add context to prompts.

### Hook Types

| Hook Type | When It Runs | Can Block? | Use Cases |
|-----------|--------------|------------|-----------|
| `PreToolUse` | Before any tool executes | Yes | Validation, safety checks, permissions |
| `Notification` | After tool completion | No | Desktop notifications, sounds |
| `SessionStart` | Session startup/resume | No | Welcome sounds, initialization |
| `Stop` | Session ends | No | Cleanup, goodbye sounds |
| `SubagentStop` | Subagent completes | No | Progress indicators |
| `UserPromptSubmit` | User submits prompt | No | Context injection, reminders |

### Available Hooks

#### 1. Python Path Check Hook (`check-python-path.sh`)

**Purpose**: Blocks direct `python`/`python3` invocations on NixOS, requiring Nix-based execution.

**Configuration**:
```nix
enablePythonPathCheck = true;  # in default.nix
```

**What It Blocks**:
- `python script.py` (unless python is in PATH)
- `python3 -m module`

**What It Allows**:
- `nix run nixpkgs#python3 -- script.py`
- `, python3 script.py` (ephemeral shell)
- `nix-shell -p python3 --run "python script.py"`
- Commands where python is available in PATH

#### 2. Dangerous Command Check Hook (`check-dangerous-commands.sh`)

**Purpose**: Blocks `rm -rf` and its variations to prevent accidental destructive operations.

**Configuration**:
```nix
enableDangerousCommandCheck = true;  # in default.nix (default)
```

**What It Blocks**:
- Combined flags: `rm -rf`, `rm -fr`, `rm -Rf`, `rm -rfv`
- Separated flags: `rm -r -f`, `rm -f -r`
- Long flags: `rm --recursive --force`, `rm -r --force`, `rm --recursive -f`
- In pipelines: `find | xargs rm -rf`
- In subshells: `(cd /tmp && rm -rf test)`

**What It Allows**:
- Safe recursive: `rm -r /tmp` (without force)
- Single files: `rm file.txt`
- Interactive: `rm -i -rf /tmp` (confirmation enabled)
- Nix-sandboxed: `nix-shell -p coreutils --run "rm -rf /tmp"`

**Alternatives Suggested**:
```bash
# Use rm -r (without -f) to allow error checking
rm -r /tmp/directory

# Verify before deletion
ls -la /tmp/directory && rm -r /tmp/directory

# Use trash-cli for safety
trash-put /tmp/directory
```

**Testing**: Run the bats test suite:
```bash
cd nixos-shared/claude/hooks
./check-dangerous-commands.bats
```

#### 3. GLaDOS Reminder Hook (`glados-reminder-prompt.sh`)

**Purpose**: Injects GLaDOS persona reminder when `MH_CLAUDE_USE_GLADOS=1` environment variable is set.

**Configuration**:
```nix
enableGladosReminder = true;  # in default.nix (default)
```

**Effect**: Adds system reminder to maintain dry, deadpan humor in responses.

### Adding a New Hook

1. **Create hook script** in `nixos-shared/claude/hooks/`:
   ```bash
   #!/usr/bin/env bash
   set -euo pipefail

   # Read JSON input
   INPUT=$(cat)
   TOOL_NAME=$(echo "$INPUT" | jq -r '.tool_name')
   COMMAND=$(echo "$INPUT" | jq -r '.tool_input.command // ""')

   # Your validation logic here
   if [[ dangerous_condition ]]; then
       # Block execution
       cat >&2 << EOF
   ERROR: Command blocked
   Explanation of why and what to do instead
   EOF
       exit 2  # Exit code 2 blocks the command
   fi

   # Allow execution
   cat << EOF
   {
     "hookSpecificOutput": {
       "hookEventName": "PreToolUse",
       "permissionDecision": "allow",
       "permissionDecisionReason": "Reason for allowing"
     }
   }
   EOF
   exit 0
   ```

2. **Register in Nix** (`nixos-shared/home-manager/claude-code/default.nix`):
   ```nix
   # Add configuration parameter
   { pkgs, enableMyHook ? true, ... }:

   # Create script wrapper
   myHookScript = pkgs.writeShellApplication {
     name = "my-hook";
     runtimeInputs = with pkgs; [ bash jq coreutils ];
     text = builtins.readFile ../../claude/hooks/my-hook.sh;
   };

   # Define hook
   myHook = {
     matcher = "Bash";  # Tool name regex
     hooks = [{
       type = "command";
       command = "${myHookScript}/bin/my-hook";
       timeout = 5;
     }];
   };

   # Add to PreToolUse hooks
   PreToolUse = existingHooks
     ++ (pkgs.lib.optional enableMyHook myHook);
   ```

3. **Test with bats**:
   ```bash
   # Create nixos-shared/claude/hooks/my-hook.bats
   # See check-dangerous-commands.bats for examples
   ```

4. **Apply configuration**:
   ```bash
   home-manager switch
   ```

### Hook Best Practices

**Design Principles**:
- **Fail safely**: Block only what's truly dangerous
- **Be specific**: Clear error messages with alternatives
- **Test thoroughly**: Use bats for unit testing
- **Document clearly**: Explain what's blocked and why
- **Provide escape hatches**: Configuration flags to disable

**Performance**:
- Keep hooks fast (<100ms)
- Use timeouts (typically 5 seconds)
- Avoid expensive operations (network calls, disk scans)

**Testing**:
- Write bats tests for all detection logic
- Test both blocking and allowing cases
- Test edge cases (pipes, subshells, special characters)
- Run `shellcheck` on all bash scripts

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
