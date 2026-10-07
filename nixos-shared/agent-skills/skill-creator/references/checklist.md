# Checklist for Effective Skills

Before sharing a skill, verify against this checklist.

## Pre-Creation

- [ ] Domain experimented with before writing (not speculative — tried CLIs, libraries, workflows)

## Core Quality

- [ ] No derived data (don't spell out what Claude can figure out from info already provided)
- [ ] Description is specific and includes key terms
- [ ] Additional details are in separate reference files (if needed)
- [ ] No time-sensitive information (or moved to "old patterns" section)
- [ ] Consistent terminology throughout
- [ ] Examples are concrete, not abstract
- [ ] File references use markdown link syntax: `[path](path)`
- [ ] If skill wraps a library/API: Are official docs linked (when publicly accessible)?
- [ ] Progressive disclosure used appropriately
- [ ] Workflows have clear steps
- [ ] Fixes section contains only empirically observed failures (no speculative troubleshooting)

## Code and Scripts

- [ ] Scripts solve problems rather than punt to Claude
- [ ] Error handling is explicit and helpful
- [ ] Scripts have clear `--help` output and descriptive argument names (APIs outlast context attention)
- [ ] Scripts are single-touch where possible (fold setup + teardown into one command)
- [ ] Scripts expose clean, composable primitives (not monolithic with complex interdependencies)
- [ ] Scripts target repo-specific workflows (generic tools already exist)
- [ ] No "voodoo constants" (all magic numbers justified and documented)
- [ ] No Windows-style paths (all forward slashes)
- [ ] Validation/verification steps for critical operations
- [ ] Feedback loops included for quality-critical tasks
- [ ] MCP tool references use fully qualified names (ServerName:tool_name)

## Testing

- [ ] Tested with real usage scenarios
- [ ] Objectively verifiable skills only (exempt: subjective skills such as writing style or design, where direct user feedback is the test):
  - [ ] At least 3 evaluation scenarios created
  - [ ] Tested with Haiku (may need more explicit guidance), Sonnet, and Opus (may be over-explained)
- [ ] Team feedback incorporated (if applicable)
- [ ] Observed how Claude navigates the skill (file access patterns)


## Description

- [ ] Non-empty; name and description limits per SKILL.md frontmatter rules (`quick_validate.py` checks them)
- [ ] Includes specific triggers/contexts for when to use
- [ ] Not vague ("helps with documents" → bad)

## Install (nixos-config)

- [ ] Lives in `~/repos/nixos-config/nixos-shared/agent-skills/<name>/`
- [ ] New files `git add`ed (flake src = git index; untracked files are invisible to the build)
- [ ] `nix build --no-link .#agentSkills.<name>` passes (frontmatter, shellcheck, py_compile)
- [ ] Rebuilt; the skill shows up in `~/.claude/skills/`
