# Checklist for Effective Skills

Before sharing a skill, verify against this checklist.

## Core Quality

- [ ] Description is specific and includes key terms
- [ ] Description includes both what the skill does AND when to use it
- [ ] Description uses third person only (no "I can" or "You can")
- [ ] SKILL.md body is under 500 lines
- [ ] Additional details are in separate reference files (if needed)
- [ ] No time-sensitive information (or moved to "old patterns" section)
- [ ] Consistent terminology throughout
- [ ] Examples are concrete, not abstract
- [ ] File references are one level deep from SKILL.md
- [ ] File references use markdown link syntax: `[path](path)`
- [ ] Progressive disclosure used appropriately
- [ ] Workflows have clear steps
- [ ] Reference files >100 lines have table of contents

## Code and Scripts

- [ ] Scripts solve problems rather than punt to Claude
- [ ] Error handling is explicit and helpful
- [ ] No "voodoo constants" (all magic numbers justified and documented)
- [ ] Required packages listed in instructions and verified as available
- [ ] Scripts have clear documentation
- [ ] No Windows-style paths (all forward slashes)
- [ ] Validation/verification steps for critical operations
- [ ] Feedback loops included for quality-critical tasks
- [ ] MCP tool references use fully qualified names (ServerName:tool_name)

## Testing

- [ ] At least 3 evaluation scenarios created
- [ ] Tested with Haiku (may need more explicit guidance)
- [ ] Tested with Sonnet (balanced)
- [ ] Tested with Opus (may be over-explained)
- [ ] Tested with real usage scenarios
- [ ] Team feedback incorporated (if applicable)
- [ ] Observed how Claude navigates the skill (file access patterns)

## Naming

- [ ] Name is hyphen-case (lowercase letters, digits, hyphens)
- [ ] Name is max 64 characters
- [ ] Name doesn't contain reserved words ("anthropic", "claude")
- [ ] Name doesn't start/end with hyphen or have consecutive hyphens
- [ ] Gerund form preferred (e.g., "processing-pdfs", "analyzing-data")

## Description

- [ ] Non-empty
- [ ] Max 1024 characters
- [ ] No XML tags (angle brackets)
- [ ] Third person only
- [ ] Includes specific triggers/contexts for when to use
- [ ] Not vague ("helps with documents" → bad)
