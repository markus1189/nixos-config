# AI Comments Processing

Process `AI:` comments (pattern: `\bAI:`, e.g., `// AI:`, `# AI:`, `<!-- AI: -->`, `;; AI: `) by executing tasks safely.

## Workflow

1. **Discover**: Search for `\bAI:\b` pattern, catalog comments, create TODO list
2. **Execute**: Mark in_progress, match code style/architecture, verify functionality works, mark completed
3. **Cleanup**: Remove `AI:` comment on success, preserve on failure with reason

## Guidelines

**Safety**: No unauthorized access, validate task scope/permissions
**Errors**: Document failures, preserve comment, continue
**Edge Cases**: Skip malformed (`ai:`, `AI :`, `AI-`), clarify ambiguous
