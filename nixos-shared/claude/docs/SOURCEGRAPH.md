# Sourcegraph CLI Usage Guide

## Installation

### NixOS (Recommended)
```bash
# Run once without installing
nix shell nixpkgs#src-cli --command src --help

# Or install permanently
nix-env -iA nixpkgs.src-cli
```

### Manual Installation
```bash
# Linux
curl -L https://sourcegraph.com/.api/src-cli/src_linux_amd64 -o ~/.local/bin/src
chmod +x ~/.local/bin/src

# macOS
curl -L https://sourcegraph.com/.api/src-cli/src_darwin_amd64 -o /usr/local/bin/src
chmod +x /usr/local/bin/src
```

## Authentication

```bash
# Login to Sourcegraph instance
src login https://YOUR-SOURCEGRAPH-INSTANCE

# This sets environment variables:
# - SRC_ENDPOINT
# - SRC_ACCESS_TOKEN
```

## Basic Commands

```bash
# Basic search
src search 'search-query'

# Search with repository filter
src search 'r:repository-name search-query'

# Search with language filter
src search 'lang:python function-name'
```

## Search Syntax Reference

### Repository Filters
- `repo:repository-path` - Search within specific repository
- `repo:^github\.com/ORG/.*` - Search all repos in organization
- `repo:has.path(file-path)` - Find repos containing specific file
- `repo:has.content(content)` - Search repos with specific content

### Language Filters
- `lang:python` - Search Python files only
- `-lang:java` - Exclude Java files
- Supports all major programming languages

### Time-Based Filters
- `before:last-week` - Code committed before last week
- `after:february-10-2021` - Code committed after date
- Works with `type:commit` or `type:diff`

### Search Types
- `type:file` - Search file contents (default)
- `type:path` - Search file paths
- `type:commit` - Search commit messages
- `type:diff` - Search diff content
- `type:repo` - Search repository names
- `type:symbol` - Search code symbols

### Additional Filters
- `archived:yes/no/only` - Search archived repositories
- `case:yes/no` - Toggle case-sensitive search
- `message:commit-message` - Search commit messages

## Example Searches

```bash
# Find all Python functions named 'parse'
src search 'lang:python type:file parse'

# Search for recent commits in organization
src search 'repo:^github\.com/myorg/.* type:commit after:last-week'

# Find files containing specific import
src search 'import pandas repo:has.path(requirements.txt)'

# Search for TODO comments in active repositories
src search 'TODO archived:no'

# Search within specific repository
src search 'r:github.com/sourcegraph/src-cli NewArchiveRegistry'

# Case-sensitive search
src search 'case:yes MyFunction'

# Search commit messages
src search 'type:commit message:bugfix'
```

## Common Syntax Issues

**CRITICAL**: The entire search query must be in single quotes as ONE argument:
- ✅ Correct: `src search 'spawnUrgencyHook lang:haskell'`
- ❌ Wrong: `src search 'spawnUrgencyHook' lang:haskell`

All filters (lang:, repo:, type:, etc.) go inside the same quoted string.

## Pro Tips

- Combine multiple filters for precise results
- Default searches are case-insensitive
- Works on both Sourcegraph.com and private instances
- Use quotes for multi-word search terms
- Regular expressions supported in search patterns
- Can search across multiple repositories simultaneously

## Troubleshooting

- For authentication issues, check the "frontend" container logs
- On Apache proxies, remove `SetInputFilter DEFLATE` from `httpd.conf` to resolve gzip errors
- Verify `SRC_ENDPOINT` and `SRC_ACCESS_TOKEN` environment variables are set after login