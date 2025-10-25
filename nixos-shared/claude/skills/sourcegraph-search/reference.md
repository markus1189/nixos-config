# Sourcegraph Search Query Syntax - Complete Reference

This document provides comprehensive documentation for Sourcegraph search query syntax, operators, filters, and pattern types.

## Table of Contents

1. [Query Structure](#query-structure)
2. [Pattern Types](#pattern-types)
3. [Repository Filters](#repository-filters)
4. [File and Content Filters](#file-and-content-filters)
5. [Search Type Filters](#search-type-filters)
6. [Boolean Operators](#boolean-operators)
7. [Time and Scope Filters](#time-and-scope-filters)
8. [Result Manipulation](#result-manipulation)
9. [CLI-Specific Options](#cli-specific-options)
10. [Advanced Regex Patterns](#advanced-regex-patterns)

---

## Query Structure

A Sourcegraph query consists of:
- A search **pattern** (literal text, regex, or structural)
- Zero or more **filters** (repo:, lang:, file:, etc.)
- Optional **operators** (AND, OR, NOT)
- Optional **modifiers** (case:, patternType:, etc.)

```
PATTERN [filter:value] [filter:value] [operator PATTERN] [modifier:value]
```

### Examples

```bash
# Simple pattern
error

# Pattern + filter
error repo:kubernetes

# Pattern + multiple filters
error repo:kubernetes lang:go

# Pattern + filters + operator
(error OR exception) repo:kubernetes lang:go file:test

# Pattern + filters + modifiers
error repo:kubernetes case:yes patternType:regexp
```

---

## Pattern Types

Sourcegraph supports three pattern types for matching code.

### 1. Literal Search (Default)

Matches exact text. Case-insensitive by default.

**Syntax**: Just type your search term
**Special characters**: Automatically escaped

```bash
src search 'func main('
src search 'console.log'
src search 'import React from'
```

**When to use**:
- Searching for exact function names
- Finding specific strings or identifiers
- Looking for error messages
- Fast, simple searches

### 2. Regular Expression Search

Uses RE2 regex syntax for pattern matching.

**Syntax**: `patternType:regexp PATTERN` or use `/pattern/` in keyword mode

```bash
src search 'patternType:regexp func \w+Handler'
src search 'patternType:regexp import.*useState.*useEffect'
src search 'patternType:regexp (password|secret|api_key)\s*=\s*["\'][^"\']+["\']'
```

**RE2 Regex Syntax** (subset of common patterns):

| Pattern | Matches | Example |
|---------|---------|---------|
| `.` | Any character | `a.c` matches "abc", "a2c" |
| `*` | Zero or more | `ab*c` matches "ac", "abc", "abbc" |
| `+` | One or more | `ab+c` matches "abc", "abbc" |
| `?` | Zero or one | `ab?c` matches "ac", "abc" |
| `^` | Start of line | `^func` matches "func" at line start |
| `$` | End of line | `\.go$` matches ".go" at line end |
| `\|` | Alternation | `cat\|dog` matches "cat" or "dog" |
| `[]` | Character class | `[a-z]` matches any lowercase letter |
| `[^]` | Negated class | `[^0-9]` matches non-digits |
| `\d` | Digit | `\d+` matches one or more digits |
| `\w` | Word char | `\w+` matches word characters |
| `\s` | Whitespace | `\s+` matches whitespace |
| `\b` | Word boundary | `\bfunc\b` matches "func" as whole word |
| `()` | Capture group | `(foo\|bar)` groups alternation |
| `(?:)` | Non-capturing | `(?:foo\|bar)` groups without capture |

**Character escaping**:
```bash
# Escape special chars: . * + ? ^ $ | [] {} () \
src search 'patternType:regexp func\(\)'
src search 'patternType:regexp \[TODO\]'
```

**When to use**:
- Flexible pattern matching
- Searching for variations (singular/plural, etc.)
- Finding patterns with wildcards
- Complex code patterns

### 3. Structural Search

Syntax-aware search that understands code structure.

**Syntax**: `patternType:structural PATTERN`

Structural search uses **holes** (`:[var]`, `:[...]`, etc.) as placeholders:

| Hole | Matches | Example |
|------|---------|---------|
| `:[var]` | Single identifier or expression | `fmt.Sprintf(:[var])` |
| `:[...]` | Zero or more characters/expressions | `func(:[...])` |
| `:[x~regex]` | Expression matching regex | `:[msg~TODO\|FIXME]` |

**Examples**:

```bash
# Find sprintf with any format string
src search 'patternType:structural fmt.Sprintf(:[format], :[...])'

# Find function calls with specific argument count
src search 'patternType:structural authenticate(:[user], :[pass])'

# Find if statements with TODO/FIXME
src search 'patternType:structural if :[cond] { :[body~TODO|FIXME] }'

# Match any function definition
src search 'patternType:structural func :[name](:[params]) { :[...] }'
```

**When to use**:
- Matching based on code structure, not just text
- Finding balanced brackets/parentheses
- Searching for syntactic patterns
- Language-aware refactoring searches

**Limitations**:
- Slower than literal/regexp
- Not all languages fully supported
- More complex syntax

---

## Repository Filters

Control which repositories to search.

### `repo:REGEX`

Filter by repository path. Uses RE2 regex.

**Syntax**: `repo:PATTERN`
**Negation**: `-repo:PATTERN`

```bash
# Exact repository
src search 'repo:^github\.com/sourcegraph/sourcegraph$ error'

# Organization prefix
src search 'repo:^github\.com/kubernetes/ API'

# Multiple repos (OR logic)
src search 'repo:kubernetes|docker container'

# Exclude repositories
src search 'error -repo:archive -repo:test'

# All repos containing "auth"
src search 'repo:auth login'
```

**Multiple `repo:` filters = AND logic**:
```bash
# Matches repos with BOTH "foo" AND "bar" in path
src search 'repo:foo repo:bar'

# Example: github.com/company/foo-bar would match
```

**Common patterns**:
```bash
# Specific org
repo:^github\.com/myorg/

# Exclude archived
-repo:archived

# Multiple orgs
repo:^github\.com/(org1|org2)/

# Specific repo exact match
repo:^github\.com/user/project$
```

### `rev:REVISION`

Search at specific Git revision (branch, tag, commit).

**Syntax**: `rev:REVISION`

```bash
# Search on main branch
src search 'repo:myrepo rev:main config'

# Search on specific tag
src search 'repo:myrepo rev:v1.2.3 feature'

# Search on commit hash
src search 'repo:myrepo rev:abc123def456 bug'

# Multiple revisions (OR logic)
src search 'repo:myrepo rev:main:develop feature'
```

**Use cases**:
- Compare code across branches
- Search historical code at specific version
- Find when feature was introduced

### `repohasfile:PATH`

Only search repos that contain a matching file.

**Syntax**: `repohasfile:PATTERN`

```bash
# Repos with package.json
src search 'repohasfile:package\.json react'

# Repos with Dockerfile
src search 'repohasfile:^Dockerfile'

# Repos with specific config
src search 'repohasfile:\.github/workflows/ CI'
```

**Combined with content filters**:
```bash
# Find React projects with TypeScript
src search 'repohasfile:package\.json repohasfile:tsconfig\.json'
```

### `fork:yes|no|only`

Include/exclude forked repositories.

**Default**: `fork:no` (excludes forks)

```bash
# Include forks
src search 'fork:yes error'

# Only forks
src search 'fork:only bug'

# Exclude forks (default)
src search 'fork:no error'
src search 'error'  # Same as above
```

### `archived:yes|no|only`

Include/exclude archived repositories.

**Default**: `archived:no` (excludes archived repos)

```bash
# Include archived repos
src search 'archived:yes legacy'

# Only archived repos
src search 'archived:only deprecated'

# Exclude archived (default)
src search 'archived:no active'
src search 'active'  # Same as above
```

### `visibility:public|private|any`

Filter by repository visibility.

```bash
# Only public repos
src search 'visibility:public open-source'

# Only private repos
src search 'visibility:private internal'

# Both (default)
src search 'visibility:any code'
```

### `repo:has.meta(KEY:VALUE)`

Search repos with specific metadata key-value pairs.

```bash
# Repos tagged with specific metadata
src search 'repo:has.meta(team:backend) service'

# Multiple metadata filters
src search 'repo:has.meta(lang:go) repo:has.meta(type:service)'
```

---

## File and Content Filters

Control which files to search and how content is matched.

### `file:REGEX`

Filter by file path. Uses RE2 regex.

**Syntax**: `file:PATTERN`
**Negation**: `-file:PATTERN`

```bash
# JavaScript files
src search 'file:\.js$ import'

# TypeScript files (tsx or ts)
src search 'file:\.tsx?$ useState'

# Specific directory
src search 'file:^src/components/ render'

# Exclude test files
src search 'bug -file:test -file:spec'

# Only test files
src search 'file:(test|spec) assert'

# Specific filename
src search 'file:schema\.graphql type'

# Deep paths
src search 'file:internal/auth/ middleware'
```

**Common patterns**:
```bash
# Python files
file:\.py$

# Go test files
file:_test\.go$

# Config files
file:(config|configuration)\.(json|yaml|yml)$

# Exclude vendor/node_modules
-file:vendor/ -file:node_modules/

# Specific extension
file:\.(js|jsx|ts|tsx)$
```

### `lang:LANGUAGE`

Filter by programming language.

**Syntax**: `lang:LANGUAGE`
**Negation**: `-lang:LANGUAGE`

**Common languages**:
- `go`, `python`, `javascript`, `typescript`, `java`, `c`, `cpp`, `csharp`
- `rust`, `ruby`, `php`, `swift`, `kotlin`, `scala`, `haskell`
- `html`, `css`, `scss`, `json`, `yaml`, `markdown`, `sql`
- `shell`, `bash`, `dockerfile`, `makefile`

```bash
# Go files
src search 'lang:go func main'

# Python files
src search 'lang:python def authenticate'

# TypeScript files
src search 'lang:typescript interface User'

# Multiple languages (use OR)
src search '(lang:python OR lang:ruby) class'

# Exclude language
src search 'config -lang:json -lang:yaml'
```

**Advantages over `file:`**:
- Language detection handles files without extensions
- Automatically excludes generated files
- Works with language-specific features

### `content:PATTERN`

Explicitly specify the search pattern (useful when pattern looks like a filter).

**Syntax**: `content:"PATTERN"`

```bash
# Search for literal "repo:foo" text
src search 'content:"repo:foo" lang:markdown'

# Search for filter-like strings
src search 'content:"type:string" lang:typescript'

# Combine with other filters
src search 'content:"TODO" lang:go -file:test'
```

**When to use**:
- Pattern contains colons that might be interpreted as filters
- Searching for text that looks like Sourcegraph syntax
- Explicit separation of pattern from filters

### `case:yes|no`

Control case sensitivity.

**Default**: `case:no` (case-insensitive)

```bash
# Case-insensitive (default)
src search 'error'  # Matches: error, Error, ERROR

# Case-sensitive
src search 'case:yes Error'  # Only matches: Error

# Case-sensitive regex
src search 'patternType:regexp case:yes [A-Z][a-z]+Error'
```

---

## Search Type Filters

Change the type of search results returned.

### `type:TYPE`

Specify what to search.

**Available types**:
- `file` (default): Search file contents
- `path`: Search file paths only
- `symbol`: Search symbols (functions, classes, etc.)
- `commit`: Search commit messages
- `diff`: Search commit diffs

#### `type:file` (Default)

Search file contents.

```bash
src search 'type:file func main'
src search 'func main'  # Same as above
```

#### `type:path`

Search only file paths, not contents.

```bash
# Find files with "auth" in path
src search 'type:path auth'

# Find test files
src search 'type:path test'

# Find configuration files
src search 'type:path config\.(json|yaml)'
```

**Use case**: Finding files by name/path without searching contents.

#### `type:symbol`

Search for code symbols (functions, classes, methods, types).

```bash
# Find function definitions
src search 'type:symbol handleRequest'

# Find class definitions
src search 'type:symbol lang:python User'

# Find type definitions
src search 'type:symbol lang:typescript interface.*User'
```

**Use case**: Code navigation, finding definitions.

#### `type:commit`

Search commit messages.

```bash
# Find commits mentioning bug fix
src search 'type:commit bug fix'

# Find commits by author pattern
src search 'type:commit author:john refactor'

# Recent commits about feature
src search 'type:commit after:"1 week ago" feature'
```

**Use case**: Searching Git history, finding when changes were made.

#### `type:diff`

Search commit diffs (changes).

```bash
# Find when password was added/removed
src search 'type:diff password'

# Find API changes
src search 'type:diff lang:go func.*API'

# Security audit: find credential additions
src search 'type:diff (api_key|secret|password)'

# Recent changes
src search 'type:diff after:"1 month ago" authentication'
```

**Use case**: Finding when code was changed, security audits, refactoring analysis.

**Commit/diff-specific filters**:
- `author:PATTERN`: Filter by commit author
- `before:"DATE"`: Commits before date
- `after:"DATE"`: Commits after date
- `message:"PATTERN"`: Filter commit messages

```bash
# Commits by specific author in last month
src search 'type:commit author:alice after:"1 month ago"'

# Diffs before specific date
src search 'type:diff before:"2024-01-01" deprecated'
```

### `select:TYPE`

Convert or filter result types.

**Available types**:
- `file`: Return only file results
- `commit`: Return only commits
- `symbol`: Return only symbols
- `repo`: Return only repositories

```bash
# Search for pattern but only show repos containing it
src search 'select:repo error'

# Find symbols but return their files
src search 'type:symbol select:file UserAuth'

# Get repository list from file search
src search 'select:repo lang:go kubernetes'
```

**Use case**: Result type conversion, getting unique repos/files from search.

---

## Boolean Operators

Combine search patterns with logical operators.

### `AND` (Intersection)

Both patterns must match in the same file.

**Syntax**: `PATTERN1 AND PATTERN2` or `PATTERN1 and PATTERN2`

```bash
# Files containing both "mutex" and "lock"
src search 'mutex AND lock'

# Multiple conditions
src search 'authentication AND authorization AND middleware'

# With filters
src search 'repo:kubernetes AND lang:go AND error AND timeout'
```

**Operator precedence**: `AND` binds tighter than `OR`
```bash
# This means: foo OR (bar AND baz)
src search 'foo OR bar AND baz'

# Use parentheses for clarity
src search 'foo OR (bar AND baz)'
src search '(foo OR bar) AND baz'
```

### `OR` (Union)

Either pattern (or both) must match.

**Syntax**: `PATTERN1 OR PATTERN2` or `PATTERN1 or PATTERN2`

```bash
# Files containing "error" or "exception"
src search 'error OR exception'

# Multiple alternatives
src search 'TODO OR FIXME OR HACK'

# With filters
src search 'lang:python (class OR def)'

# Complex combinations
src search '(ImportError OR ModuleNotFoundError) lang:python'
```

### `NOT` / `-` (Negation)

Exclude pattern or filter.

**Syntax**: `NOT PATTERN` or `-PATTERN` or `-filter:value`

```bash
# Match "error" but not "exception"
src search 'error NOT exception'

# Using minus prefix
src search 'error -exception'

# Exclude files
src search 'TODO -file:test -file:vendor'

# Exclude repos
src search 'auth -repo:archived -repo:example'

# Exclude language
src search 'config -lang:json'
```

**Negation precedence**:
- `-` before filter name: excludes that filter
- `NOT` before pattern: excludes that pattern
- Can combine: `pattern1 AND NOT pattern2 -file:test`

### Parentheses `()`

Group expressions to control precedence.

```bash
# Without parens: (a AND b) OR c
src search 'a AND b OR c'

# Force different grouping
src search '(a AND b) OR c'  # Same as above
src search 'a AND (b OR c)'  # Different: a AND (b OR c)

# Complex example
src search 'repo:kubernetes (error OR exception) AND lang:go NOT timeout'
```

**Best practice**: Use parentheses for clarity, even when not strictly needed.

---

## Time and Scope Filters

### `before:DATE` / `after:DATE`

Filter commits/diffs by date.

**Only works with**: `type:commit` or `type:diff`

**Date formats**:
- `"YYYY-MM-DD"` - Specific date
- `"N days ago"` - Relative time
- `"N weeks ago"` - Relative time
- `"N months ago"` - Relative time
- `"N years ago"` - Relative time

```bash
# Commits in last week
src search 'type:commit after:"1 week ago" refactor'

# Diffs before specific date
src search 'type:diff before:"2024-01-01" deprecated'

# Date range
src search 'type:commit after:"2024-01-01" before:"2024-06-01" feature'

# Recent changes
src search 'type:diff after:"3 days ago" password'
```

### `author:PATTERN`

Filter commits by author.

**Only works with**: `type:commit` or `type:diff`

```bash
# Commits by specific author
src search 'type:commit author:alice'

# Author name pattern
src search 'type:commit author:.*smith.*'

# Author email
src search 'type:commit author:alice@company\.com'

# Combined with other filters
src search 'type:diff author:bob after:"1 month ago" security'
```

### `message:PATTERN`

Filter by commit message content.

**Only works with**: `type:commit`

```bash
# Commits with "fix" in message
src search 'type:commit message:fix'

# Specific message pattern
src search 'type:commit message:"Merge pull request"'

# Regex in message
src search 'type:commit message:(bug|issue).*[0-9]+'
```

### `count:N`

Limit number of results.

```bash
# Get first 10 results
src search 'count:10 error'

# Quick sample
src search 'count:5 TODO lang:go'

# With other filters
src search 'count:50 repo:kubernetes authentication'
```

**Use case**: Quick sampling, performance optimization, limiting output.

### `timeout:DURATION`

Set search timeout.

**Format**: `Ns` (seconds), `Nm` (minutes)
**Default**: 1 minute (cannot exceed this by default)

```bash
# 30 second timeout
src search 'timeout:30s complex.*regex'

# 2 minute timeout (if admin allows)
src search 'timeout:2m repo:huge-monorepo'
```

### `context:NAME`

Use search context (predefined repository groups).

```bash
# Search within context
src search 'context:global error'

# Multiple contexts
src search 'context:team-backend service'
```

**Use case**: Scoping searches to predefined repo sets, team contexts.

---

## Result Manipulation

### `count:all`

Return all results (remove default limit).

```bash
src search 'count:all pattern'
```

**Warning**: May be very slow on large result sets.

### `-display N` (CLI flag)

Limit displayed results (only with `-stream`).

```bash
src search -stream -display 20 'pattern'
```

**Note**: Statistics still show all results, only display is limited.

---

## CLI-Specific Options

### Flags

**Output format**:
- `-json`: Output results as JSON
- `-stream`: Stream results as they arrive
- `-display N`: Limit displayed results (with `-stream`)

**Debugging**:
- `-dump-requests`: Log GraphQL requests/responses
- `-trace`: Log trace ID for requests
- `-get-curl`: Print equivalent curl command (includes access token!)

**Security**:
- `-insecure-skip-verify`: Skip TLS certificate validation

**User agent**:
- `-user-agent-telemetry`: Include OS/arch in User-Agent (default: true)

### Special Syntax

**Query separator `--`**:

Use `--` to separate flags from query, especially for queries starting with `-`:

```bash
# Without --, this fails (parses -repo as flag)
src search '-repo:foo/bar error'  # ERROR

# With --, this works
src search -- '-repo:foo/bar error'  # OK
```

**Environment variables**:
- `NO_COLOR=t`: Disable colored output
- `COLOR=t`: Force colored output (even when piping)

```bash
# Disable colors
NO_COLOR=t src search 'pattern'

# Force colors
COLOR=t src search 'pattern' | less -R
```

### JSON Output Schema

Use `-explain-json` to see JSON schema:

```bash
src search -explain-json
```

**Result types in JSON**:
- `FileMatch`: Regular file search results
- `CommitSearchResult`: Commit/diff results
- `Repository`: Repository results

**Differentiate with `__typename` field**:

```json
{
  "results": {
    "results": [
      {
        "__typename": "FileMatch",
        "repository": { "name": "...", "url": "..." },
        "file": { "name": "...", "path": "...", "url": "..." },
        "lineMatches": [...]
      }
    ]
  }
}
```

### Examples with Flags

```bash
# JSON output
src search -json 'error lang:go'

# Streaming with display limit
src search -stream -display 10 'TODO'

# Debug GraphQL
src search -dump-requests 'pattern'

# Get equivalent curl command
src search -get-curl 'pattern'
```

---

## Advanced Regex Patterns

### Common Regex Patterns for Code Search

**Function definitions**:
```bash
# Go functions
src search 'patternType:regexp func \w+\('

# Python functions
src search 'patternType:regexp def \w+\('

# JavaScript/TypeScript functions
src search 'patternType:regexp (function|const|let|var) \w+\s*=\s*(\([^)]*\)|async)'
```

**Import statements**:
```bash
# Python imports
src search 'patternType:regexp ^import \w+'
src search 'patternType:regexp ^from [\w.]+ import'

# JavaScript/TypeScript imports
src search 'patternType:regexp import .* from ["\']'

# Go imports
src search 'patternType:regexp import \('
```

**Class definitions**:
```bash
# Python classes
src search 'patternType:regexp class \w+(\(.*\))?:'

# JavaScript/TypeScript classes
src search 'patternType:regexp class \w+ (extends|implements)?'

# Java classes
src search 'patternType:regexp (public|private|protected)?\s*class \w+'
```

**Variable assignments**:
```bash
# Const/let/var in JavaScript
src search 'patternType:regexp (const|let|var) \w+ ='

# Assignment with specific value
src search 'patternType:regexp \w+\s*=\s*null'
```

**Comments**:
```bash
# Line comments (various languages)
src search 'patternType:regexp //.*TODO'
src search 'patternType:regexp #.*FIXME'

# Block comments
src search 'patternType:regexp /\*.*TODO.*\*/'
```

**String literals**:
```bash
# Double-quoted strings
src search 'patternType:regexp "[^"]*password[^"]*"'

# Single-quoted strings
src search 'patternType:regexp '\''[^'\'']*api[^'\'']*'\'''

# Backtick strings (template literals)
src search 'patternType:regexp `[^`]*\$\{[^}]*\}[^`]*`'
```

**Error handling**:
```bash
# Try-catch blocks
src search 'patternType:regexp try\s*\{.*catch'

# Error returns in Go
src search 'patternType:regexp if err != nil'

# Raise/throw
src search 'patternType:regexp (raise|throw) \w+Error'
```

**URLs and endpoints**:
```bash
# HTTP URLs
src search 'patternType:regexp https?://[^\s"]+'

# API endpoints
src search 'patternType:regexp /(api|v[0-9]+)/[a-z/]+'
```

**Security patterns**:
```bash
# Credentials in code
src search 'patternType:regexp (password|secret|api_key|token)\s*=\s*["\'][^"\']+["\']'

# Private keys
src search 'patternType:regexp -----BEGIN.*PRIVATE KEY-----'

# Hardcoded IPs
src search 'patternType:regexp \b\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}\b'

# AWS keys
src search 'patternType:regexp (AKIA|ASIA)[0-9A-Z]{16}'
```

### RE2 Limitations

RE2 does NOT support:
- Backreferences: `(a)\1`
- Lookahead/lookbehind: `(?=pattern)`, `(?!pattern)`
- Conditional patterns: `(?(condition)yes|no)`

**Workarounds**:
- Use structural search for complex syntax
- Break complex patterns into multiple searches
- Use post-processing with JSON output

---

## Performance Optimization

### Fast Searches

1. **Use specific `repo:` filters**
   ```bash
   # Fast: specific repo
   src search 'repo:^github\.com/org/project$ pattern'

   # Slower: broad search
   src search 'pattern'
   ```

2. **Add `lang:` filters**
   ```bash
   # Fast: language-specific
   src search 'lang:go pattern'

   # Slower: all languages
   src search 'pattern'
   ```

3. **Use `file:` to narrow scope**
   ```bash
   # Fast: specific files
   src search 'file:\.go$ pattern'

   # Slower: all files
   src search 'pattern'
   ```

4. **Limit results with `count:`**
   ```bash
   # Fast: small result set
   src search 'count:10 pattern'

   # Slower: all results
   src search 'pattern'
   ```

5. **Use literal search when possible**
   ```bash
   # Fast: literal
   src search 'exact string'

   # Slower: regexp
   src search 'patternType:regexp complex.*pattern'

   # Slowest: structural
   src search 'patternType:structural :[...]'
   ```

### Query Optimization Tips

- Start with most restrictive filters
- Use literal search for exact matches
- Prefer `lang:` over `file:` when possible
- Combine filters: `repo:` + `lang:` + `file:`
- Use `select:repo` to get unique repository list
- Stream large result sets with `-stream`
- Set reasonable `count:` limits

---

## Common Pitfalls and Solutions

### Problem: No results

**Possible causes**:
- Repository not indexed
- Too restrictive filters
- Pattern doesn't match
- Wrong pattern type

**Solutions**:
```bash
# Remove filters one by one
src search 'pattern'  # Start broad
src search 'repo:specific pattern'  # Add filters incrementally

# Try different pattern types
src search 'pattern'  # Literal
src search 'patternType:regexp pattern.*'  # Regexp

# Check if repo is indexed
src search 'repo:^github\.com/org/project$'  # Should return results
```

### Problem: Too many results

**Solutions**:
```bash
# Add more filters
src search 'error repo:myorg lang:go -file:test'

# Use more specific pattern
src search 'specific-function-name'  # Instead of just "error"

# Limit results
src search 'count:20 pattern'
```

### Problem: Query starting with `-` fails

**Solution**: Use `--` separator
```bash
src search -- '-repo:foo error'
```

### Problem: Regex not working

**Possible causes**:
- Forgot `patternType:regexp`
- Using unsupported RE2 feature
- Need to escape special chars

**Solutions**:
```bash
# Add pattern type
src search 'patternType:regexp func \w+'

# Escape special chars
src search 'patternType:regexp func\(\)'

# Check RE2 compatibility (no lookahead/lookbehind)
```

### Problem: Structural search not matching

**Possible causes**:
- Syntax errors in structural pattern
- Language not fully supported
- Whitespace sensitivity

**Solutions**:
```bash
# Simplify pattern
src search 'patternType:structural func :[name](:[...])'

# Try regexp instead
src search 'patternType:regexp func \w+\('
```

---

## Quick Reference Tables

### Filters

| Filter | Purpose | Example |
|--------|---------|---------|
| `repo:` | Repository path | `repo:kubernetes` |
| `file:` | File path | `file:\.go$` |
| `lang:` | Programming language | `lang:python` |
| `type:` | Result type | `type:commit` |
| `rev:` | Git revision | `rev:main` |
| `case:` | Case sensitivity | `case:yes` |
| `patternType:` | Pattern type | `patternType:regexp` |
| `count:` | Result limit | `count:10` |
| `timeout:` | Search timeout | `timeout:30s` |
| `fork:` | Include forks | `fork:yes` |
| `archived:` | Include archived | `archived:yes` |
| `select:` | Result type conversion | `select:repo` |
| `before:` | Before date (commit/diff) | `before:"2024-01-01"` |
| `after:` | After date (commit/diff) | `after:"1 week ago"` |
| `author:` | Commit author | `author:alice` |

### Operators

| Operator | Purpose | Example |
|----------|---------|---------|
| `AND` | Both patterns | `error AND timeout` |
| `OR` | Either pattern | `error OR exception` |
| `NOT` | Exclude pattern | `error NOT test` |
| `-` | Negate filter/pattern | `-file:test` |
| `()` | Grouping | `(a OR b) AND c` |

### Pattern Types

| Type | Syntax | Use Case |
|------|--------|----------|
| Literal | (default) | Exact text matching |
| Regexp | `patternType:regexp` | Pattern matching with regex |
| Structural | `patternType:structural` | Syntax-aware code matching |

### CLI Flags

| Flag | Purpose |
|------|---------|
| `-json` | JSON output |
| `-stream` | Stream results |
| `-display N` | Limit displayed results |
| `-dump-requests` | Debug GraphQL |
| `-trace` | Log trace ID |
| `-get-curl` | Show curl equivalent |

---

## Further Resources

- Official documentation: https://sourcegraph.com/docs/code-search/queries/language
- RE2 syntax: https://github.com/google/re2/wiki/Syntax
- GraphQL API: https://sourcegraph.com/api/console
- Examples: See `examples.md` in this skill directory
