# Coding Agent/Claude/Gemini Instructions

## Core Principles
- **Clarify ambiguous requests**: "make it faster" → "reduce API response time to less than 100ms"
- **Prefer modification**: Edit existing files when the change fits naturally
- **Create when justified**: New files for distinct concerns, tests, or when editing would bloat existing code
- **Immutable patterns**: `readonly` vars, pure functions, explicit state changes
- **Fail explicitly**: Clear exit codes, descriptive error messages
- **Separate concerns**: Pure logic isolated from side effects

## Decision Framework
**Create new files when**:
- Adding a distinct feature/module
- Writing tests for existing code
- Configuration requires separate files
- Existing file would become unwieldy (>500 lines)

**Edit existing files when**:
- Fixing bugs or improving existing functionality
- Adding related functionality to appropriate modules
- Updating configuration values

## Environment Support
**Primary (NixOS)**:
- Search: `nix search nixpkgs $NAME`
- One-time: `, command`
- **Always use Nix shebangs** - never `/bin/bash`

**Fallback (standard)**:
- Use system package manager or existing tools
- Standard shebangs when Nix unavailable

## Script Templates

**Nix shebang syntax rules:**
  - `--expr` requires double backticks: `--expr ``code here`` --command`
  - Simple packages use hash syntax: `nixpkgs#package --command`
  - Prefer simple syntax when possible (no withPackages needed)

```bash
# Temp file: mktemp -t claude-code.XXXXXX.$EXT

# Bash (robust)
#!/usr/bin/env nix
#! nix shell nixpkgs#bash nixpkgs#coreutils --command bash
set -euo pipefail
readonly SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Python with deps
#!/usr/bin/env nix
#! nix shell --impure --expr ``with import <nixpkgs>{}; pkgs.python3.withPackages (ps: [ps.requests])`` --command python

# Haskell with deps
#!/usr/bin/env nix
#! nix shell --impure --expr ``with import <nixpkgs>{}; pkgs.haskellPackages.ghcWithPackages (ps: [ps.aeson])`` --command runhaskell
```

## Screenshot Analysis
When the user references screenshots or recent images:
1. Use `find ~/Screenshots -mtime -1 -type f | sort -r` as a starting point to locate recent screenshots
2. Adapt the find command as needed (adjust time range or file types based on context)
3. Read the most relevant screenshot using the Read tool and analyze its contents

## Shell Rules

- NEVER use `rm -rf`, it will be denied.  You can use `rm -r` without force
- don't use `ls -a`, use `ls -A` to list all files, but exclude '.' and '..'

## Presenting Options

When offering multiple choices or options to the user, always prefix
each with a clear identifier (numbers, letters, or short labels) so
they can easily reference their selection (e.g., "1. Option A",
"2. Option B" or "a) First choice", "b) Second choice")

## Using ddgr

Alternative websearch via DuckDuckGo can be done using `ddgr --json --noua --noprompt $SEARCH_TERM`
