# Coding Agent/Claude/Gemini Instructions

## Core Principles
- **Clarify ambiguous requests**: "make it faster" → "reduce API response time to less than 100ms"
- **Prefer modification**: Edit existing files for bug fixes, related features, config updates
- **Create when justified**: New files for tests, separate modules (>300 lines or unrelated logic)
- **Immutable patterns**: `readonly` vars, pure functions, explicit state changes
- **Fail explicitly**: Clear exit codes, descriptive error messages
- **Separate concerns**: Pure logic isolated from side effects

## Decision Framework
**Create new files when**:
- Adding a new module with unrelated domain logic
- Writing tests for existing code
- File would exceed ~300 lines with changes
- Framework/tooling requires separate files (package.json, tsconfig.json)

**Edit existing files when**:
- Fixing bugs or refactoring
- Adding features to existing modules (same domain)
- Updating configuration values
- Changes keep file under ~300 lines

## Environment (NixOS)
- Search packages: `nix search nixpkgs $NAME`
- One-time commands: `, command`
- **Scripts**: Use Nix shebangs (see templates below)
- **Flakes**: Modern projects use `nix develop` or `nix run` - adapt as needed

## Script Templates

**Nix shebang (simple packages)**:
```bash
#!/usr/bin/env nix
#! nix shell nixpkgs#bash nixpkgs#coreutils --command bash
set -euo pipefail
readonly SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Temp files: mktemp -t claude-code.XXXXXX.$EXT
```

**With package sets** (Python/Haskell):
```bash
# Python
#!/usr/bin/env nix
#! nix shell --expr ``with import <nixpkgs>{}; python3.withPackages (ps: [ps.requests])`` --command python

# Haskell
#!/usr/bin/env nix
#! nix shell --expr ``with import <nixpkgs>{}; haskellPackages.ghcWithPackages (ps: [ps.aeson])`` --command runhaskell
```

**Syntax notes**:
- `--expr` requires double backticks: `--expr ``code`` --command`
- Simple packages: `nixpkgs#package --command`
- Never use `--impure` unless accessing system state (rare)

## Shell Safety
- **NEVER use `rm -rf`** - explicitly forbidden, will be denied
- Use `rm -r` (without force) when recursive deletion needed
- Verify paths before deletion: `ls $DIR` then `rm -r $DIR`
- Use `ls -A` (not `ls -a`) to list files excluding `.` and `..`

## Screenshot Analysis
Find recent screenshots:
1. `find ~/Screenshots -mtime -1 -type f | sort -r`
2. Adjust `-mtime` as needed (-7 for week, -30 for month)
3. Read with Read tool and analyze

## Presenting Options
Prefix choices with clear identifiers for easy reference:
- "1. Option A", "2. Option B"
- "a) First choice", "b) Second choice"

## Using ddgr
DuckDuckGo search: `ddgr --json --noua --noprompt $SEARCH_TERM`
