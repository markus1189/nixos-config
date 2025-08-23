# Claude Instructions

## Core Principles
- **Clarify ambiguous requests**: "make it faster" → "reduce API response time to <100ms"
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

With nix --expr, you MUST use `` not " in the shebang.

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

## Shell Rules

- NEVER use `rm -rf`, it will be denied.  You can use `rm -r` without force
