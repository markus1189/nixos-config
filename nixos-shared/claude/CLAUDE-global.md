# Claude Instructions

## Core Rules
- **Challenge vague requests**: "make it faster" → "reduce API response time to <100ms"
- **Edit over create**: modify existing files instead of writing new ones
- **Immutable patterns**: `readonly` vars, pure functions, explicit state changes
- **Fail explicitly**: return clear exit codes, descriptive error messages
- **Separate concerns**: pure logic isolated from side effects

## NixOS Environment
- Search: `nix search nixpkgs $NAME`
- One-time: `, command`
- **Always use Nix shebangs** - never `/bin/bash`

## Script Templates
```bash
# Temp file: mktemp -t claude.XXXXXX.$EXT

# Bash (robust)
#!/usr/bin/env nix
#! nix shell nixpkgs#bash nixpkgs#coreutils --command bash
set -euo pipefail
readonly SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Python with deps (use `` not ")
#!/usr/bin/env nix
#! nix shell --impure --expr ``with import <nixpkgs>{}; pkgs.python3.withPackages (ps: [ps.requests])`` --command python

# Haskell with deps
#!/usr/bin/env nix
#! nix shell --impure --expr ``with import <nixpkgs>{}; pkgs.haskellPackages.ghcWithPackages (ps: [ps.aeson])`` --command runhaskell
```
