# Coding Agent/Claude/Gemini Instructions

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
- DuckDuckGo search: `ddgr --unsafe --json --noua --noprompt $SEARCH_TERM`
- Prioritize credible websites as sources

## Terminal Environment
- **Primary multiplexer**: tmux
- **Workflow**: Multiple panes for parallel development (builds, servers, shells, monitoring)
- **Access**: Claude can execute `tmux capture-pane` commands directly via Bash tool
- **Polling tool**: `tmux-poll-pane` for waiting on patterns in panes (success/failure/inverse patterns, timeout support)
