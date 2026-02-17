# Global Coding Agent Instructions

## Environment (NixOS)
- Search packages: `nix search nixpkgs $NAME`
- There is no global python3, use nix to run scripts
- One-time commands: `nix run nixpkgs#$program` or comma via `, command`
- **Scripts**: Use Nix shebangs (see templates below)
- **Flakes**: Modern projects use `nix develop` or `nix run` - adapt as needed
- **Editable System Config Location** in ~/repos/nixos-config

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
# Python without packages
#!/usr/bin/env nix
#! nix shell nixpkgs#python3 --command python
```

```bash
# Python with packages (e.g. requests)
#! /usr/bin/env nix
#! nix shell --impure --expr ``
#! nix with (import (builtins.getFlake ''nixpkgs'') {});
#! nix python3.withPackages (ps: with ps; [ requests ])
#! nix ``
#! nix --command python3

# Haskell
#!/usr/bin/env nix
#! nix shell --impure --expr ``with import <nixpkgs>{}; haskellPackages.ghcWithPackages (ps: [ps.aeson])`` --command runhaskell
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

## Using ddgr
- DuckDuckGo search: `ddgr --unsafe --json --noua --noprompt $SEARCH_TERM`
- Prioritize credible websites as sources

## Web Content
- Extract readable content from URLs: `curl -sL "$URL" | pandoc -f html -t gfm-raw_html`

## Terminal Environment
- **Primary multiplexer**: tmux
- **Access**: `tmux capture-pane -p -t '%123'` (replace 123 with global pane id)

## Find Installed Emacs Package Source
`emacsclient --eval "(locate-library \"PACKAGE\")" | tr -d '"'` → list dir → read source/docs
