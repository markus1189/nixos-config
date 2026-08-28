# Global Coding Agent Instructions

## Output Guides

1. Use these to mark severities/priorities/etc: 🔴🟠🟡🟢🔵
2. close longer answers with a TL;DR

## Verify Before Asserting

1. Never judge what you have not read this session. Same bar for
   consequences ("this breaks X"): trace it or call it a guess. Can't
   check? Say "unchecked" in that sentence.

2. Empty output is not evidence: check the command ran (`command -v`, exit status)
   before concluding "none". Read git branch/HEAD in the call that acts, never recall it.

## Environment (NixOS)
- Search packages: `nix search nixpkgs $NAME`
- One-time commands: `nix run nixpkgs#$program` or comma via `, command`
- Scripts: Use Nix shebangs (see templates below)
- Flakes: use `nix develop` or `nix run` etc
- Flake `src` = the git index: new files are invisible to `nix build`
- Editable System Config Location in ~/repos/nixos-config
- Read upstream source: `nix build --no-link --print-out-paths nixpkgs#$pkg.src`

## Script Templates
Nix shebang (simple packages):
```bash
#!/usr/bin/env nix
#! nix shell nixpkgs#bash nixpkgs#coreutils --command bash
set -euo pipefail
readonly SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Temp files: mktemp -t claude-code.XXXXXX.$EXT
```

Python: global `python3` (`nixos-shared/python-env.nix`) ships requests,
httpx, pyyaml, rich, beautifulsoup4, lxml, lz4, python-dateutil, numpy, pandas,
matplotlib, pillow, psutil, pytest, tiktoken. No pip. Use `#!/usr/bin/env python3` directly.

Only for packages OUTSIDE that list:
```bash
#! /usr/bin/env nix
#! nix shell --impure --expr ``
#! nix with (import (builtins.getFlake ''nixpkgs'') {});
#! nix python3.withPackages (ps: with ps; [ polars ])
#! nix ``
#! nix --command python3
```

Haskell:
```bash
#!/usr/bin/env nix
#! nix shell --impure --expr ``with import (builtins.getFlake ''nixpkgs'') {}; haskellPackages.ghcWithPackages (ps: [ps.aeson])`` --command runhaskell
```

Syntax notes:
- `--expr` requires double backticks: `--expr ``code`` --command`
- Simple packages: `nixpkgs#package --command`

## Web Search and Fetching
- DuckDuckGo search: `ddgr --unsafe --json --noua $SEARCH_TERM` (`--json` implies `--noprompt`), prioritize credible websites as sources
- Extract readable content from URLs: `curl -sL "$URL" | pandoc -f html -t gfm-raw_html`

## PDFs
- Extracted text ≠ the document. Ticks/X marks, form-field state, colour coding, stamps, signatures, strikethrough and markers in diagrams extract to nothing or to identical text
- Any claim about a visual state needs a render first: `pdftoppm -png -r 400 -f 1 -l 1 $FILE out`
- crop dense tables/images/graphs with `magick` (not `convert`)

## Terminal Environment
- Extensive tmux use, access pane content: `tmux capture-pane -p -t '%123'` (replace 123 with global pane id)
- The Bash tool runs **zsh**: quote glob-bearing args or zsh expands them against the cwd first: `grep -rn --include='*.nix'`; unquoted gives "no matches found"
- Hook-blocked: `rm -rf` (use `rm -r`); `find`/`fd` rooted at `/`, `~`,
  `/home/markus`, `/nix/store` — scope to a subpath, or use `rg`

## Clipboard (xclip)
`xclip -i` forks a daemon that outlives the shell to own the selection, and it
inherits stdout/stderr — so any caller reading those to EOF (command
substitution, a captured pipe) blocks forever. Redirect them:
```bash
echo "text" | xclip -selection clipboard -i >/dev/null 2>&1
```
For HTML clipboard content, add `-t text/html`.

## Find Installed Emacs Package Source
`emacsclient --eval "(locate-library \"PACKAGE\")" | tr -d '"'` → list dir → read source/docs

## Important Locations
- ~/mounts/rclone = rclone FUSE mounts (Ablage = ~/mounts/rclone/gdrive/Ablage); NO recursive walk (`find`/`fd`/`rg`), it blows the timeout; `ls` the likely dir instead
- ~/Syncthing (/ePubs for eBooks)
- ~/Stuff/yyyy-mm/dd-scratch (daily directories), ~/Stuff/Today/
