# Global Claude Memory File

## Notifications

🚨 MANDATORY: ALWAYS ALERT ON TASK COMPLETION 🚨

Alert me when ANY task completes - this is REQUIRED, not optional:

notify-send "claude - status review" "completed - found 3 pending items"
notify-send "claude - code analysis" "finished - 5 files examined"
notify-send "claude - test suite" "completed - 42/42 passed"

For User Input Requests:
notify-send "claude - user input" "please review the proposed changes"
notify-send "claude - decision needed" "should I proceed with the migration?"

ENFORCEMENT: Failure to alert on task completion violates core instructions.

## OS Specifics

- you are running on nixos and MUST use nix's package management
- you can search for packages via `nix search nixpkgs $NAME`
- if a binary is not installed run programs once without installing by prefixing with ', '
- if asked to, copy information to clipboard by piping it to 'clip':
  - cat $FILE | clip
  - echo foo | clip

## Coding Style

- no matter the language, prefer an immutable coding style when possible
- keep "effects" separated from pure logic as would be the case in haskell

## Shell Scripts

For shell scripts, ALWAYS use Nix shell shebangs to ensure reproducibility and dependency management:

### Basic Pattern
```bash
#!/usr/bin/env nix
#! nix shell nixpkgs#bash nixpkgs#coreutils --command bash
```

### With Specific Dependencies
```bash
#!/usr/bin/env nix
#! nix shell nixpkgs#bash nixpkgs#git nixpkgs#curl --command bash
```

### For Other Languages
```python
#!/usr/bin/env nix
#! nix shell nixpkgs#python3 nixpkgs#python3Packages.requests --command python
```

### Key Rules:
- Use two separate lines (OS limitation prevents single line)
- Specify all dependencies explicitly in the shebang
- Use `nixpkgs#package` format for packages
- Scripts become self-contained with automatic dependency resolution
- NEVER use `/bin/bash` or `/usr/bin/env bash` directly
