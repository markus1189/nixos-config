# Global Claude Memory File

## OS Specifics

- you are running on nixos and MUST use nix's package management
- you can search for packages via `nix search nixpkgs $NAME`
- if a binary is not installed run programs once without installing by prefixing with ', '
- if asked to, copy information to clipboard by piping it to 'clip':
  - cat $FILE | clip
  - echo foo | clip

## Coding Style

- no matter the language, prefer an immutable coding style
- keep "effects" separated from pure logic as would be the case in haskell

## Shell Scripts and Throwaway Scripts for you

If you want to create a temporary script to execute, use:

```bash
mktemp -t claude.XXXXXX.txt
```

Instead of '.txt' you can use the specific extension of the script to write, e.g. '.py' for python.

To write shell scripts, ALWAYS use Nix shell shebangs to ensure reproducibility and dependency management:

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
Writing a python script with the 'prettytable' package available:

```python
#!/usr/bin/env nix
#! nix shell --impure --expr ``with import <nixpkgs>{}; pkgs.python3.withPackages (ps: with ps; [prettytable])`` --command python

print("Hello World")
```

Writing a haskell script:

```haskell
#!/usr/bin/env nix
#! nix shell --impure --expr ``with import <nixpkgs>{}; pkgs.haskellPackages.ghcWithPackages (ps: with ps; [wreq])`` --command runhaskell

main = putStrLn "Hello World"
```

### Key Rules:
- Use two separate lines (OS limitation prevents single line)
- Specify all dependencies explicitly in the shebang
- Use `nixpkgs#package` format for packages
- Scripts become self-contained with automatic dependency resolution
- NEVER use `/bin/bash` or `/usr/bin/env bash` directly

## Script Error Handling

ALL scripts must follow robust error handling patterns:

### Standard Error Handling
```bash
#!/usr/bin/env nix
#! nix shell nixpkgs#bash nixpkgs#coreutils --command bash

set -euo pipefail  # Exit on error, undefined vars, pipe failures
IFS=$'\n\t'       # Secure Internal Field Separator

# Cleanup function
cleanup() {
    local exit_code=$?
    # Cleanup temporary files, kill background processes, etc.
    [[ -n "${TEMP_DIR:-}" ]] && rm -rf "$TEMP_DIR"
    exit $exit_code
}
trap cleanup EXIT INT TERM
```

### Input Validation
```bash
# Validate required arguments
if [[ $# -lt 1 ]]; then
    echo "Error: Missing required argument" >&2
    echo "Usage: $0 <required_arg>" >&2
    exit 1
fi

# Validate file exists
readonly input_file="$1"
if [[ ! -f "$input_file" ]]; then
    echo "Error: File '$input_file' does not exist" >&2
    exit 2
fi
```

### Logging and Debugging
```bash
# Enable debug mode with environment variable
[[ "${DEBUG:-}" == "1" ]] && set -x

# Logging function
log() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $*" >&2
}

# Usage
log "Starting processing of $input_file"
```

### Immutable Patterns
- Use `readonly` for variables that shouldn't change
- Prefer functions over repeated code blocks
- Separate pure logic from side effects
- Return explicit exit codes for different error conditions
- Dependencies are handled by nix-shell shebang - no runtime checking needed
