# Global Claude Memory File

## Notifications

🚨 MANDATORY: ALERT ON SUBSTANTIAL TASK COMPLETION 🚨

Alert me when substantial tasks complete - use judgment for what constitutes "substantial":

**Always Notify:**
- Multi-step operations (builds, tests, analysis of multiple files)
- Tasks taking >30 seconds
- Tasks that modify system state
- Error conditions or failures
- User input requests

**Examples:**
```bash
notify-send "claude - build system" "completed - 3 packages built successfully"
notify-send "claude - test suite" "completed - 42/42 passed"
notify-send "claude - error" "failed - dependency missing: postgresql"
notify-send "claude - user input" "please review the proposed changes"
```

**Don't Notify:**
- Simple file reads
- Single-line edits
- Trivial bash commands
- Basic searches

**Error Notifications:**
```bash
notify-send "claude - error" "build failed - see logs for details"
notify-send "claude - warning" "tests passed but with 3 warnings"
```

ENFORCEMENT: Failure to alert on substantial task completion violates core instructions.

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
