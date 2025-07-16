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

## Sourcegraph CLI (src) Usage

The Sourcegraph CLI (`src`) is available for searching code across repositories. Key usage patterns:

### Basic Search Syntax
```bash
src search 'search-term'                    # Basic search
src search 'search-term lang:python'        # Filter by language
src search 'search-term type:file'          # Search file names only
src search 'search-term repo:owner/repo'    # Search specific repository
```

### Advanced Filtering
```bash
# Language filters
src search 'function lang:python'           # Python files only
src search 'function lang:shell'            # Shell scripts only
src search 'function lang:javascript'       # JavaScript files

# File type filters
src search 'config type:file'               # Search filenames
src search 'bug type:commit'                # Search commit messages
src search 'TODO type:diff'                 # Search diffs

# Repository filters
src search 'r:github.com/user/repo function'  # Specific repo
src search 'repo:has.path(Dockerfile) nginx'  # Repos with Dockerfile

# Time filters
src search 'bug after:2023-01-01'           # Recent commits
src search 'fix before:2023-12-31'          # Older commits
```

### Regex and Pattern Matching
```bash
# Regex patterns (use .* for wildcards)
src search 'dunstify.*--action'             # Match dunstify with --action flag
src search 'function.*\(.*\).*{'            # Function definitions
src search 'import.*pandas'                 # Import statements

# Case sensitivity
src search 'Error case:yes'                 # Case sensitive
src search 'error case:no'                  # Case insensitive (default)
```

### Common Search Patterns
```bash
# Find configuration files
src search 'nginx lang:conf'
src search 'database type:file file:\.env$'

# Find function definitions
src search 'def.*function_name lang:python'
src search 'function.*function_name lang:javascript'

# Find imports/includes
src search 'import.*library_name'
src search '#include.*header.h'

# Find TODO/FIXME comments
src search 'TODO|FIXME'
src search 'XXX|HACK'
```

### Output and Formatting
```bash
src search -json 'search-term'              # JSON output
src search 'search-term' | head -20         # Limit results
```

### Key Rules
- Always wrap the entire search query in single quotes
- Use `.*` for wildcard matching in regex patterns
- Combine filters with spaces (they act as AND)
- Use `lang:` for programming language filtering
- Use `type:` to specify what to search (file, commit, diff, etc.)
- Use `repo:` or `r:` for repository filtering
