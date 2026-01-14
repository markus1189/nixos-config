#!/usr/bin/env nix
#! nix shell nixpkgs#bash nixpkgs#plantuml --command bash
set -euo pipefail

# Usage: generate-diagram.sh [options] <file.puml>
#        echo '...' | generate-diagram.sh --pipe [--svg|--png|...]
#
# Wrapper around plantuml with input validation and output reporting.
# All plantuml flags are passed through.

show_usage() {
    echo "Usage: generate-diagram.sh [options] <file.puml>"
    echo "       echo '...' | generate-diagram.sh --pipe [options]"
    echo ""
    echo "Options are passed to plantuml. Common ones:"
    echo "  --svg        Generate SVG instead of PNG"
    echo "  --pdf        Generate PDF"
    echo "  --pipe, -p   Read from stdin, write to stdout"
    echo "  --check-syntax  Validate without generating"
    exit 1
}

# Detect pipe mode and format from args
pipe_mode=false
format="png"
files=()

for arg in "$@"; do
    case "$arg" in
        -p|--pipe) pipe_mode=true ;;
        --svg) format="svg" ;;
        --pdf) format="pdf" ;;
        --eps) format="eps" ;;
        --txt) format="txt" ;;
        --utxt) format="utxt" ;;
        -h|--help) show_usage ;;
    esac
    # Collect non-option args as potential files
    if [[ ! "$arg" =~ ^- ]]; then
        files+=("$arg")
    fi
done

if $pipe_mode; then
    # Pipe mode: stdin to stdout
    plantuml "$@"
else
    # File mode: validate inputs exist
    if [[ ${#files[@]} -eq 0 ]]; then
        echo "Error: No input file specified" >&2
        show_usage
    fi

    for file in "${files[@]}"; do
        if [[ ! -f "$file" ]]; then
            echo "Error: File not found: $file" >&2
            exit 1
        fi
    done

    # Run plantuml
    plantuml "$@"

    # Report generated files
    for file in "${files[@]}"; do
        base="${file%.*}"
        output="${base}.${format}"
        if [[ -f "$output" ]]; then
            echo "Generated: $output"
        fi
    done
fi
