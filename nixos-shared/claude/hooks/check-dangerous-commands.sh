#!/usr/bin/env bash
# Hook script to block dangerous command patterns before execution.
#
# Detects rm invocations that combine recursive (-r/-R/--recursive) with
# force (-f/--force) and lack interactive confirmation (-i/-I/--interactive).
#
# Detection is AST-based: the command is parsed by ast-grep + tree-sitter-bash
# so flags belonging to other commands, content inside strings, and shell
# comments do not trigger false positives.

set -euo pipefail

# ============================================================================
# Output formatting
# ============================================================================

output_allow() {
    local reason="$1"
    cat << EOF
{
  "hookSpecificOutput": {
    "hookEventName": "PreToolUse",
    "permissionDecision": "allow",
    "permissionDecisionReason": "$reason"
  }
}
EOF
}

block_dangerous_command() {
    local reason="$1"
    cat >&2 << EOF
ERROR: Dangerous command blocked: $reason

The command contains 'rm -rf' (or equivalent with flags in any order).
This is forbidden.

If you need recursive deletion:
  - Use 'rm -r' (without -f) to allow error checking
  - Verify the path first: ls -la <path>

If you need forced deletion:
   - ask the user to run the command for you
EOF
    exit 2
}

# ============================================================================
# Input parsing
# ============================================================================

parse_tool_name() {
    local input="$1"
    echo "$input" | jq -r '.tool_name'
}

parse_command() {
    local input="$1"
    echo "$input" | jq -r '.tool_input.command // ""'
}

# ============================================================================
# Detection
# ============================================================================

is_nix_sandboxed() {
    local command="$1"
    [[ "$command" =~ ^[[:space:]]*(,|nix[[:space:]]+(run|shell)|nix-shell) ]]
}

# Inspect a flat token stream of rm-style flags. Returns 0 (dangerous) when
# both a recursive and a force flag are present and no interactive flag is.
analyze_flag_tokens() {
    local has_r=0 has_f=0 has_i=0
    local tok
    for tok in "$@"; do
        case "$tok" in
            --) break ;;
            --recursive)   has_r=1 ;;
            --force)       has_f=1 ;;
            --interactive) has_i=1 ;;
            --*) ;;
            -?*)
                local rest="${tok#-}"
                local i ch
                for ((i=0; i<${#rest}; i++)); do
                    ch="${rest:i:1}"
                    case "$ch" in
                        r|R) has_r=1 ;;
                        f|F) has_f=1 ;;
                        i|I) has_i=1 ;;
                    esac
                done
                ;;
        esac
    done
    (( has_r && has_f && ! has_i ))
}

# Decide whether a single rm match (e.g. "rm -rf foo") is dangerous.
is_dangerous_rm_match() {
    local match_text="$1"
    local rest="${match_text#rm}"
    local -a tokens=()
    # shellcheck disable=SC2206  # intentional word-splitting on the rm args
    tokens=( $rest )
    analyze_flag_tokens "${tokens[@]}"
}

# Decide whether an xargs match (e.g. "xargs -I {} rm -rf {}") embeds a
# dangerous rm. tree-sitter-bash parses `xargs rm -rf` as a single command,
# so we have to handle this shape separately.
is_dangerous_xargs_match() {
    local match_text="$1"
    local rest="${match_text#xargs}"
    local -a tokens=()
    # shellcheck disable=SC2206
    tokens=( $rest )
    local i
    for ((i=0; i<${#tokens[@]}; i++)); do
        if [[ "${tokens[i]}" == "rm" ]]; then
            analyze_flag_tokens "${tokens[@]:i+1}"
            return $?
        fi
    done
    return 1
}

# Run ast-grep against the command text using the given pattern; emit one
# matched node text per line.
extract_matches() {
    local command="$1"
    local pattern="$2"
    printf '%s\n' "$command" \
        | ast-grep --lang bash --pattern "$pattern" --stdin --json=stream 2>/dev/null \
        | jq -r '.text'
}

is_dangerous_rm_command() {
    local command="$1"
    [[ -z "$command" ]] && return 1

    local match
    while IFS= read -r match; do
        [[ -z "$match" ]] && continue
        if is_dangerous_rm_match "$match"; then
            return 0
        fi
    done < <(extract_matches "$command" 'rm $$$')

    while IFS= read -r match; do
        [[ -z "$match" ]] && continue
        if is_dangerous_xargs_match "$match"; then
            return 0
        fi
    done < <(extract_matches "$command" 'xargs $$$')

    return 1
}

# ============================================================================
# Main hook entry point
# ============================================================================

main() {
    local input
    input=$(cat)

    local tool_name command
    tool_name=$(parse_tool_name "$input")
    command=$(parse_command "$input")

    if [[ "$tool_name" != "Bash" ]]; then
        output_allow "Non-Bash tool"
        return 0
    fi

    if [[ -z "$command" ]]; then
        output_allow "Empty command"
        return 0
    fi

    if is_nix_sandboxed "$command"; then
        output_allow "Nix-sandboxed invocation"
        return 0
    fi

    if is_dangerous_rm_command "$command"; then
        block_dangerous_command "rm with recursive + force flags (no interactive)"
    fi

    output_allow "No dangerous patterns detected"
    return 0
}

if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi
