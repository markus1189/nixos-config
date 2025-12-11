#!/usr/bin/env bash
# Hook script to block dangerous command patterns before execution.
#
# Primarily targets rm -rf and its variations to prevent accidental
# destructive operations.

set -euo pipefail

# ============================================================================
# Helper Functions - Output formatting
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
# Input Parsing Functions - Unit testable
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
# Detection Functions - Unit testable
# ============================================================================

is_nix_sandboxed() {
    local command="$1"
    [[ "$command" =~ ^[[:space:]]*(,|nix[[:space:]]+(run|shell)|nix-shell) ]]
}

contains_rm_command() {
    local command="$1"
    [[ "$command" =~ (^|[[:space:]|&;\(])rm([[:space:]]|$) ]]
}

is_interactive_rm() {
    local command="$1"
    # Match -i or -I flags (with or without other letters like -rfi)
    # Must be preceded by whitespace to avoid matching '--recursive'
    [[ "$command" =~ [[:space:]]-[a-zA-Z]*[iI][a-zA-Z]*([[:space:]]|$) ]]
}

has_combined_rf_flags() {
    local command="$1"
    [[ "$command" =~ (^|[[:space:]|&;\(])rm[[:space:]]+-[a-zA-Z]*[rR][a-zA-Z]*[fF] ]] || \
    [[ "$command" =~ (^|[[:space:]|&;\(])rm[[:space:]]+-[a-zA-Z]*[fF][a-zA-Z]*[rR] ]]
}

has_recursive_flag() {
    local command="$1"
    [[ "$command" =~ -[a-zA-Z]*[rR] ]] || [[ "$command" =~ --recursive ]]
}

has_force_flag() {
    local command="$1"
    [[ "$command" =~ -[a-zA-Z]*[fF] ]] || [[ "$command" =~ --force ]]
}

has_separated_rf_flags() {
    local command="$1"
    has_recursive_flag "$command" && has_force_flag "$command"
}

is_dangerous_rm_command() {
    local command="$1"

    # Must contain rm
    if ! contains_rm_command "$command"; then
        return 1
    fi

    # Interactive rm is safe
    if is_interactive_rm "$command"; then
        return 1
    fi

    # Check for dangerous patterns
    if has_combined_rf_flags "$command"; then
        return 0
    fi

    if has_separated_rf_flags "$command"; then
        return 0
    fi

    return 1
}

# ============================================================================
# Main Hook Logic
# ============================================================================

main() {
    # Read hook input from stdin
    local input
    input=$(cat)

    # Extract tool name and command
    local tool_name
    local command
    tool_name=$(parse_tool_name "$input")
    command=$(parse_command "$input")

    # Only process Bash tool calls
    if [[ "$tool_name" != "Bash" ]]; then
        output_allow "Non-Bash tool"
        return 0
    fi

    # Skip check if command is empty
    if [[ -z "$command" ]]; then
        output_allow "Empty command"
        return 0
    fi

    # Skip check if command uses Nix-based invocation
    if is_nix_sandboxed "$command"; then
        output_allow "Nix-sandboxed invocation"
        return 0
    fi

    # Check if command is dangerous
    if is_dangerous_rm_command "$command"; then
        if has_combined_rf_flags "$command"; then
            block_dangerous_command "rm with combined -rf flags"
        else
            block_dangerous_command "rm with separated -r and -f flags"
        fi
    fi

    # No rm command present
    if ! contains_rm_command "$command"; then
        output_allow "No rm command present"
        return 0
    fi

    # Command is safe to execute
    output_allow "No dangerous patterns detected"
    return 0
}

# Run main function if script is executed (not sourced for testing)
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi
