#!/usr/bin/env bash
# Hook script to check if python/python3 commands are available in PATH
# before allowing Claude Code to execute them.
#
# If python command is not found, blocks execution and suggests NixOS alternatives.

set -euo pipefail

# Read hook input from stdin
INPUT=$(cat)

# Extract tool name and command
TOOL_NAME=$(echo "$INPUT" | jq -r '.tool_name')
COMMAND=$(echo "$INPUT" | jq -r '.tool_input.command // ""')

# Only process Bash tool calls
if [[ "$TOOL_NAME" != "Bash" ]]; then
    # Allow other tools to pass through
    cat << 'EOF'
{
  "hookSpecificOutput": {
    "hookEventName": "PreToolUse",
    "permissionDecision": "allow",
    "permissionDecisionReason": "Non-Bash tool"
  }
}
EOF
    exit 0
fi

# Skip check if command uses Nix-based python invocation
# These patterns are already using NixOS best practices
if [[ "$COMMAND" =~ ^[[:space:]]*(,|nix[[:space:]]+(run|shell)|nix-shell) ]]; then
    cat << 'EOF'
{
  "hookSpecificOutput": {
    "hookEventName": "PreToolUse",
    "permissionDecision": "allow",
    "permissionDecisionReason": "Using Nix-based invocation"
  }
}
EOF
    exit 0
fi

# Check if command contains python or python3
# Match python/python3 as a standalone command (with word boundaries)
if [[ "$COMMAND" =~ (^|[[:space:]\|&;(])(python3?|python)([[:space:]\|&;)]|$) ]]; then
    # Extract the actual python command (python or python3)
    PYTHON_CMD=""
    if [[ "$COMMAND" =~ python3 ]]; then
        PYTHON_CMD="python3"
    elif [[ "$COMMAND" =~ python ]]; then
        PYTHON_CMD="python"
    fi

    # Check if the command is available in PATH
    if ! command -v "$PYTHON_CMD" >/dev/null 2>&1; then
        # Command not found - block with error message
        cat >&2 << EOF
ERROR: '$PYTHON_CMD' command not found in PATH.

On NixOS, use one of these alternatives:
  - , $PYTHON_CMD          (comma runner - temporary shell)
  - nix run nixpkgs#$PYTHON_CMD
  - nix-shell -p $PYTHON_CMD
EOF
        exit 2  # Exit code 2 blocks the command
    fi
fi

# Command is safe to execute or doesn't contain python
cat << 'EOF'
{
  "hookSpecificOutput": {
    "hookEventName": "PreToolUse",
    "permissionDecision": "allow",
    "permissionDecisionReason": "Python command available in PATH or not needed"
  }
}
EOF
exit 0
