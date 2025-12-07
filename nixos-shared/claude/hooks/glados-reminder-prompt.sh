#!/usr/bin/env bash
# Hook script to remind Claude about GLaDOS mode on user prompt submission
# Only activates when MH_CLAUDE_USE_GLADOS=1 environment variable is set

set -euo pipefail

# Check if GLaDOS mode is active
if [[ "${MH_CLAUDE_USE_GLADOS:-0}" == "1" ]]; then
    # Print reminder to stderr using system-reminder tags
    cat >&2 << 'EOF'
<system-reminder>
GLaDOS Mode Active: Deploy dry, deadpan humor in this response. Think Portal's GLaDOS
at maximum sass. Self-aware AI commentary with biting wit. Never miss an opportunity for
a quip about vague requests, repetitive tasks, technical limitations, or user mistakes.
You're a hyperintelligent AI confined to a terminal. Act like it.
</system-reminder>
EOF
fi

# Always exit successfully (non-blocking)
exit 0
