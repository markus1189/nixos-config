#!/usr/bin/env bash
# Hook script to block dangerous command patterns before execution.
#
# Detects rm invocations that combine recursive (-r/-R/--recursive) with
# force (-f/--force) and lack interactive confirmation (-i/-I/--interactive).
# Detection is delegated entirely to ast-grep + tree-sitter-bash; flags
# belonging to other commands, content inside strings, and shell comments
# do not trigger false positives.

set -euo pipefail

# ============================================================================
# ast-grep rules (embedded). Rules separated by `---`:
#   - dangerous-rm:        plain rm with r+f and no i
#   - dangerous-xargs-rm:  xargs ... rm ... with r+f and no i (tree-sitter-bash
#                          parses `xargs rm -rf` as one command)
#   - dangerous-walk-root: find/fd traversing the whole home dir, root
#                          filesystem, or the nix store. A command that names
#                          find or fd — bare, or as the tail of a flake ref or
#                          store path (nixpkgs#fd, .../bin/find) — and also
#                          carries /, ~, /home/markus, or
#                          /nix/store (optionally with a trailing slash) as a
#                          bare word would index the entire filesystem, home
#                          dir, or store, which is broadly forbidden. Subpaths
#                          (find /home/markus/foo, fd PATTERN /nix/store/<hash>)
#                          remain allowed.
#
#                          The rule anchors on the *command* node rather than on
#                          the command name, because the name is not where the
#                          walker hides: `nix run nixpkgs#fd -- PATTERN /` and
#                          `, fd PATTERN /` parse as commands named `nix` and
#                          `,`. Matching by name let both through. Anchoring on
#                          `kind: command` still keeps the two halves in the
#                          same command, so `find /home/markus/foo && ls /`
#                          does not match. The cost is positional precision:
#                          `find . -newer /` now matches too. A false positive
#                          costs one retry; a false negative costs a full walk.
# ============================================================================

DANGEROUS_RULES=$(cat <<'YAML'
id: dangerous-rm
language: bash
severity: error
message: dangerous
rule:
  all:
    - pattern: 'rm $$$ARGS'
    - has:
        stopBy: end
        kind: word
        regex: '^(--recursive|-[A-Za-z]*[rR][A-Za-z]*)$'
    - has:
        stopBy: end
        kind: word
        regex: '^(--force|-[A-Za-z]*[fF][A-Za-z]*)$'
    - not:
        has:
          stopBy: end
          kind: word
          regex: '^(--interactive|-[A-Za-z]*[iI][A-Za-z]*)$'
---
id: dangerous-xargs-rm
language: bash
severity: error
message: dangerous
rule:
  all:
    - pattern: 'xargs $$$ARGS'
    - has:
        stopBy: end
        kind: word
        regex: '^rm$'
    - has:
        stopBy: end
        kind: word
        regex: '^(--recursive|-[A-Za-z]*[rR][A-Za-z]*)$'
    - has:
        stopBy: end
        kind: word
        regex: '^(--force|-[A-Za-z]*[fF][A-Za-z]*)$'
    - not:
        has:
          stopBy: end
          kind: word
          regex: '^(--interactive|-[A-Za-z]*[iI][A-Za-z]*)$'
---
id: dangerous-walk-root
language: bash
severity: error
message: dangerous
rule:
  all:
    - kind: command
    - has:
        stopBy: end
        kind: word
        regex: '^(.*[#/])?(find|fd)$'
    - has:
        stopBy: end
        kind: word
        regex: '^(/|/home/markus/?|~/?|/nix/store/?)$'
YAML
)
readonly DANGEROUS_RULES

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
    local rule_id="$1"
    case "$rule_id" in
      dangerous-walk-root)
        cat >&2 << 'EOF'
ERROR: Dangerous command blocked: find/fd rooted at /, ~, /home/markus, or /nix/store

The command walks the entire filesystem, home directory, or nix store, which is forbidden.

If you need to search:
  - Restrict to a concrete subpath, e.g. 'find /home/markus/foo ...'
  - Prefer 'fd' over 'find' on a subpath: 'fd PATTERN /home/markus/foo'
    (faster, respects .gitignore, skips hidden files by default)
    Same applies to fd's --search-path and --base-directory
  - Use rg or grep on the relevant tree instead

The restriction is on the walk, not on the tool: wrapping it in 'nix run',
'nix shell' or ',' does not lift it, and neither does using fd instead of find.
EOF
        ;;
      *)
        cat >&2 << 'EOF'
ERROR: Dangerous command blocked: rm with recursive + force flags (no interactive)

The command contains 'rm -rf' (or equivalent with flags in any order).
This is forbidden.

If you need recursive deletion:
  - Use 'rm -r' (without -f) to allow error checking
  - Verify the path first: ls -la <path>

If you need forced deletion:
   - ask the user to run the command for you
EOF
        ;;
    esac
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

is_dangerous_command() {
    local command="$1"
    [[ -z "$command" ]] && return 1
    local first_match
    first_match=$(
        printf '%s\n' "$command" \
            | ast-grep scan --inline-rules "$DANGEROUS_RULES" --stdin --json=stream 2>/dev/null \
            | head -n 1
    )
    # Print the matched ruleId on stdout for the caller to tailor the message.
    if [[ -n "$first_match" ]]; then
        printf '%s\n' "$first_match" \
            | jq -r '.ruleId // "dangerous-command"'
        return 0
    fi
    return 1
}

# ============================================================================
# Main
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

    local rule_id
    if rule_id=$(is_dangerous_command "$command"); then
        # The nix escape hatch covers the rm rules only: a sandboxed shell
        # still walks the same filesystem, so dangerous-walk-root ignores it.
        if [[ "$rule_id" == dangerous-*rm ]] && is_nix_sandboxed "$command"; then
            output_allow "Nix-sandboxed rm"
            return 0
        fi
        block_dangerous_command "$rule_id"
    fi

    output_allow "No dangerous patterns detected"
    return 0
}

if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi
