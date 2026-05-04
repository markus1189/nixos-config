#!/usr/bin/env nix
#! nix shell --impure --expr ``with import <nixpkgs> {}; bats.withLibraries (p: [ p.bats-assert p.bats-support ])`` --command bats

# Tests for check-dangerous-commands.sh

# Load bats-assert and bats-support libraries before each test
setup() {
    bats_load_library bats-support
    bats_load_library bats-assert

    # Source the hook script to get access to its functions
    source "$BATS_TEST_DIRNAME/check-dangerous-commands.sh"
}

# ============================================================================
# Input parsing tests
# ============================================================================

@test "parse_tool_name: extracts Bash tool" {
    local input='{"tool_name":"Bash","tool_input":{"command":"ls"}}'
    run parse_tool_name "$input"
    assert_success
    assert_output "Bash"
}

@test "parse_tool_name: extracts other tools" {
    local input='{"tool_name":"Read","tool_input":{"file_path":"/tmp/test"}}'
    run parse_tool_name "$input"
    assert_success
    assert_output "Read"
}

@test "parse_command: extracts simple command" {
    local input='{"tool_name":"Bash","tool_input":{"command":"ls -la"}}'
    run parse_command "$input"
    assert_success
    assert_output "ls -la"
}

@test "parse_command: extracts rm -rf command" {
    local input='{"tool_name":"Bash","tool_input":{"command":"rm -rf /tmp"}}'
    run parse_command "$input"
    assert_success
    assert_output "rm -rf /tmp"
}

@test "parse_command: handles missing command field" {
    local input='{"tool_name":"Bash","tool_input":{}}'
    run parse_command "$input"
    assert_success
    assert_output ""
}

@test "parse_command: handles non-Bash tool without command" {
    local input='{"tool_name":"Read","tool_input":{"file_path":"/tmp/test"}}'
    run parse_command "$input"
    assert_success
    assert_output ""
}

# ============================================================================
# Helper function tests for the AST-based detector
# ============================================================================

@test "analyze_flag_tokens: combined -rf is dangerous" {
    run analyze_flag_tokens -rf /tmp
    assert_success
}

@test "analyze_flag_tokens: separated -r -f is dangerous" {
    run analyze_flag_tokens -r -f /tmp
    assert_success
}

@test "analyze_flag_tokens: long --recursive --force is dangerous" {
    run analyze_flag_tokens --recursive --force /tmp
    assert_success
}

@test "analyze_flag_tokens: -i present cancels danger" {
    run analyze_flag_tokens -i -rf /tmp
    assert_failure
}

@test "analyze_flag_tokens: combined -rfi cancels danger" {
    run analyze_flag_tokens -rfi /tmp
    assert_failure
}

@test "analyze_flag_tokens: only -r is safe" {
    run analyze_flag_tokens -r /tmp
    assert_failure
}

@test "analyze_flag_tokens: bare path is safe" {
    run analyze_flag_tokens file.txt
    assert_failure
}

@test "analyze_flag_tokens: -- ends option scanning" {
    run analyze_flag_tokens -rf -- /tmp
    assert_success
}

@test "is_dangerous_rm_match: rm -rf foo dangerous" {
    run is_dangerous_rm_match "rm -rf foo"
    assert_success
}

@test "is_dangerous_rm_match: rm -i -rf foo safe" {
    run is_dangerous_rm_match "rm -i -rf foo"
    assert_failure
}

@test "is_dangerous_xargs_match: xargs rm -rf dangerous" {
    run is_dangerous_xargs_match "xargs rm -rf"
    assert_success
}

@test "is_dangerous_xargs_match: xargs -I {} rm -rf {} dangerous" {
    run is_dangerous_xargs_match "xargs -I {} rm -rf {}"
    assert_success
}

@test "is_dangerous_xargs_match: xargs -r echo not dangerous" {
    run is_dangerous_xargs_match "xargs -r echo"
    assert_failure
}

# ============================================================================
# Tests for is_dangerous_rm_command function
# ============================================================================

# Blocked: combined flags
@test "is_dangerous_rm_command: rm -rf blocked" {
    run is_dangerous_rm_command "rm -rf /tmp/test"
    assert_success
}

@test "is_dangerous_rm_command: rm -rfv with extra flags blocked" {
    run is_dangerous_rm_command "rm -rfv /tmp"
    assert_success
}

# Blocked: separated flags
@test "is_dangerous_rm_command: rm -r -f blocked" {
    run is_dangerous_rm_command "rm -r -f /var/log"
    assert_success
}

@test "is_dangerous_rm_command: rm --recursive --force blocked" {
    run is_dangerous_rm_command "rm --recursive --force dist/"
    assert_success
}

# Allowed: safe operations
@test "is_dangerous_rm_command: rm -r allowed" {
    run is_dangerous_rm_command "rm -r /tmp/test"
    assert_failure
}

@test "is_dangerous_rm_command: rm single file allowed" {
    run is_dangerous_rm_command "rm file.txt"
    assert_failure
}

@test "is_dangerous_rm_command: rm -i -rf interactive allowed" {
    run is_dangerous_rm_command "rm -i -rf /tmp/test"
    assert_failure
}

@test "is_dangerous_rm_command: non-rm command allowed" {
    run is_dangerous_rm_command "grep -r pattern ."
    assert_failure
}

# ============================================================================
# Edge cases
# ============================================================================

@test "is_dangerous_rm_command: rm in pipeline detected" {
    run is_dangerous_rm_command "find /tmp -name '*.tmp' | xargs rm -rf"
    assert_success
}

@test "is_dangerous_rm_command: rm in subshell detected" {
    run is_dangerous_rm_command "(cd /tmp && rm -rf test)"
    assert_success
}

@test "is_dangerous_rm_command: empty command allowed" {
    run is_dangerous_rm_command ""
    assert_failure
}

# ============================================================================
# Known false positives — current regex implementation incorrectly blocks
# these. Tests document desired behaviour and will fail until the detector is
# rewritten (e.g. against an AST). See discussion in commit history.
# ============================================================================

@test "FP: rm -rf inside a quoted git commit message is allowed" {
    run is_dangerous_rm_command 'git commit -m "fix rm -rf bug"'
    assert_failure
}

@test "FP: rm -rf inside a shell comment is allowed" {
    run is_dangerous_rm_command 'rm foo.txt # rm -rf /'
    assert_failure
}

@test "FP: tar -rf alongside safe rm is allowed" {
    run is_dangerous_rm_command 'rm foo.txt && tar -rf a.tar bar'
    assert_failure
}

@test "FP: curl -fR alongside safe rm is allowed" {
    run is_dangerous_rm_command 'rm foo.txt && curl -fR https://example'
    assert_failure
}

@test "FP: rm -rf inside a double-quoted echo string is allowed" {
    run is_dangerous_rm_command 'echo "; rm -rf /"'
    assert_failure
}

@test "FP: rm -rf inside a single-quoted printf string is allowed" {
    run is_dangerous_rm_command "printf 'rm -rf /'"
    assert_failure
}

@test "FP: grep -r and unrelated safe rm are allowed" {
    run is_dangerous_rm_command 'grep -r pattern src/ && rm foo.txt'
    assert_failure
}

@test "FP: --recursive on a different command is allowed" {
    run is_dangerous_rm_command 'rsync --recursive --force-delete a/ b/ && rm foo.txt'
    assert_failure
}

# ============================================================================
# Known false negatives — current regex implementation incorrectly allows
# these. Tests document desired behaviour and will fail until rewritten.
# ============================================================================

@test "FN: rm -rf is blocked even if a later command has -i" {
    run is_dangerous_rm_command 'rm -rf foo && cp -i bar baz'
    assert_success
}

@test "FN: dangerous rm -rf in chain after a safe interactive rm is blocked" {
    run is_dangerous_rm_command 'rm -i a.txt && rm -rf /tmp/cache'
    assert_success
}

# ============================================================================
# Additional positive coverage — these should already pass and guard
# against future regressions.
# ============================================================================

@test "is_dangerous_rm_command: rm -fr (reversed) blocked" {
    run is_dangerous_rm_command 'rm -fr /tmp'
    assert_success
}

@test "is_dangerous_rm_command: rm -Rf (capital R) blocked" {
    run is_dangerous_rm_command 'rm -Rf /tmp'
    assert_success
}

@test "is_dangerous_rm_command: rm -rf with -- separator blocked" {
    run is_dangerous_rm_command 'rm -rf -- /tmp/x'
    assert_success
}

@test "is_dangerous_rm_command: rm -r --force blocked" {
    run is_dangerous_rm_command 'rm -r --force dist/'
    assert_success
}

@test "is_dangerous_rm_command: rm --recursive -f blocked" {
    run is_dangerous_rm_command 'rm --recursive -f dist/'
    assert_success
}
