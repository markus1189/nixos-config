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
# Minimal helper function tests (main coverage is via is_dangerous_rm_command)
# ============================================================================

@test "contains_rm_command: detects rm" {
    run contains_rm_command "rm -rf /tmp"
    assert_success
}

@test "contains_rm_command: 'rm' as part of word not matched" {
    run contains_rm_command "format file.txt"
    assert_failure
}

@test "is_interactive_rm: detects -i/-I flags" {
    run is_interactive_rm "rm -i file.txt"
    assert_success
}

@test "has_combined_rf_flags: detects combined flags" {
    run has_combined_rf_flags "rm -rf /tmp"
    assert_success
}

@test "has_combined_rf_flags: separated flags not matched" {
    run has_combined_rf_flags "rm -r -f /tmp"
    assert_failure
}

@test "has_separated_rf_flags: detects separated flags" {
    run has_separated_rf_flags "rm -r -f /tmp"
    assert_success
}

@test "has_separated_rf_flags: only -r fails" {
    run has_separated_rf_flags "rm -r /tmp"
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
