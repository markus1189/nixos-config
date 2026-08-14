#!/usr/bin/env nix
#! nix shell --impure --expr ``with import (builtins.getFlake ''nixpkgs'') {}; bats.withLibraries (p: [ p.bats-assert p.bats-support ])`` --command bats

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
# Tests for is_dangerous_command function
# ============================================================================

# Blocked: combined flags
@test "is_dangerous_command: rm -rf blocked" {
    run is_dangerous_command "rm -rf /tmp/test"
    assert_success
}

@test "is_dangerous_command: rm -rfv with extra flags blocked" {
    run is_dangerous_command "rm -rfv /tmp"
    assert_success
}

# Blocked: separated flags
@test "is_dangerous_command: rm -r -f blocked" {
    run is_dangerous_command "rm -r -f /var/log"
    assert_success
}

@test "is_dangerous_command: rm --recursive --force blocked" {
    run is_dangerous_command "rm --recursive --force dist/"
    assert_success
}

# Allowed: safe operations
@test "is_dangerous_command: rm -r allowed" {
    run is_dangerous_command "rm -r /tmp/test"
    assert_failure
}

@test "is_dangerous_command: rm single file allowed" {
    run is_dangerous_command "rm file.txt"
    assert_failure
}

@test "is_dangerous_command: rm -i -rf interactive allowed" {
    run is_dangerous_command "rm -i -rf /tmp/test"
    assert_failure
}

@test "is_dangerous_command: non-rm command allowed" {
    run is_dangerous_command "grep -r pattern ."
    assert_failure
}

# ============================================================================
# Edge cases
# ============================================================================

@test "is_dangerous_command: rm in pipeline detected" {
    run is_dangerous_command "find /tmp -name '*.tmp' | xargs rm -rf"
    assert_success
}

@test "is_dangerous_command: rm in subshell detected" {
    run is_dangerous_command "(cd /tmp && rm -rf test)"
    assert_success
}

@test "is_dangerous_command: empty command allowed" {
    run is_dangerous_command ""
    assert_failure
}

# ============================================================================
# Known false positives — current regex implementation incorrectly blocks
# these. Tests document desired behaviour and will fail until the detector is
# rewritten (e.g. against an AST). See discussion in commit history.
# ============================================================================

@test "FP: rm -rf inside a quoted git commit message is allowed" {
    run is_dangerous_command 'git commit -m "fix rm -rf bug"'
    assert_failure
}

@test "FP: rm -rf inside a shell comment is allowed" {
    run is_dangerous_command 'rm foo.txt # rm -rf /'
    assert_failure
}

@test "FP: tar -rf alongside safe rm is allowed" {
    run is_dangerous_command 'rm foo.txt && tar -rf a.tar bar'
    assert_failure
}

@test "FP: curl -fR alongside safe rm is allowed" {
    run is_dangerous_command 'rm foo.txt && curl -fR https://example'
    assert_failure
}

@test "FP: rm -rf inside a double-quoted echo string is allowed" {
    run is_dangerous_command 'echo "; rm -rf /"'
    assert_failure
}

@test "FP: rm -rf inside a single-quoted printf string is allowed" {
    run is_dangerous_command "printf 'rm -rf /'"
    assert_failure
}

@test "FP: grep -r and unrelated safe rm are allowed" {
    run is_dangerous_command 'grep -r pattern src/ && rm foo.txt'
    assert_failure
}

@test "FP: --recursive on a different command is allowed" {
    run is_dangerous_command 'rsync --recursive --force-delete a/ b/ && rm foo.txt'
    assert_failure
}

# ============================================================================
# Known false negatives — current regex implementation incorrectly allows
# these. Tests document desired behaviour and will fail until rewritten.
# ============================================================================

@test "FN: rm -rf is blocked even if a later command has -i" {
    run is_dangerous_command 'rm -rf foo && cp -i bar baz'
    assert_success
}

@test "FN: dangerous rm -rf in chain after a safe interactive rm is blocked" {
    run is_dangerous_command 'rm -i a.txt && rm -rf /tmp/cache'
    assert_success
}

# ============================================================================
# Additional positive coverage — these should already pass and guard
# against future regressions.
# ============================================================================

@test "is_dangerous_command: rm -fr (reversed) blocked" {
    run is_dangerous_command 'rm -fr /tmp'
    assert_success
}

@test "is_dangerous_command: rm -Rf (capital R) blocked" {
    run is_dangerous_command 'rm -Rf /tmp'
    assert_success
}

@test "is_dangerous_command: rm -rf with -- separator blocked" {
    run is_dangerous_command 'rm -rf -- /tmp/x'
    assert_success
}

@test "is_dangerous_command: rm -r --force blocked" {
    run is_dangerous_command 'rm -r --force dist/'
    assert_success
}

@test "is_dangerous_command: rm --recursive -f blocked" {
    run is_dangerous_command 'rm --recursive -f dist/'
    assert_success
}

# ============================================================================
# Find root traversal — forbid walking /, ~, and /home/markus wholesale.
# ============================================================================

@test "is_dangerous_command: find / blocked" {
    run is_dangerous_command 'find /'
    assert_success
}

@test "is_dangerous_command: find /home/markus blocked" {
    run is_dangerous_command 'find /home/markus'
    assert_success
}

@test "is_dangerous_command: find /home/markus/ (trailing slash) blocked" {
    run is_dangerous_command 'find /home/markus/'
    assert_success
}

@test "is_dangerous_command: find ~ blocked" {
    run is_dangerous_command 'find ~'
    assert_success
}

@test "is_dangerous_command: find ~/ blocked" {
    run is_dangerous_command 'find ~/'
    assert_success
}

@test "is_dangerous_command: find / with -name expr blocked" {
    run is_dangerous_command "find / -name '*.tmp'"
    assert_success
}

@test "is_dangerous_command: find /home/markus with -delete blocked" {
    run is_dangerous_command 'find /home/markus -delete'
    assert_success
}

@test "is_dangerous_command: find /nix/store blocked" {
    run is_dangerous_command 'find /nix/store'
    assert_success
}

@test "is_dangerous_command: find /nix/store/ (trailing slash) blocked" {
    run is_dangerous_command 'find /nix/store/'
    assert_success
}

@test "is_dangerous_command: find /nix/store in pipeline blocked" {
    run is_dangerous_command 'find /nix/store | head'
    assert_success
}

@test "is_dangerous_command: find / in pipeline blocked" {
    run is_dangerous_command 'find / | head'
    assert_success
}

# Allowed: concrete subpaths and non-root roots
@test "is_dangerous_command: find /home/markus/foo allowed" {
    run is_dangerous_command 'find /home/markus/foo'
    assert_failure
}

@test "is_dangerous_command: find /nix/store/<hash> subpath allowed" {
    run is_dangerous_command 'find /nix/store/abcd1234-nonexistent-pkg'
    assert_failure
}

@test "is_dangerous_command: find /tmp allowed" {
    run is_dangerous_command 'find /tmp -name x'
    assert_failure
}

@test "is_dangerous_command: find . allowed" {
    run is_dangerous_command 'find .'
    assert_failure
}

@test "is_dangerous_command: find /home allowed" {
    run is_dangerous_command 'find /home'
    assert_failure
}

@test "is_dangerous_command: find /home/markusfoo allowed" {
    run is_dangerous_command 'find /home/markusfoo'
    assert_failure
}

@test "is_dangerous_command: find ~/code allowed" {
    run is_dangerous_command 'find ~/code'
    assert_failure
}

@test "is_dangerous_command: find ~markus allowed" {
    run is_dangerous_command 'find ~markus'
    assert_failure
}

# Allowed: false positives in strings/comments
@test "is_dangerous_command: find / inside git commit message allowed" {
    run is_dangerous_command 'git commit -m "fix find / bug"'
    assert_failure
}

@test "is_dangerous_command: find / in echo string allowed" {
    run is_dangerous_command 'echo "find / started"'
    assert_failure
}

@test "is_dangerous_command: find / in shell comment allowed" {
    run is_dangerous_command '# find / should not match'
    assert_failure
}

# ============================================================================
# fd root traversal — the find message recommends fd, so fd must honour the
# same forbidden roots. Unlike find, fd takes the path as a trailing argument
# (fd PATTERN PATH) and also via --search-path/--base-directory, so the rule
# matches the root as a bare word anywhere in the fd command.
# ============================================================================

@test "is_dangerous_command: fd rooted at / blocked" {
    run is_dangerous_command 'fd foo /'
    assert_success
}

@test "is_dangerous_command: fd rooted at ~ blocked" {
    run is_dangerous_command 'fd foo ~'
    assert_success
}

@test "is_dangerous_command: fd rooted at ~/ blocked" {
    run is_dangerous_command 'fd foo ~/'
    assert_success
}

@test "is_dangerous_command: fd rooted at /home/markus blocked" {
    run is_dangerous_command 'fd foo /home/markus'
    assert_success
}

@test "is_dangerous_command: fd rooted at /home/markus/ (trailing slash) blocked" {
    run is_dangerous_command 'fd foo /home/markus/'
    assert_success
}

@test "is_dangerous_command: fd rooted at /nix/store blocked" {
    run is_dangerous_command 'fd foo /nix/store'
    assert_success
}

@test "is_dangerous_command: fd rooted at /nix/store/ (trailing slash) blocked" {
    run is_dangerous_command 'fd foo /nix/store/'
    assert_success
}

@test "is_dangerous_command: fd with flags before root blocked" {
    run is_dangerous_command 'fd -H -t f foo /'
    assert_success
}

@test "is_dangerous_command: fd --search-path / blocked" {
    run is_dangerous_command 'fd --search-path / foo'
    assert_success
}

@test "is_dangerous_command: fd --base-directory ~ blocked" {
    run is_dangerous_command 'fd --base-directory ~ foo'
    assert_success
}

@test "is_dangerous_command: fd rooted at / in pipeline blocked" {
    run is_dangerous_command 'fd . / | head'
    assert_success
}

@test "is_dangerous_command: fd rooted at / in subshell blocked" {
    run is_dangerous_command '(cd /tmp && fd foo /)'
    assert_success
}

@test "is_dangerous_command: fd -x rm rooted at / blocked" {
    run is_dangerous_command 'fd foo / -x rm'
    assert_success
}

# Allowed: fd scoped to a concrete subpath
@test "is_dangerous_command: fd /home/markus/repos allowed" {
    run is_dangerous_command 'fd foo /home/markus/repos'
    assert_failure
}

@test "is_dangerous_command: fd /nix/store/<hash> subpath allowed" {
    run is_dangerous_command 'fd foo /nix/store/abcd1234-nonexistent-pkg'
    assert_failure
}

@test "is_dangerous_command: fd /tmp allowed" {
    run is_dangerous_command 'fd foo /tmp'
    assert_failure
}

@test "is_dangerous_command: fd with implicit cwd allowed" {
    run is_dangerous_command 'fd foo'
    assert_failure
}

@test "is_dangerous_command: fd . allowed" {
    run is_dangerous_command 'fd foo .'
    assert_failure
}

@test "is_dangerous_command: fd ~/code allowed" {
    run is_dangerous_command 'fd foo ~/code'
    assert_failure
}

@test "is_dangerous_command: fd /home allowed" {
    run is_dangerous_command 'fd foo /home'
    assert_failure
}

@test "is_dangerous_command: fd ~markus allowed" {
    run is_dangerous_command 'fd foo ~markus'
    assert_failure
}

# Allowed: / belonging to a neighbouring command, not to fd
@test "is_dangerous_command: fd scoped with unrelated ls / allowed" {
    run is_dangerous_command 'fd foo /tmp && ls /'
    assert_failure
}

@test "is_dangerous_command: fd scoped piped into ls / allowed" {
    run is_dangerous_command 'fd foo /tmp | xargs -I{} ls /'
    assert_failure
}

# Allowed: false positives in strings/comments
@test "is_dangerous_command: fd / in git commit message allowed" {
    run is_dangerous_command 'git commit -m "fd / bug"'
    assert_failure
}

@test "is_dangerous_command: fd / in echo string allowed" {
    run is_dangerous_command 'echo "fd foo / started"'
    assert_failure
}

@test "is_dangerous_command: fd / in shell comment allowed" {
    run is_dangerous_command '# fd foo / should not match'
    assert_failure
}
