#!/usr/bin/env nix
#! nix shell --impure --expr ``with import (builtins.getFlake ''nixpkgs'') {}; [ (bats.withLibraries (p: [ p.bats-assert p.bats-support ])) python3 jq ]`` --command bats

# Tests for agent-sound.py: which sound each hook event picks.
# AGENT_SOUND_DRY_RUN prints the sound file name instead of playing it.

setup() {
    bats_load_library bats-support
    bats_load_library bats-assert
    export AGENT_SOUND_DRY_RUN=1
}

# sound_for EVENT_JSON
sound_for() {
    python3 "$BATS_TEST_DIRNAME/agent-sound.py" <<<"$1"
}

# bash_sound COMMAND
bash_sound() {
    sound_for "$(jq -nc --arg c "$1" \
        '{hook_event_name: "PreToolUse", tool_name: "Bash", tool_input: {command: $c}}')"
}

tool_sound() {
    sound_for "{\"hook_event_name\":\"PreToolUse\",\"tool_name\":\"$1\",\"tool_input\":{}}"
}

# ============================================================================
# Dedicated tools
# ============================================================================

@test "tool: Read plays the read sound" {
    run tool_sound Read
    assert_output "come-here-notification.wav"
}

@test "tool: Edit plays the mutate sound" {
    run tool_sound Edit
    assert_output "intuition-561.wav"
}

@test "tool: Skill plays the skill sound" {
    run tool_sound Skill
    assert_output "graceful-285.wav"
}

@test "tool: AskUserQuestion plays your-turn" {
    run tool_sound AskUserQuestion
    assert_output "your-turn-491.wav"
}

@test "tool: MCP tools are no longer silent" {
    run tool_sound mcp__claude_ai_Google_Calendar__list_events
    assert_output "happy-to-help-notification-sound.wav"
}

@test "tool: unknown tools get the generic sound" {
    run tool_sound ToolSearch
    assert_output "happy-to-help-notification-sound.wav"
}

# ============================================================================
# Bash classification
# ============================================================================

@test "bash: reads sound like Read" {
    run bash_sound "rg -n foo src | head -20"
    assert_output "come-here-notification.wav"
}

@test "bash: git read subcommands are reads" {
    run bash_sound "git -C ~/repo log --oneline -5 && git status --short"
    assert_output "come-here-notification.wav"
}

@test "bash: curl GET is a read" {
    run bash_sound "curl -sL https://example.com | pandoc -f html -t gfm"
    assert_output "come-here-notification.wav"
}

@test "bash: curl POST queries stay reads" {
    run bash_sound "curl -s -X POST https://overpass-api.de/api/interpreter --data-urlencode 'data=[out:json]'"
    assert_output "come-here-notification.wav"
}

@test "bash: command -v is a read" {
    run bash_sound "command -v sox"
    assert_output "come-here-notification.wav"
}

@test "bash: line continuations don't split the command" {
    run bash_sound $'rg -n \\\n  --glob "*.nix" foo'
    assert_output "come-here-notification.wav"
}

@test "bash: ddgr is research" {
    run bash_sound "ddgr --unsafe --json --noua 'claude code hooks'"
    assert_output "happy-to-help-notification-sound.wav"
}

@test "bash: nix build is a build" {
    run bash_sound "cd ~/repos/nixos-config && nix build --no-link .#checks.x86_64-linux.statix"
    assert_output "sly-user-interface-sound.wav"
}

@test "bash: nh os build is a build" {
    run bash_sound "nh os build"
    assert_output "sly-user-interface-sound.wav"
}

@test "bash: nix run classifies the wrapped program" {
    run bash_sound "nix run nixpkgs#ripgrep -- -n foo"
    assert_output "come-here-notification.wav"
}

@test "bash: nix shell --command classifies the wrapped program" {
    run bash_sound "nix shell nixpkgs#jq --command jq . x.json"
    assert_output "come-here-notification.wav"
}

@test "bash: bash -c classifies its argument" {
    run bash_sound "timeout 20 bash -c 'until grep -q DONE log; do sleep 1; done'"
    assert_output "come-here-notification.wav"
}

@test "bash: unknown programs stay mutations" {
    run bash_sound "python3 script.py"
    assert_output "intuition-561.wav"
}

@test "bash: sed -i is a mutation" {
    run bash_sound "sed -i 's/a/b/' file.txt"
    assert_output "intuition-561.wav"
}

@test "bash: redirecting into a file is a mutation" {
    run bash_sound "rg foo > hits.txt"
    assert_output "intuition-561.wav"
}

@test "bash: redirecting to /dev/null stays a read" {
    run bash_sound "rg foo 2>/dev/null >/dev/null"
    assert_output "come-here-notification.wav"
}

@test "bash: heredoc bodies are not commands" {
    run bash_sound $'cat <<\'EOF\'\ngit push\nEOF'
    assert_output "come-here-notification.wav"
}

@test "bash: quoted text is not a command" {
    run bash_sound "echo 'remember to git push' | wc -c"
    assert_output "come-here-notification.wav"
}

@test "bash: git push is outward" {
    run bash_sound "git add -A && git commit -m x && git push -u origin main"
    assert_output "communication-channel-519.wav"
}

@test "bash: gh pr create is outward" {
    run bash_sound "gh pr create --draft --repo NixOS/nixpkgs --title x"
    assert_output "communication-channel-519.wav"
}

@test "bash: gh pr view is a read" {
    run bash_sound "gh pr view 123 --repo NixOS/nixpkgs --json state"
    assert_output "come-here-notification.wav"
}

@test "bash: gh api with fields is outward" {
    run bash_sound "gh api repos/o/r/issues/1/comments -f body=hi"
    assert_output "communication-channel-519.wav"
}

@test "bash: gh api -X GET with fields is a read" {
    run bash_sound "gh api -X GET search/issues -f q=foo"
    assert_output "come-here-notification.wav"
}

@test "bash: curl PUT is outward" {
    run bash_sound "curl -sf -X PUT -d @x.json https://api.example.com/thing"
    assert_output "communication-channel-519.wav"
}

# ============================================================================
# Other events
# ============================================================================

@test "notification: permission prompt plays your-turn" {
    run sound_for '{"hook_event_name":"Notification","notification_type":"permission_prompt"}'
    assert_output "your-turn-491.wav"
}

@test "notification: idle prompt keeps the soft sound" {
    run sound_for '{"hook_event_name":"Notification","notification_type":"idle_prompt"}'
    assert_output "just-maybe-577.wav"
}

@test "session start: clear and compact differ from startup" {
    run sound_for '{"hook_event_name":"SessionStart","source":"startup"}'
    assert_output "involved-notification.wav"
    run sound_for '{"hook_event_name":"SessionStart","source":"clear"}'
    assert_output "pull-out-551.wav"
    run sound_for '{"hook_event_name":"SessionStart","source":"compact"}'
    assert_output "hollow-582.wav"
}

@test "stop and subagent stop" {
    run sound_for '{"hook_event_name":"Stop"}'
    assert_output "for-sure-576.wav"
    run sound_for '{"hook_event_name":"SubagentStop"}'
    assert_output "time-is-now-585.wav"
}

@test "garbage input plays nothing and exits cleanly" {
    run sound_for 'not json'
    assert_success
    assert_output ""
}
