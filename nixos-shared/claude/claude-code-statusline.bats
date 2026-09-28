#!/usr/bin/env nix
#! nix shell --impure --expr ``with import (builtins.getFlake ''nixpkgs'') {}; bats.withLibraries (p: [ p.bats-assert p.bats-support ])`` --command bats

# Tests for claude-code-statusline.sh

# Load bats-assert and bats-support libraries before each test
setup() {
    bats_load_library bats-support
    bats_load_library bats-assert

    # Source the statusline script to get access to its functions
    source "$BATS_TEST_DIRNAME/claude-code-statusline.sh"

    # Ensure deterministic state: bats inherits the parent shell env, and
    # this var is set in the user's normal shell.
    unset CLAUDE_CODE_ENABLE_TELEMETRY
}

# Test fixture helpers
mock_input_basic() {
    cat <<'EOF'
{
  "model": {"display_name": "@bedrock/eu.anthropic.claude-sonnet-4-5-20250929-v1:0"},
  "output_style": {"name": "default"},
  "version": "1.2.3",
  "workspace": {"project_dir": "/home/user/project"},
  "cost": {"total_cost_usd": 1.2345},
  "context_window": {
    "current_usage": {
      "input_tokens": 30000,
      "cache_creation_input_tokens": 10000,
      "cache_read_input_tokens": 10000
    },
    "total_input_tokens": 50000,
    "context_window_size": 200000
  },
  "transcript_path": "/path/to/abc123-timestamp.jsonl"
}
EOF
}

mock_input_high_usage() {
    cat <<'EOF'
{
  "model": {"display_name": "claude-opus-4"},
  "output_style": {"name": "concise"},
  "context_window": {
    "current_usage": {
      "input_tokens": 100000,
      "cache_creation_input_tokens": 30000,
      "cache_read_input_tokens": 20000
    },
    "total_input_tokens": 150000,
    "context_window_size": 200000
  },
  "transcript_path": "/path/to/xyz789-timestamp.jsonl"
}
EOF
}

# Tests for shorten_bedrock_model function

@test "shorten_bedrock_model: Sonnet 4.5 Bedrock model" {
    run shorten_bedrock_model "@bedrock/eu.anthropic.claude-sonnet-4-5-20250929-v1:0"
    assert_success
    assert_output "sonnet-4.5"
}

@test "shorten_bedrock_model: Haiku 3.5 Bedrock model" {
    run shorten_bedrock_model "@bedrock/us.anthropic.claude-haiku-3-5-20250101-v1:0"
    assert_success
    assert_output "haiku-3.5"
}

@test "shorten_bedrock_model: Opus 4.5 Bedrock model" {
    run shorten_bedrock_model "@bedrock/eu.anthropic.claude-opus-4-5-20251101-v1"
    assert_success
    assert_output "opus-4.5"
}

@test "shorten_bedrock_model: Non-Bedrock model unchanged" {
    run shorten_bedrock_model "claude-opus-4"
    assert_success
    assert_output "claude-opus-4"
}

# Tests for get_model_name function

@test "get_model_name: Bedrock model shortened without style" {
    input=$(mock_input_basic)
    run get_model_name
    assert_success
    # May have indicator suffixes (🔑 for requesty, 🪨 for bedrock)
    assert_regex "$output" '^sonnet-4\.5'
}

@test "get_model_name: Model with output style appended" {
    input=$(mock_input_high_usage)
    run get_model_name
    assert_success
    # May have indicator suffixes, but must start with model (style)
    assert_regex "$output" '^claude-opus-4 \(concise\)'
}

@test "get_model_name: Bedrock indicator when CLAUDE_CODE_USE_BEDROCK is set" {
    input=$(mock_input_basic)
    unset ANTHROPIC_BASE_URL
    export CLAUDE_CODE_USE_BEDROCK=1
    run get_model_name
    assert_success
    assert_output "sonnet-4.5🪨"
}

@test "get_model_name: Requesty indicator when ANTHROPIC_BASE_URL is requesty" {
    input=$(mock_input_basic)
    unset CLAUDE_CODE_USE_BEDROCK
    export ANTHROPIC_BASE_URL="https://router.eu.requesty.ai"
    run get_model_name
    assert_success
    assert_output "sonnet-4.5🔑"
}

# Tests for get_context_size function

@test "get_context_size: Valid token count" {
    input=$(mock_input_basic)
    run get_context_size
    assert_success
    assert_output "50000"
}

@test "get_context_size: Missing tokens returns placeholder" {
    input='{"context_window": {}}'
    run get_context_size
    assert_success
    assert_output "⌀"
}

# Tests for get_context_with_bar function

@test "get_context_with_bar: 50k tokens shows 2 filled dots" {
    input=$(mock_input_basic)
    run get_context_with_bar
    assert_success
    assert_output "50.0kt/200k 25%[●●○○○○○○○○]"
}

@test "get_context_with_bar: Missing data shows empty bar" {
    input='{"context_window": {}}'
    run get_context_with_bar
    assert_success
    assert_output "⌀[○○○○○○○○○○]"
}

# Tests for get_context_color function

@test "get_context_color: Low usage (25%) is green" {
    input=$(mock_input_basic)  # 50k/200k = 25%
    run get_context_color
    assert_success
    assert_output "120;220;120"
}

@test "get_context_color: High usage (75%) is red" {
    input=$(mock_input_high_usage)  # 150k/200k = 75%
    run get_context_color
    assert_success
    assert_output "255;120;120"
}

# Tests for get_cost function

@test "get_cost: Round to 2 decimal places" {
    input=$(mock_input_basic)
    run get_cost
    assert_success
    assert_output "1.23"
}

# Tests for get_transcript_id function

@test "get_transcript_id: Extract ID from path" {
    input=$(mock_input_basic)
    run get_transcript_id
    assert_success
    assert_output "abc123"
}

# Tests for get_context_window_size
#
# Regression: this used to render total_input_tokens as a third copy of the
# context size; the window itself was never shown.

@test "get_context_window_size: 200k window" {
    input=$(mock_input_basic)
    run get_context_window_size
    assert_success
    assert_output "200k"
}

@test "get_context_window_size: 1M window" {
    input='{"context_window": {"context_window_size": 1000000}}'
    run get_context_window_size
    assert_success
    assert_output "1M"
}

@test "get_context_window_size: fractional millions keep one decimal" {
    input='{"context_window": {"context_window_size": 1500000}}'
    run get_context_window_size
    assert_success
    assert_output "1.5M"
}

@test "get_context_window_size: missing field yields empty string" {
    input='{"context_window": {}}'
    run get_context_window_size
    assert_success
    assert_output ""
}

# Tests for the percentage inside the context segment

@test "get_context_with_bar: native percentage rounded" {
    input='{"context_window": {"used_percentage": 42.6}}'
    run get_context_with_bar
    assert_success
    assert_output "⌀ 43%[●●●●○○○○○○]"
}

@test "get_context_with_bar: null percentage and no tokens show placeholder only" {
    input='{"context_window": {"used_percentage": null}}'
    run get_context_with_bar
    assert_success
    assert_output "⌀[○○○○○○○○○○]"
}

@test "get_context_with_bar: 1M window" {
    input='{"context_window": {"current_usage": {"input_tokens": 62036}, "context_window_size": 1000000, "used_percentage": 6}}'
    run get_context_with_bar
    assert_success
    assert_output "62.0kt/1M 6%[○○○○○○○○○○]"
}

# Tests for get_model_name effort and thinking extensions

@test "get_model_name: effort level appended as text" {
    input='{"model": {"display_name": "opus"}, "effort": {"level": "high"}}'
    unset CLAUDE_CODE_USE_BEDROCK ANTHROPIC_BASE_URL
    run get_model_name
    assert_success
    assert_output "opus (high)"
}

@test "get_model_name: effort level absent when field missing" {
    input='{"model": {"display_name": "opus"}}'
    unset CLAUDE_CODE_USE_BEDROCK ANTHROPIC_BASE_URL
    run get_model_name
    assert_success
    assert_output "opus"
}

@test "get_model_name: thinking enabled appends brain emoji" {
    input='{"model": {"display_name": "opus"}, "thinking": {"enabled": true}}'
    unset CLAUDE_CODE_USE_BEDROCK ANTHROPIC_BASE_URL
    run get_model_name
    assert_success
    assert_output "opus🧠"
}

@test "get_model_name: thinking disabled omits brain emoji" {
    input='{"model": {"display_name": "opus"}, "thinking": {"enabled": false}}'
    unset CLAUDE_CODE_USE_BEDROCK ANTHROPIC_BASE_URL
    run get_model_name
    assert_success
    assert_output "opus"
}

@test "get_model_name: style, effort, and thinking combined" {
    input='{"model": {"display_name": "opus"}, "output_style": {"name": "concise"}, "effort": {"level": "medium"}, "thinking": {"enabled": true}}'
    unset CLAUDE_CODE_USE_BEDROCK ANTHROPIC_BASE_URL
    run get_model_name
    assert_success
    assert_output "opus (concise) (medium)🧠"
}

@test "get_model_name: OTEL indicator when CLAUDE_CODE_ENABLE_TELEMETRY is set" {
    input='{"model": {"display_name": "opus"}}'
    unset CLAUDE_CODE_USE_BEDROCK ANTHROPIC_BASE_URL
    export CLAUDE_CODE_ENABLE_TELEMETRY=1
    run get_model_name
    assert_success
    assert_output "opus📡"
}

@test "get_model_name: OTEL indicator omitted when CLAUDE_CODE_ENABLE_TELEMETRY is unset" {
    input='{"model": {"display_name": "opus"}}'
    unset CLAUDE_CODE_USE_BEDROCK ANTHROPIC_BASE_URL CLAUDE_CODE_ENABLE_TELEMETRY
    run get_model_name
    assert_success
    assert_output "opus"
}

# Tests for get_rate_limit_5h function

@test "get_rate_limit_5h: present percentage rendered as integer" {
    input='{"rate_limits": {"five_hour": {"used_percentage": 23.5}}}'
    run get_rate_limit_5h
    assert_success
    assert_output "5h 24%"
}

@test "get_rate_limit_5h: zero percentage rendered" {
    input='{"rate_limits": {"five_hour": {"used_percentage": 0}}}'
    run get_rate_limit_5h
    assert_success
    assert_output "5h 0%"
}

@test "get_rate_limit_5h: missing field yields empty string" {
    input='{}'
    run get_rate_limit_5h
    assert_success
    assert_output ""
}

@test "get_rate_limit_5h: null field yields empty string" {
    input='{"rate_limits": {"five_hour": {"used_percentage": null}}}'
    run get_rate_limit_5h
    assert_success
    assert_output ""
}

# Tests for get_rate_limit_5h_color function

@test "get_rate_limit_5h_color: low usage (25%) is green" {
    input='{"rate_limits": {"five_hour": {"used_percentage": 25}}}'
    run get_rate_limit_5h_color
    assert_success
    assert_output "120;220;120"
}

@test "get_rate_limit_5h_color: medium usage (60%) is orange" {
    input='{"rate_limits": {"five_hour": {"used_percentage": 60}}}'
    run get_rate_limit_5h_color
    assert_success
    assert_output "255;180;100"
}

@test "get_rate_limit_5h_color: high usage (80%) is red" {
    input='{"rate_limits": {"five_hour": {"used_percentage": 80}}}'
    run get_rate_limit_5h_color
    assert_success
    assert_output "255;120;120"
}

@test "get_rate_limit_5h_color: missing field defaults to purple" {
    input='{}'
    run get_rate_limit_5h_color
    assert_success
    assert_output "180;140;255"
}

# Tests for get_exceeds_200k_indicator function

@test "get_exceeds_200k_indicator: true returns fire emoji" {
    input='{"exceeds_200k_tokens": true}'
    run get_exceeds_200k_indicator
    assert_success
    assert_output "🔥"
}

@test "get_exceeds_200k_indicator: false returns empty string" {
    input='{"exceeds_200k_tokens": false}'
    run get_exceeds_200k_indicator
    assert_success
    assert_output ""
}

@test "get_exceeds_200k_indicator: missing field returns empty string" {
    input='{}'
    run get_exceeds_200k_indicator
    assert_success
    assert_output ""
}

# Tests for get_cost null handling
#
# Regression: total_cost_usd is null before the first API response and right
# after /clear. Multiplying that in jq aborted the getter and rendered a bare
# "$" segment.

@test "get_cost: null cost falls back to zero" {
    input='{"cost": {"total_cost_usd": null}}'
    run get_cost
    assert_success
    assert_output "0"
}

@test "get_cost: missing cost object falls back to zero" {
    input='{}'
    run get_cost
    assert_success
    assert_output "0"
}

# Tests for post-/compact behaviour
#
# current_usage goes null after a compaction while used_percentage survives, so
# the bar and the percentage segment must not disagree.

@test "get_context_with_bar: bar follows used_percentage when current_usage is null" {
    input='{"context_window": {"current_usage": null, "used_percentage": 35, "total_input_tokens": 70000, "context_window_size": 200000}}'
    run get_context_with_bar
    assert_success
    assert_output "70.0kt/200k 35%[●●●○○○○○○○]"
}

@test "get_context_color: uses used_percentage when current_usage is null" {
    input='{"context_window": {"current_usage": null, "used_percentage": 75}}'
    run get_context_color
    assert_success
    assert_output "255;120;120"
}

@test "get_context_with_bar: percentage derived from current_usage when field absent" {
    input=$(mock_input_basic)  # 50k/200k
    run get_context_with_bar
    assert_success
    assert_output --partial " 25%["
}

# Tests for format_duration

@test "format_duration: hours and minutes" {
    run format_duration 8000
    assert_success
    assert_output "2h13m"
}

@test "format_duration: minutes only" {
    run format_duration 2820
    assert_success
    assert_output "47m"
}

@test "format_duration: under a minute" {
    run format_duration 30
    assert_success
    assert_output "<1m"
}

# Tests for get_rate_limit_5h_reset

@test "get_rate_limit_5h_reset: future reset renders a countdown" {
    input="{\"rate_limits\": {\"five_hour\": {\"used_percentage\": 10, \"resets_at\": $((EPOCHSECONDS + 8000))}}}"
    run get_rate_limit_5h_reset
    assert_success
    assert_output "↻2h13m"
}

@test "get_rate_limit_5h_reset: elapsed reset yields empty string" {
    input="{\"rate_limits\": {\"five_hour\": {\"used_percentage\": 10, \"resets_at\": $((EPOCHSECONDS - 60))}}}"
    run get_rate_limit_5h_reset
    assert_success
    assert_output ""
}

@test "get_rate_limit_5h_reset: missing field yields empty string" {
    input='{"rate_limits": {"five_hour": {"used_percentage": 10}}}'
    run get_rate_limit_5h_reset
    assert_success
    assert_output ""
}

# Tests for the 5h projection. rl5h_input PCT ELAPSED builds a window that
# opened ELAPSED seconds ago.

rl5h_input() {
    input="{\"rate_limits\": {\"five_hour\": {\"used_percentage\": $1, \"resets_at\": $((EPOCHSECONDS + 18000 - $2))}}}"
}

@test "get_rate_limit_5h: projection appended once the window is old enough" {
    rl5h_input 10 3600
    run get_rate_limit_5h
    assert_success
    assert_output "5h 10%→50%"
}

@test "get_rate_limit_5h: no projection in the first 15 minutes" {
    rl5h_input 3 300
    run get_rate_limit_5h
    assert_success
    assert_output "5h 3%"
}

@test "get_rate_limit_5h_projection: extrapolates from the unrounded percentage" {
    rl5h_input 1.4 900
    run get_rate_limit_5h_projection
    assert_success
    assert_output "42"
}

@test "get_rate_limit_5h_projection: elapsed reset yields empty string" {
    input="{\"rate_limits\": {\"five_hour\": {\"used_percentage\": 10, \"resets_at\": $((EPOCHSECONDS - 60))}}}"
    run get_rate_limit_5h_projection
    assert_success
    assert_output ""
}

@test "get_rate_limit_5h_color: high usage late in the window stays green" {
    rl5h_input 70 17400
    run get_rate_limit_5h_color
    assert_success
    assert_output "120;220;120"
}

@test "get_rate_limit_5h_color: projection right at 100% is orange" {
    rl5h_input 23.64 3600
    run get_rate_limit_5h_color
    assert_success
    assert_output "255;180;100"
}

@test "get_rate_limit_5h_color: low usage burning fast is red" {
    rl5h_input 30 3600
    run get_rate_limit_5h_color
    assert_success
    assert_output "255;120;120"
}

@test "get_rate_limit_5h_warning: time to exhaustion when it precedes the reset" {
    rl5h_input 30 3600
    run get_rate_limit_5h_warning
    assert_success
    assert_output "⚠3h0m"
}

@test "get_rate_limit_5h_warning: silent when the reset comes first" {
    rl5h_input 20 3600
    run get_rate_limit_5h_warning
    assert_success
    assert_output ""
}

@test "get_rate_limit_5h_warning: exhausted window warns immediately" {
    rl5h_input 100 3600
    run get_rate_limit_5h_warning
    assert_success
    assert_output "⚠<1m"
}

@test "get_rate_limit_5h_warning: zero usage yields empty string" {
    rl5h_input 0 3600
    run get_rate_limit_5h_warning
    assert_success
    assert_output ""
}

@test "get_rate_limit_5h_projection: early burst is shrunk towards the prior" {
    rl5h_input 13.3 900
    run get_rate_limit_5h_projection
    assert_success
    assert_output "129"
}

@test "get_rate_limit_5h_projection: light early usage is pulled up to the prior" {
    rl5h_input 0.5 900
    run get_rate_limit_5h_projection
    assert_success
    assert_output "35"
}

@test "main: burning 5h window renders projection, warning and reset" {
    rl5h_input 30 3600
    run main <<<"$input"
    assert_success
    assert_output --partial "5h 30%→123% ⚠3h0m ↻"
}

# Tests for get_cache

@test "get_cache: absent prompt_cache yields empty string" {
    input='{}'
    run get_cache
    assert_success
    assert_output ""
}

@test "get_cache: renders ttl and hit ratio behind the cache glyph" {
    input='{"prompt_cache": {"warm": true, "ttl": "1h", "hit_ratio": 0.914}}'
    run get_cache
    assert_success
    assert_output "${CACHE_GLYPH}1h 91%"
}

@test "get_cache: short ttl and low hit ratio use the same glyph" {
    input='{"prompt_cache": {"warm": true, "ttl": "5m", "hit_ratio": 0.4}}'
    run get_cache
    assert_success
    assert_output "${CACHE_GLYPH}5m 40%"
}

@test "get_cache: null hit ratio omits the percentage" {
    input='{"prompt_cache": {"warm": true, "ttl": "1h", "hit_ratio": null}}'
    run get_cache
    assert_success
    assert_output "${CACHE_GLYPH}1h"
}

# Tests for get_cache_color (inverted: a high hit ratio is the good case)

@test "get_cache_color: high hit ratio (91%) is green" {
    input='{"prompt_cache": {"hit_ratio": 0.91}}'
    run get_cache_color
    assert_success
    assert_output "120;220;120"
}

@test "get_cache_color: medium hit ratio (60%) is orange" {
    input='{"prompt_cache": {"hit_ratio": 0.6}}'
    run get_cache_color
    assert_success
    assert_output "255;180;100"
}

@test "get_cache_color: low hit ratio (20%) is red" {
    input='{"prompt_cache": {"hit_ratio": 0.2}}'
    run get_cache_color
    assert_success
    assert_output "255;120;120"
}

@test "get_cache_color: unknown hit ratio defaults to purple" {
    input='{"prompt_cache": {"warm": true}}'
    run get_cache_color
    assert_success
    assert_output "180;140;255"
}

# Cold cache
#
# Regression: hit_ratio is a session average, so a cache that had gone cold
# still rendered as a healthy green segment.

@test "get_cache: warm false renders cold instead of the hit ratio" {
    input='{"prompt_cache": {"warm": false, "caching_observed": true, "ttl": "5m", "hit_ratio": 0.93}}'
    run get_cache
    assert_success
    assert_output "${CACHE_GLYPH}5m cold"
}

@test "get_cache_color: cold cache is red despite a high hit ratio" {
    input='{"prompt_cache": {"warm": false, "caching_observed": true, "ttl": "5m", "hit_ratio": 0.93}}'
    run get_cache_color
    assert_success
    assert_output "255;120;120"
}

@test "get_cache: expires_at in the past is cold even while warm is stale" {
    input="{\"prompt_cache\": {\"warm\": true, \"caching_observed\": true, \"ttl\": \"1h\", \"hit_ratio\": 0.93, \"expires_at\": $((EPOCHSECONDS - 60))}}"
    run get_cache
    assert_success
    assert_output "${CACHE_GLYPH}1h cold"
}

@test "get_cache: expires_at in the future stays warm" {
    input="{\"prompt_cache\": {\"warm\": true, \"caching_observed\": true, \"ttl\": \"1h\", \"hit_ratio\": 0.93, \"expires_at\": $((EPOCHSECONDS + 600))}}"
    run get_cache
    assert_success
    assert_output "${CACHE_GLYPH}1h 93%"
}

@test "get_cache: unobserved caching is never shown as cold" {
    input='{"prompt_cache": {"warm": false, "caching_observed": false, "ttl": "5m", "hit_ratio": null}}'
    run get_cache
    assert_success
    assert_output "${CACHE_GLYPH}5m"
}

# Tests for separator glyph selection

@test "separator: differing colors use the solid arrow" {
    run separator "$RED" "$ORANGE"
    assert_success
    assert_output --partial "$SEP_THICK"
}

@test "separator: matching colors use the hairline arrow" {
    run separator "$GREEN" "$GREEN"
    assert_success
    assert_output --partial "$SEP_THIN"
    refute_output --partial "$SEP_THICK"
}

# Tests for get_git_status
#
# Regression: git diff ignores untracked files, so a repo holding only a new
# file rendered as clean.

git_tmp_repo() {
    cd "$BATS_TEST_TMPDIR" || return 1
    git init -q repo && cd repo || return 1
    git -c user.name=t -c user.email=t@t commit -q --allow-empty -m init
}

@test "get_git_status: clean repo is a tick" {
    git_tmp_repo
    run get_git_status
    assert_success
    assert_output "✓"
}

@test "get_git_status: untracked file counts as dirty" {
    git_tmp_repo
    touch newfile.txt
    run get_git_status
    assert_success
    assert_output "±"
}

@test "get_git_status: staged change counts as dirty" {
    git_tmp_repo
    touch staged.txt && git add staged.txt
    run get_git_status
    assert_success
    assert_output "±"
}

@test "get_git_status: outside a repo yields empty string" {
    cd "$BATS_TEST_TMPDIR" || return 1
    GIT_CEILING_DIRECTORIES="$BATS_TEST_TMPDIR" run get_git_status
    assert_success
    assert_output ""
}

# Tests for get_working_dir

@test "get_working_dir: prefers current_dir over project_dir" {
    input='{"workspace": {"current_dir": "/srv/project/sub", "project_dir": "/srv/project"}}'
    run get_working_dir
    assert_success
    assert_output "/srv/project/sub"
}

@test "get_working_dir: falls back to project_dir" {
    input='{"workspace": {"project_dir": "/srv/project"}}'
    run get_working_dir
    assert_success
    assert_output "/srv/project"
}

@test "get_working_dir: home is abbreviated" {
    input="{\"workspace\": {\"current_dir\": \"$HOME/repos\"}}"
    run get_working_dir
    assert_success
    assert_output "~/repos"
}

# End-to-end tests for main
#
# The getters above are exercised in isolation; these run the script the way
# Claude Code does, which is where the null-cost crash actually surfaced.

mock_input_full() {
    cat <<'EOF'
{
  "model": {"display_name": "Opus"},
  "version": "2.1.260",
  "transcript_path": "/path/to/abc123-timestamp.jsonl",
  "workspace": {"project_dir": "/home/user/project"},
  "cost": {"total_cost_usd": 1.2345},
  "context_window": {
    "current_usage": {"input_tokens": 8500, "cache_creation_input_tokens": 5000, "cache_read_input_tokens": 82000},
    "total_input_tokens": 95500,
    "context_window_size": 200000,
    "used_percentage": 47.75
  },
  "exceeds_200k_tokens": false,
  "prompt_cache": {"warm": true, "ttl": "1h", "hit_ratio": 0.91},
  "rate_limits": {"five_hour": {"used_percentage": 23.5}},
  "effort": {"level": "high"},
  "thinking": {"enabled": true}
}
EOF
}

@test "main: full payload renders three rows without errors" {
    run bash -c "$(declare -f mock_input_full); mock_input_full | bash '$BATS_TEST_DIRNAME/claude-code-statusline.sh' 2>&1"
    assert_success
    assert_equal "${#lines[@]}" 3
    refute_output --partial "jq: error"
    refute_output --partial "command not found"
}

@test "main: null cost payload renders without a jq error" {
    run bash -c "printf '%s' '{\"cost\": {\"total_cost_usd\": null}, \"context_window\": {}}' | bash '$BATS_TEST_DIRNAME/claude-code-statusline.sh' 2>&1"
    assert_success
    refute_output --partial "jq: error"
    assert_output --partial "0\$"
}

@test "main: empty JSON object renders without errors" {
    run bash -c "printf '%s' '{}' | bash '$BATS_TEST_DIRNAME/claude-code-statusline.sh' 2>&1"
    assert_success
    assert_equal "${#lines[@]}" 3
    refute_output --partial "jq: error"
}

# Recorded from a live Claude Code 2.1.283 session (ids and paths replaced).
# Its timestamps are absolute, so only assert what does not depend on the
# clock.
mock_input_live_2_1_283() {
    cat <<'JSON'
{
  "session_id": "00000000-0000-0000-0000-000000000000",
  "transcript_path": "/home/user/.claude/projects/-home-user-project/00000000-0000-0000-0000-000000000000.jsonl",
  "cwd": "/home/user/project",
  "scratchpad_dir": "/tmp/claude-1000/scratchpad",
  "prompt_id": "11111111-1111-1111-1111-111111111111",
  "effort": {
    "level": "medium"
  },
  "session_name": "statusline fixture",
  "model": {
    "id": "claude-opus-5-5",
    "display_name": "Opus 5.5"
  },
  "workspace": {
    "current_dir": "/home/user/project",
    "project_dir": "/home/user/project",
    "added_dirs": []
  },
  "version": "2.1.283",
  "output_style": {
    "name": "default"
  },
  "cost": {
    "total_cost_usd": 0.5324084000000001,
    "total_duration_ms": 253990,
    "total_api_duration_ms": 71521,
    "total_lines_added": 0,
    "total_lines_removed": 0
  },
  "context_window": {
    "total_input_tokens": 62036,
    "total_output_tokens": 378,
    "context_window_size": 1000000,
    "current_usage": {
      "input_tokens": 2,
      "output_tokens": 378,
      "cache_creation_input_tokens": 1793,
      "cache_read_input_tokens": 60241
    },
    "used_percentage": 6,
    "remaining_percentage": 94
  },
  "exceeds_200k_tokens": false,
  "prompt_cache": {
    "warm": true,
    "caching_observed": true,
    "ttl": "1h",
    "expires_at": 1790602139,
    "requests": 11,
    "misses": 0,
    "expected_rebuilds": 0,
    "hit_ratio": 0.9451518802082814,
    "cache_write_tokens": 33032,
    "miss_recache_tokens": 0,
    "last_miss_at": null,
    "last_miss_cause": null,
    "miss_causes": {},
    "recache_tokens_if_cold": 62036
  },
  "fast_mode": false,
  "thinking": {
    "enabled": true
  },
  "rate_limits": {
    "five_hour": {
      "used_percentage": 14,
      "resets_at": 1790610000
    },
    "seven_day": {
      "used_percentage": 32,
      "resets_at": 1790805600
    }
  }
}
JSON
}

@test "main: live 2.1.283 payload renders three rows without errors" {
    run bash -c "$(declare -f mock_input_live_2_1_283); mock_input_live_2_1_283 | bash '$BATS_TEST_DIRNAME/claude-code-statusline.sh' 2>&1"
    assert_success
    assert_equal "${#lines[@]}" 3
    refute_output --partial "jq: error"
    assert_output --partial "62.0kt/1M 6%["
}

# Regression: the same context size used to render three times (token label,
# a separate percentage segment and total_input_tokens rounded to kt).
@test "main: context size and percentage appear once" {
    run bash -c "$(declare -f mock_input_full); mock_input_full | bash '$BATS_TEST_DIRNAME/claude-code-statusline.sh' 2>&1"
    assert_success
    assert_output --partial "95.5kt/200k 48%["
    refute_output --partial "96kt"
    assert_equal "$(grep -o '48%' <<<"$output" | wc -l)" 1
}
