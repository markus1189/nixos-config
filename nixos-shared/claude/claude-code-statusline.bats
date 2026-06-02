#!/usr/bin/env nix
#! nix shell --impure --expr ``with import <nixpkgs> {}; bats.withLibraries (p: [ p.bats-assert p.bats-support ])`` --command bats

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
    assert_regex "$output" '50\.0kt\[●●○○○○○○○○\]'
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

# Tests for get_formatted_context_window function

@test "get_formatted_context_window: 50k tokens shows as 50kt" {
    input=$(mock_input_basic)
    run get_formatted_context_window
    assert_success
    assert_output "50kt"
}

@test "get_formatted_context_window: 150k tokens shows as 150kt" {
    input=$(mock_input_high_usage)
    run get_formatted_context_window
    assert_success
    assert_output "150kt"
}

@test "get_formatted_context_window: Small number (under 1000) shows as-is" {
    input='{"context_window": {"total_input_tokens": 500}}'
    run get_formatted_context_window
    assert_success
    assert_output "500"
}

@test "get_formatted_context_window: Zero tokens shows placeholder" {
    input='{"context_window": {"total_input_tokens": 0}}'
    run get_formatted_context_window
    assert_success
    assert_output "⌀"
}

@test "get_formatted_context_window: Missing data shows placeholder" {
    input='{"context_window": {}}'
    run get_formatted_context_window
    assert_success
    assert_output "⌀"
}

# Tests for get_context_percentage function

@test "get_context_percentage: Uses native percentage" {
    input='{"context_window": {"used_percentage": 42.6}}'
    run get_context_percentage
    assert_success
    assert_output "43%"
}

@test "get_context_percentage: Rounds to nearest integer" {
    input='{"context_window": {"used_percentage": 75.8}}'
    run get_context_percentage
    assert_success
    assert_output "76%"
}

@test "get_context_percentage: Missing percentage shows placeholder" {
    input='{"context_window": {}}'
    run get_context_percentage
    assert_success
    assert_output "⌀"
}

@test "get_context_percentage: Null percentage shows placeholder" {
    input='{"context_window": {"used_percentage": null}}'
    run get_context_percentage
    assert_success
    assert_output "⌀"
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
