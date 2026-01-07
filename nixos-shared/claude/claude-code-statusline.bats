#!/usr/bin/env nix
#! nix shell --impure --expr ``with import <nixpkgs> {}; bats.withLibraries (p: [ p.bats-assert p.bats-support ])`` --command bats

# Tests for claude-code-statusline.sh

# Load bats-assert and bats-support libraries before each test
setup() {
    bats_load_library bats-support
    bats_load_library bats-assert

    # Source the statusline script to get access to its functions
    source "$BATS_TEST_DIRNAME/claude-code-statusline.sh"
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
    # May have indicator suffixes (🔑 for portkey, 🪨 for bedrock)
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

@test "get_model_name: Portkey indicator when ANTHROPIC_BASE_URL is portkey" {
    input=$(mock_input_basic)
    unset CLAUDE_CODE_USE_BEDROCK
    export ANTHROPIC_BASE_URL="https://api.portkey.ai"
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
