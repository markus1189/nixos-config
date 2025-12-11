#!/usr/bin/env bats

# Tests for claude-code-statusline.sh

# Source the statusline script to get access to its functions
# Use BATS_TEST_DIRNAME which is set by bats to the directory containing this test file
source "$BATS_TEST_DIRNAME/claude-code-statusline.sh"

# Test fixture helpers
mock_input_basic() {
    cat <<'EOF'
{
  "model": {"display_name": "@bedrock/eu.anthropic.claude-sonnet-4-5-20250929-v1:0"},
  "output_style": {"name": "default"},
  "version": "1.2.3",
  "workspace": {"project_dir": "/home/user/project"},
  "cost": {"total_cost_usd": 1.2345},
  "context_window": {"total_input_tokens": 50000, "context_window_size": 200000},
  "transcript_path": "/path/to/abc123-timestamp.jsonl"
}
EOF
}

mock_input_high_usage() {
    cat <<'EOF'
{
  "model": {"display_name": "claude-opus-4"},
  "output_style": {"name": "concise"},
  "context_window": {"total_input_tokens": 150000, "context_window_size": 200000},
  "transcript_path": "/path/to/xyz789-timestamp.jsonl"
}
EOF
}

# Tests for shorten_bedrock_model function

@test "shorten_bedrock_model: Sonnet 4.5 Bedrock model" {
    result=$(shorten_bedrock_model "@bedrock/eu.anthropic.claude-sonnet-4-5-20250929-v1:0")
    [ "$result" = "sonnet-4.5" ]
}

@test "shorten_bedrock_model: Haiku 3.5 Bedrock model" {
    result=$(shorten_bedrock_model "@bedrock/us.anthropic.claude-haiku-3-5-20250101-v1:0")
    [ "$result" = "haiku-3.5" ]
}

@test "shorten_bedrock_model: Non-Bedrock model unchanged" {
    result=$(shorten_bedrock_model "claude-opus-4")
    [ "$result" = "claude-opus-4" ]
}

# Tests for get_model_name function

@test "get_model_name: Bedrock model shortened without style" {
    input=$(mock_input_basic)
    result=$(get_model_name)
    # May have indicator suffixes (🔑 for portkey, 🪨 for bedrock)
    [[ "$result" =~ ^sonnet-4\.5 ]]
}

@test "get_model_name: Model with output style appended" {
    input=$(mock_input_high_usage)
    result=$(get_model_name)
    # May have indicator suffixes, but must start with model (style)
    [[ "$result" =~ ^claude-opus-4\ \(concise\) ]]
}

# Tests for get_context_size function

@test "get_context_size: Valid token count" {
    input=$(mock_input_basic)
    result=$(get_context_size)
    [ "$result" = "50000" ]
}

@test "get_context_size: Missing tokens returns placeholder" {
    input='{"context_window": {}}'
    result=$(get_context_size)
    [ "$result" = "⌀" ]
}

# Tests for get_context_with_bar function

@test "get_context_with_bar: 50k tokens shows 2 filled dots" {
    input=$(mock_input_basic)
    result=$(get_context_with_bar)
    [[ "$result" =~ 50\.0kt\[●●○○○○○○○○\] ]]
}

@test "get_context_with_bar: Missing data shows empty bar" {
    input='{"context_window": {}}'
    result=$(get_context_with_bar)
    [ "$result" = "⌀[○○○○○○○○○○]" ]
}

# Tests for get_context_color function

@test "get_context_color: Low usage (25%) is green" {
    input=$(mock_input_basic)  # 50k/200k = 25%
    result=$(get_context_color)
    [ "$result" = "120;220;120" ]
}

@test "get_context_color: High usage (75%) is red" {
    input=$(mock_input_high_usage)  # 150k/200k = 75%
    result=$(get_context_color)
    [ "$result" = "255;120;120" ]
}

# Tests for get_cost function

@test "get_cost: Round to 2 decimal places" {
    input=$(mock_input_basic)
    result=$(get_cost)
    [ "$result" = "1.23" ]
}

# Tests for get_transcript_id function

@test "get_transcript_id: Extract ID from path" {
    input=$(mock_input_basic)
    result=$(get_transcript_id)
    [ "$result" = "abc123" ]
}
