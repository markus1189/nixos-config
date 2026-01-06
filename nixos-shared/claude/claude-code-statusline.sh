#!/usr/bin/env bash

# Info: BATS unit tests at ./claude-code-statusline.bats

get_output_style() {
    local style
    style=$(echo "$input" | jq -r '.output_style.name // "default"')
    if [ "$style" = "default" ] || [ "$style" = "null" ] || [ -z "$style" ]; then
        echo ""
    else
        echo "$style"
    fi
}

shorten_bedrock_model() {
    local model="$1"

    # Match Bedrock pattern: @bedrock/.../claude-{model}-{version}-{date}-v1:0
    if [[ "$model" =~ @bedrock/.*claude-(sonnet|haiku|opus)-([0-9]+-[0-9]+|[0-9]+)- ]]; then
        local model_type="${BASH_REMATCH[1]}"
        local version="${BASH_REMATCH[2]}"
        # Convert dashes to dots (4-5 → 4.5)
        version="${version//-/.}"
        echo "${model_type}-${version}"
    else
        # Not a Bedrock model or doesn't match pattern - return as-is
        echo "$model"
    fi
}

get_model_name() {
    local model_name style_suffix indicator_suffix
    model_name=$(echo "$input" | jq -r '.model.display_name')

    # Shorten Bedrock model names
    model_name=$(shorten_bedrock_model "$model_name")

    # Add output style if not default
    local output_style
    output_style=$(get_output_style)
    if [ -n "$output_style" ]; then
        style_suffix=" ($output_style)"
    else
        style_suffix=""
    fi

    # Add indicator emojis
    indicator_suffix=""
    if [ -n "${CLAUDE_CODE_USE_BEDROCK:-}" ]; then
        indicator_suffix+="🪨"
    fi
    if [ "${ANTHROPIC_BASE_URL:-}" = "https://api.portkey.ai" ]; then
        indicator_suffix+="🔑"
    fi

    echo "${model_name}${style_suffix}${indicator_suffix}"
}
get_current_dir() { echo "$input" | jq -r '.workspace.current_dir'; }
get_project_dir() { echo "$input" | jq -r '.workspace.project_dir' | sed "s|^$HOME|~|"; }
get_version() { echo "$input" | jq -r '.version'; }
get_cost() { echo "$input" | jq -r '.cost.total_cost_usd*100|round/100'; }
get_duration() { echo "$input" | jq -r '.cost.total_duration_ms'; }
get_lines_added() { echo "$input" | jq -r '.cost.total_lines_added'; }
get_lines_removed() { echo "$input" | jq -r '.cost.total_lines_removed'; }
get_transcript_path() { echo "$input" | jq -r '.transcript_path'; }
get_transcript_id() { basename "$(get_transcript_path)" ".jsonl" | cut -d'-' -f1; }

get_context_size() {
    local tokens
    tokens=$(echo "$input" | jq -r '.context_window.current_usage.input_tokens // empty')
    if [ -n "$tokens" ] && [[ "$tokens" =~ ^[0-9]+$ ]]; then
        echo "$tokens"
    else
        echo "⌀"
    fi
}

get_context_window_size() {
    local size
    size=$(echo "$input" | jq -r '.context_window.context_window_size // empty')
    if [ -n "$size" ] && [[ "$size" =~ ^[0-9]+$ ]]; then
        echo "$size"
    else
        echo "200000"  # Default fallback
    fi
}

get_git_branch() {
    git branch --quiet --show-current 2>/dev/null || echo "⌀"
}

get_git_status() {
    local status=""

    # Check for uncommitted changes
    if git diff --quiet 2>/dev/null && git diff --cached --quiet 2>/dev/null; then
        status+="✓"
    else
        status+="±"
    fi

    # Get ahead/behind counts
    local upstream
    upstream=$(git rev-parse --abbrev-ref --symbolic-full-name '@{u}' 2>/dev/null)
    if [ -n "$upstream" ]; then
        local counts
        counts=$(git rev-list --left-right --count HEAD..."$upstream" 2>/dev/null)
        if [ -n "$counts" ]; then
            local ahead behind
            ahead=$(echo "$counts" | cut -f1)
            behind=$(echo "$counts" | cut -f2)
            if [ "$ahead" -gt 0 ]; then
                status+="${ahead}↑"
            fi
            if [ "$behind" -gt 0 ]; then
                status+="${behind}↓"
            fi
        fi
    fi

    echo "$status"
}

get_context_with_bar() {
    local context formatted_context window_size scale_factor
    context=$(get_context_size)

    if [ "$context" = "⌀" ]; then
        echo "⌀[○○○○○○○○○○]"
        return
    fi

    # Handle non-numeric context gracefully
    if [[ ! "$context" =~ ^[0-9]+$ ]]; then
        echo "⌀[○○○○○○○○○○]"
        return
    fi

    # Format context with kt suffix for consistency
    if [ "$context" -ge 1000 ]; then
        formatted_context=$(printf "%.1fkt" "$(echo "scale=1; $context / 1000" | bc)")
    else
        formatted_context="${context}"
    fi

    # Scale based on actual context window size
    window_size=$(get_context_window_size)
    scale_factor=$(( window_size / 10 ))
    local filled=$(( context / scale_factor ))
    if [ $filled -gt 10 ]; then filled=10; fi
    if [ $filled -lt 0 ]; then filled=0; fi

    local bar=""
    for ((i=1; i<=filled; i++)); do bar+="●"; done
    for ((i=filled+1; i<=10; i++)); do bar+="○"; done

    echo "${formatted_context}[${bar}]"
}

get_context_color() {
    local context window_size threshold_red threshold_orange
    context=$(get_context_size)

    if [ "$context" = "⌀" ]; then
        echo "180;140;255"  # Default purple
        return
    fi

    # Dynamic thresholds based on context window size
    window_size=$(get_context_window_size)
    threshold_red=$(( window_size * 60 / 100 ))      # 60% of window
    threshold_orange=$(( window_size * 40 / 100 ))   # 40% of window

    if [ "$context" -gt "$threshold_red" ]; then
        echo "255;120;120"  # Red for high usage (>60%)
    elif [ "$context" -gt "$threshold_orange" ]; then
        echo "255;180;100"  # Orange for medium usage (>40%)
    else
        echo "120;220;120"  # Green for low usage (<40%)
    fi
}

# Color definitions (RGB values)
readonly RED="255;120;120"
readonly ORANGE="255;180;100"
readonly GREEN="120;220;120"
readonly BLUE="100;180;255"
readonly PURPLE="180;140;255"
readonly PINK="255;140;180"

# ANSI escape sequences
readonly RESET='\033[0m'
readonly BLACK_FG='\033[30m'

# Color functions
bg_color() { printf "\033[48;2;%sm" "$1"; }
fg_color() { printf "\033[38;2;%sm" "$1"; }
segment() { echo -n "$(bg_color "$1")${BLACK_FG} $2 ${RESET}"; }
separator() { echo -n "$(fg_color "$1")$(bg_color "$2")${RESET}"; }

# Main execution function
main() {
    input=$(cat)

    # Build statusline with readable segments
    echo -en "$(segment "$RED" "$(get_model_name)")"
    echo -en "$(separator "$RED" "$ORANGE")"
    echo -en "$(segment "$ORANGE" "$(get_version)")"
    echo -en "$(separator "$ORANGE" "$GREEN")"
    echo -en "$(segment "$GREEN" "$(get_git_branch)$(get_git_status)")"
    echo -en "$(separator "$GREEN" "$BLUE")"
    echo -en "$(segment "$BLUE" "$(get_project_dir)")"
    echo -en "$(separator "$BLUE" "$PURPLE")"
    echo -en "$(segment "$PURPLE" "$(get_cost)$")"
    echo -en "$(separator "$PURPLE" "$(get_context_color)")"
    echo -en "$(segment "$(get_context_color)" "$(get_context_with_bar)")"
    echo -en "$(separator "$(get_context_color)" "$PINK")"
    echo -en "$(segment "$PINK" "$(get_transcript_id)")"
    echo -en "$(fg_color "$PINK")"
    echo
}

# Only run main if script is executed directly (not sourced for testing)
# When sourced, BASH_SOURCE[0] != $0
# When executed, they're equal OR BASH_SOURCE[0] is empty (piped to bash)
if [ -z "${BASH_SOURCE[0]}" ] || [ "${BASH_SOURCE[0]}" = "${0}" ]; then
    main
fi
