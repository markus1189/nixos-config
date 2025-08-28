#!/usr/bin/env bash
input=$(cat)

get_model_name() { echo "$input" | jq -r '.model.display_name'; }
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
    if test -f "$(get_transcript_path)" ; then
        jq -r '
    select(.message.usage and (.isSidechain != true)) |
    {
        timestamp: .timestamp,
        context_length: (
            (.message.usage.input_tokens // 0) +
            (.message.usage.cache_read_input_tokens // 0) +
            (.message.usage.cache_creation_input_tokens // 0)
        )
    }
' "$(get_transcript_path)" |
            jq -s 'sort_by(.timestamp) | last | .context_length // 0'
    else
        echo "n/a"
    fi
}

get_context_diff() {
    if test -f "$(get_transcript_path)" ; then
        local sizes
        sizes=$(jq -r '
    select(.message.usage and (.isSidechain == true | not)) |
    {
        timestamp: .timestamp,
        context_length: (
            (.message.usage.input_tokens // 0) +
            (.message.usage.cache_read_input_tokens // 0) +
            (.message.usage.cache_creation_input_tokens // 0)
        )
    }
' "$(get_transcript_path)" | jq -s -r 'sort_by(.timestamp) | map(.context_length) | .[-2:] | @tsv')
        
        if [ -n "$sizes" ]; then
            local prev current diff
            read -r prev current <<< "$sizes"
            
            if [ -n "$prev" ] && [ -n "$current" ] && [ "$prev" != "null" ] && [ "$current" != "null" ] && [[ "$prev" =~ ^[0-9]+$ ]] && [[ "$current" =~ ^[0-9]+$ ]]; then
                diff=$((current - prev))
                if [ $diff -gt 0 ]; then
                    echo "+$diff"
                elif [ $diff -lt 0 ]; then
                    echo "$diff"
                else
                    echo ""
                fi
            else
                echo ""
            fi
        else
            echo ""
        fi
    else
        echo ""
    fi
}

get_agent_count() {
    if test -f "$(get_transcript_path)" ; then
        jq -r 'select(.type == "assistant" and (.message.content[]?.name == "Task"))' \
            "$(get_transcript_path)" | jq -s length
    else
        echo "0"
    fi
}

get_agent_tokens() {
    if test -f "$(get_transcript_path)" ; then
        local total
        total=$(jq -r 'select(.isSidechain == true and .type == "assistant" and .message.usage) | 
            (.message.usage.input_tokens // 0) + (.message.usage.output_tokens // 0)' \
            "$(get_transcript_path)" | \
            awk '{sum+=$1} END {print sum+0}')
        if [ "$total" -gt 0 ]; then
            if [ "$total" -ge 1000 ]; then
                printf "%.1fk" "$(echo "scale=1; $total / 1000" | bc)"
            else
                echo "$total"
            fi
        else
            echo "0"
        fi
    else
        echo "0"
    fi
}

get_git_branch() {
    git branch --quiet --show-current 2>/dev/null || echo "n/a"
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
    local context diff
    context=$(get_context_size)
    diff=$(get_context_diff)
    
    if [ "$context" = "n/a" ]; then
        echo "n/a[○○○○○○○○○○]"
        return
    fi

    # Handle non-numeric context gracefully
    if [[ ! "$context" =~ ^[0-9]+$ ]]; then
        echo "n/a[○○○○○○○○○○]"
        return
    fi

    local filled=$(( context / 20000 ))  # Scale for ~200k max context
    if [ $filled -gt 10 ]; then filled=10; fi
    if [ $filled -lt 0 ]; then filled=0; fi

    local bar=""
    for ((i=1; i<=filled; i++)); do bar+="●"; done
    for ((i=filled+1; i<=10; i++)); do bar+="○"; done

    if [ -n "$diff" ]; then
        echo "${context}[${bar}]${diff}"
    else
        echo "${context}[${bar}]"
    fi
}

get_context_color() {
    local context
    context=$(get_context_size)
    if [ "$context" = "n/a" ]; then
        echo "180;140;255"  # Default purple (matching cost)
    elif [ "$context" -gt 150000 ]; then
        echo "255;120;120"  # Red for high usage
    elif [ "$context" -gt 100000 ]; then
        echo "255;180;100"  # Orange for medium usage
    else
        echo "120;220;120"  # Green for low usage
    fi
}

# Color definitions (RGB values)
readonly RED="255;120;120"
readonly ORANGE="255;180;100"
readonly GREEN="120;220;120"
readonly BLUE="100;180;255"
readonly PURPLE="180;140;255"
readonly CYAN="100;200;200"
readonly PINK="255;140;180"

# ANSI escape sequences
readonly RESET='\033[0m'
readonly BLACK_FG='\033[30m'

# Color functions
bg_color() { echo -n "\033[48;2;${1}m"; }
fg_color() { echo -n "\033[38;2;${1}m"; }
segment() { echo -n "$(bg_color "$1")${BLACK_FG} $2 ${RESET}"; }
separator() { echo -n "$(fg_color "$1")$(bg_color "$2")${RESET}"; }

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
echo -en "$(separator "$(get_context_color)" "$CYAN")"
echo -en "$(segment "$CYAN" "🤖$(get_agent_count)|$(get_agent_tokens)t")"
echo -en "$(separator "$CYAN" "$PINK")"
echo -en "$(segment "$PINK" "$(get_transcript_id)")"
echo -en "$(fg_color "$PINK")"
echo
