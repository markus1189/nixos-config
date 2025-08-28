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
    local context
    context=$(get_context_size)
    if [ "$context" = "n/a" ]; then
        echo "n/a[░░░░░░░░░░]"
        return
    fi

    # Handle non-numeric context gracefully
    if [[ ! "$context" =~ ^[0-9]+$ ]]; then
        echo "n/a[░░░░░░░░░░]"
        return
    fi

    local filled=$(( context / 20000 ))  # Scale for ~200k max context
    if [ $filled -gt 10 ]; then filled=10; fi
    if [ $filled -lt 0 ]; then filled=0; fi

    local bar=""
    for ((i=1; i<=filled; i++)); do bar+="●"; done
    for ((i=filled+1; i<=10; i++)); do bar+="○"; done

    echo "${context}[${bar}]"
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

echo -e "\033[48;2;255;120;120m\033[30m $(get_model_name) \033[0m\033[38;2;255;120;120m\033[48;2;255;180;100m\033[0m\033[48;2;255;180;100m\033[30m $(get_version) \033[0m\033[38;2;255;180;100m\033[48;2;120;220;120m\033[0m\033[48;2;120;220;120m\033[30m $(get_git_branch)$(get_git_status) \033[0m\033[38;2;120;220;120m\033[48;2;100;180;255m\033[0m\033[48;2;100;180;255m\033[30m $(get_project_dir) \033[0m\033[38;2;100;180;255m\033[48;2;180;140;255m\033[0m\033[48;2;180;140;255m\033[30m $(get_cost)$ \033[0m\033[38;2;180;140;255m\033[48;2;$(get_context_color)m\033[0m\033[48;2;$(get_context_color)m\033[30m $(get_context_with_bar) \033[0m\033[38;2;$(get_context_color)m\033[48;2;255;140;180m\033[0m\033[48;2;255;140;180m\033[30m $(get_transcript_id) \033[0m\033[38;2;255;140;180m\033[0m"
