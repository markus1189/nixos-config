#!/usr/bin/env bash
input=$(cat)

get_model_name() { echo "$input" | jq -r '.model.display_name'; }
get_current_dir() { echo "$input" | jq -r '.workspace.current_dir'; }
get_project_dir() { echo "$input" | jq -r '.workspace.project_dir'; }
get_version() { echo "$input" | jq -r '.version'; }
get_cost() { echo "$input" | jq -r '.cost.total_cost_usd*100|round/100'; }
get_duration() { echo "$input" | jq -r '.cost.total_duration_ms'; }
get_lines_added() { echo "$input" | jq -r '.cost.total_lines_added'; }
get_lines_removed() { echo "$input" | jq -r '.cost.total_lines_removed'; }
get_transcript_path() { echo "$input" | jq -r '.transcript_path'; }
get_transcript_id() { basename "$(get_transcript_path)" ".jsonl"; }

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

echo -e "\033[1m$(get_model_name)\033[0m | \033[1m\033[38;5;121m$(get_version)\033[0m | \033[1m\033[38;5;153m$(get_git_branch)\033[0m | \033[1m\033[38;5;214m$(get_cost)$\033[0m | \033[1m\033[38;5;225m$(get_project_dir)\033[0m | \033[1m\033[38;5;215m$(get_context_size)\033[0m | \033[1m\033[38;5;229m$(get_transcript_id)\033[0m"
