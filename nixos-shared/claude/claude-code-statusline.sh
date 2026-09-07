#!/usr/bin/env bash

# Info: BATS unit tests at ./claude-code-statusline.bats

# Color definitions (RGB values)
readonly RED="255;120;120"
readonly ORANGE="255;180;100"
readonly GREEN="120;220;120"
readonly BLUE="100;180;255"
readonly PURPLE="180;140;255"
readonly PINK="255;140;180"
readonly CYAN="100;200;200"
readonly YELLOW="220;180;80"

# ANSI escape sequences
readonly RESET='\033[0m'
readonly BLACK_FG='\033[30m'

# Powerline separators (Nerd Font private use area, see laptop/laptop.nix)
readonly SEP_THICK=$''  # solid arrow, between differently colored segments
readonly SEP_THIN=$''   # hairline arrow, between segments of equal color
readonly CACHE_GLYPH=$''  # nf-memory (U+E266), marks the prompt cache segment

readonly PLACEHOLDER="⌀"

# All of the JSON is read in a single jq pass: the status line is re-rendered
# after every assistant response, so one fork beats twenty.
read -r -d '' JQ_PROGRAM <<'EOF' || true
. as $r
| ($r.context_window // {}) as $cw
| ($cw.current_usage // {}) as $cu
| (($cu.input_tokens // 0)
   + ($cu.cache_creation_input_tokens // 0)
   + ($cu.cache_read_input_tokens // 0)) as $ctx
| ($cw.context_window_size // 200000) as $win
| ($cw.total_input_tokens // 0) as $tot
# used_percentage survives /compact while current_usage goes null, so prefer it
# and only fall back to the per-component sum.
| (if ($cw.used_percentage // null) != null then $cw.used_percentage
   elif $ctx > 0 and $win > 0 then ($ctx * 100 / $win)
   else null end) as $pct
| (if $ctx > 0 then $ctx elif $tot > 0 then $tot else 0 end) as $label
| ($r.prompt_cache // null) as $pc
| def kt: if . >= 1000 then ((. * 10 / 1000 | round) as $t
                             | "\(($t / 10 | floor)).\($t % 10)kt")
          else (. | tostring) end;
[
  ($r.model.display_name // ""),
  (($r.output_style.name // "default") | if . == "default" then "" else . end),
  ($r.effort.level // ""),
  (($r.thinking.enabled // false) | tostring),
  ($r.version // ""),
  ($r.transcript_path // ""),
  ($r.workspace.project_dir // ""),
  (($r.cost.total_cost_usd // 0) * 100 | round / 100 | tostring),
  (if $ctx > 0 then ($ctx | tostring) else "" end),
  (if $pct == null then "" else ($pct | round | tostring) end),
  (if $pct == null then "" else ([($pct / 10 | floor), 10] | min | tostring) end),
  (if $tot > 0 then (if $tot >= 1000 then "\($tot / 1000 | round)kt"
                     else ($tot | tostring) end)
   else "" end),
  (if $label > 0 then ($label | kt) else "" end),
  (($r.exceeds_200k_tokens // false) | tostring),
  (($r.rate_limits.five_hour.used_percentage // null)
   | if . == null then "" else (round | tostring) end),
  (($r.rate_limits.five_hour.resets_at // null)
   | if . == null then "" else (floor | tostring) end),
  (if $pc == null then "false" else "true" end),
  ($pc.ttl // ""),
  (($pc.hit_ratio // null) | if . == null then "" else (. * 100 | round | tostring) end)
] | .[]
EOF

# Populates the J_* globals from $input. main() calls this once up front so the
# command substitutions below inherit the parsed values instead of re-forking;
# the guard also lets each getter stand alone under BATS.
parse_input() {
    if [ -n "${J_PARSED:-}" ]; then
        return 0
    fi

    local -a f
    mapfile -t f < <(printf '%s' "$input" | jq -r "$JQ_PROGRAM")

    J_MODEL="${f[0]-}"
    J_STYLE="${f[1]-}"
    J_EFFORT="${f[2]-}"
    J_THINKING="${f[3]-}"
    J_VERSION="${f[4]-}"
    J_TRANSCRIPT="${f[5]-}"
    J_PROJECT_DIR="${f[6]-}"
    J_COST="${f[7]-}"
    J_CONTEXT="${f[8]-}"
    J_PCT="${f[9]-}"
    J_FILLED="${f[10]-}"
    J_WINDOW_FMT="${f[11]-}"
    J_CONTEXT_FMT="${f[12]-}"
    J_EXCEEDS="${f[13]-}"
    J_RL5H="${f[14]-}"
    J_RL5H_RESET="${f[15]-}"
    J_CACHE="${f[16]-}"
    J_CACHE_TTL="${f[17]-}"
    J_CACHE_HIT="${f[18]-}"

    J_PARSED=1
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
    parse_input

    local model_name style_suffix effort_suffix indicator_suffix
    model_name=$(shorten_bedrock_model "$J_MODEL")

    # Add output style if not default
    if [ -n "$J_STYLE" ]; then
        style_suffix=" ($J_STYLE)"
    else
        style_suffix=""
    fi

    # Add effort level (text) if present
    if [ -n "$J_EFFORT" ]; then
        effort_suffix=" (${J_EFFORT})"
    else
        effort_suffix=""
    fi

    # Add indicator emojis
    indicator_suffix=""
    if [ -n "${CLAUDE_CODE_USE_BEDROCK:-}" ]; then
        indicator_suffix+="🪨"
    fi
    if [ "${ANTHROPIC_BASE_URL:-}" = "https://router.eu.requesty.ai" ]; then
        indicator_suffix+="🔑"
    fi
    if [ "$J_THINKING" = "true" ]; then
        indicator_suffix+="🧠"
    fi
    if [ -n "${CLAUDE_CODE_ENABLE_TELEMETRY:-}" ]; then
        indicator_suffix+="📡"
    fi

    echo "${model_name}${style_suffix}${effort_suffix}${indicator_suffix}"
}

get_project_dir() {
    parse_input

    local dir="$J_PROJECT_DIR"
    if [ -n "${HOME:-}" ] && [ "${dir#"$HOME"}" != "$dir" ]; then
        dir="~${dir#"$HOME"}"
    fi
    echo "$dir"
}

get_version() { parse_input; echo "$J_VERSION"; }
get_cost() { parse_input; echo "$J_COST"; }

get_transcript_id() {
    parse_input

    if [ -z "$J_TRANSCRIPT" ]; then
        echo "$PLACEHOLDER"
        return
    fi
    basename "$J_TRANSCRIPT" ".jsonl" | cut -d'-' -f1
}

get_context_size() {
    parse_input

    if [ -n "$J_CONTEXT" ]; then
        echo "$J_CONTEXT"
    else
        echo "$PLACEHOLDER"
    fi
}

get_context_percentage() {
    parse_input

    if [ -n "$J_PCT" ]; then
        echo "${J_PCT}%"
    else
        echo "$PLACEHOLDER"
    fi
}

get_formatted_context_window() {
    parse_input

    if [ -n "$J_WINDOW_FMT" ]; then
        echo "$J_WINDOW_FMT"
    else
        echo "$PLACEHOLDER"
    fi
}

get_git_branch() {
    git branch --quiet --show-current 2>/dev/null || echo "$PLACEHOLDER"
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

# The bar tracks the percentage (which survives /compact) while the label shows
# the token count, so the two never contradict the neighbouring percentage
# segment.
get_context_with_bar() {
    parse_input

    local label="$J_CONTEXT_FMT"
    if [ -z "$label" ]; then
        label="$PLACEHOLDER"
    fi

    local filled="${J_FILLED:-0}"
    local bar=""
    for ((i = 1; i <= filled; i++)); do bar+="●"; done
    for ((i = filled + 1; i <= 10; i++)); do bar+="○"; done

    echo "${label}[${bar}]"
}

# Shared by every percentage-driven segment: green below 40%, orange below 60%,
# red above, purple when the number is not known yet.
percentage_color() {
    local pct="$1"

    if [ -z "$pct" ]; then
        echo "$PURPLE"
    elif [ "$pct" -gt 60 ]; then
        echo "$RED"
    elif [ "$pct" -gt 40 ]; then
        echo "$ORANGE"
    else
        echo "$GREEN"
    fi
}

get_context_color() { parse_input; percentage_color "$J_PCT"; }

get_rate_limit_5h() {
    parse_input

    if [ -z "$J_RL5H" ]; then
        echo ""
    else
        echo "5h ${J_RL5H}%"
    fi
}

get_rate_limit_5h_color() {
    parse_input

    if [ -z "$J_RL5H" ]; then
        echo "$PURPLE"
    elif [ "$J_RL5H" -gt 75 ]; then
        echo "$RED"
    elif [ "$J_RL5H" -gt 50 ]; then
        echo "$ORANGE"
    else
        echo "$GREEN"
    fi
}

format_duration() {
    local seconds="$1"
    local hours=$((seconds / 3600))
    local minutes=$(((seconds % 3600) / 60))

    if [ "$hours" -gt 0 ]; then
        echo "${hours}h${minutes}m"
    elif [ "$minutes" -gt 0 ]; then
        echo "${minutes}m"
    else
        echo "<1m"
    fi
}

# Time until the 5h window rolls over. Claude Code drops the window once
# resets_at has passed, but a stale value would otherwise render as a negative
# countdown, so treat anything in the past as absent.
get_rate_limit_5h_reset() {
    parse_input

    if [ -z "$J_RL5H_RESET" ]; then
        echo ""
        return
    fi

    local remaining=$((J_RL5H_RESET - EPOCHSECONDS))
    if [ "$remaining" -le 0 ]; then
        echo ""
        return
    fi

    echo "↻$(format_duration "$remaining")"
}

# Prompt cache state: the TTL of the current cached prefix and what fraction of
# input tokens came out of the cache. The glyph is deliberately constant -- the
# segment colour is the signal, so a healthy cache never looks like an alert.
get_cache() {
    parse_input

    if [ "$J_CACHE" != "true" ]; then
        echo ""
        return
    fi

    local out="$CACHE_GLYPH"

    if [ -n "$J_CACHE_TTL" ]; then
        out+="$J_CACHE_TTL"
    fi
    if [ -n "$J_CACHE_HIT" ]; then
        out+=" ${J_CACHE_HIT}%"
    fi

    echo "$out"
}

# Inverted against the other segments: a *high* hit ratio is the good case.
get_cache_color() {
    parse_input

    if [ -z "$J_CACHE_HIT" ]; then
        echo "$PURPLE"
    elif [ "$J_CACHE_HIT" -ge 80 ]; then
        echo "$GREEN"
    elif [ "$J_CACHE_HIT" -ge 50 ]; then
        echo "$ORANGE"
    else
        echo "$RED"
    fi
}

get_exceeds_200k_indicator() {
    parse_input

    if [ "$J_EXCEEDS" = "true" ]; then
        echo "🔥"
    else
        echo ""
    fi
}

# Color functions
bg_color() { printf "\033[48;2;%sm" "$1"; }
fg_color() { printf "\033[38;2;%sm" "$1"; }
segment() { echo -n "$(bg_color "$1")${BLACK_FG} $2 ${RESET}"; }

# A solid arrow needs two different colors to be visible at all; where adjacent
# segments happen to share a background (two green ones, say) fall back to the
# hairline arrow so the boundary does not disappear.
separator() {
    if [ "$1" = "$2" ]; then
        echo -n "$(bg_color "$2")${BLACK_FG}${SEP_THIN}${RESET}"
    else
        echo -n "$(fg_color "$1")$(bg_color "$2")${SEP_THICK}${RESET}"
    fi
}

row_end() { echo -en "$(fg_color "$1")${SEP_THICK}${RESET}"; echo; }

# Main execution function
main() {
    input=$(cat)

    # Parse once here so every command substitution below inherits the values.
    parse_input

    # Cache expensive calculations
    local context_color
    context_color=$(get_context_color)

    # Row 1: Session identity
    echo -en "$(segment "$RED" "$(get_model_name)")"
    echo -en "$(separator "$RED" "$ORANGE")"
    echo -en "$(segment "$ORANGE" "$(get_version)")"
    echo -en "$(separator "$ORANGE" "$PINK")"
    echo -en "$(segment "$PINK" "$(get_transcript_id)")"
    row_end "$PINK"

    # Row 2: Location
    echo -en "$(segment "$BLUE" "$(get_project_dir)")"
    echo -en "$(separator "$BLUE" "$GREEN")"
    echo -en "$(segment "$GREEN" "$(get_git_branch)$(get_git_status)")"
    row_end "$GREEN"

    # Row 3: Cost and context metrics
    echo -en "$(segment "$PURPLE" "$(get_cost)$")"
    local previous_color="$PURPLE"

    # Optional prompt cache segment (only once the cache stats exist)
    local cache cache_color
    cache=$(get_cache)
    if [ -n "$cache" ]; then
        cache_color=$(get_cache_color)
        echo -en "$(separator "$previous_color" "$cache_color")"
        echo -en "$(segment "$cache_color" "$cache")"
        previous_color="$cache_color"
    fi

    # Optional 5h rate limit segment (only when field present)
    local rate_limit_5h rate_limit_5h_color
    rate_limit_5h=$(get_rate_limit_5h)
    if [ -n "$rate_limit_5h" ]; then
        rate_limit_5h+="$(get_rate_limit_5h_reset)"
        rate_limit_5h_color=$(get_rate_limit_5h_color)
        echo -en "$(separator "$previous_color" "$rate_limit_5h_color")"
        echo -en "$(segment "$rate_limit_5h_color" "$rate_limit_5h")"
        previous_color="$rate_limit_5h_color"
    fi

    echo -en "$(separator "$previous_color" "$context_color")"
    echo -en "$(segment "$context_color" "$(get_context_with_bar)$(get_exceeds_200k_indicator)")"
    echo -en "$(separator "$context_color" "$YELLOW")"
    echo -en "$(segment "$YELLOW" "$(get_context_percentage)")"
    echo -en "$(separator "$YELLOW" "$CYAN")"
    echo -en "$(segment "$CYAN" "$(get_formatted_context_window)")"
    row_end "$CYAN"
}

# Only run main if script is executed directly (not sourced for testing)
# When sourced, BASH_SOURCE[0] != $0
# When executed, they're equal OR BASH_SOURCE[0] is empty (piped to bash)
if [ -z "${BASH_SOURCE[0]}" ] || [ "${BASH_SOURCE[0]}" = "${0}" ]; then
    main
fi
