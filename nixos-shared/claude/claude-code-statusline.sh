#!/usr/bin/env bash

# Info: BATS unit tests at ./claude-code-statusline.bats

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
  ($r.workspace.current_dir // $r.workspace.project_dir // ""),
  (($r.cost.total_cost_usd // 0) * 100 | round / 100 | tostring),
  (if $ctx > 0 then ($ctx | tostring) else "" end),
  (if $pct == null then "" else ($pct | round | tostring) end),
  (if $pct == null then "" else ([($pct / 10 | floor), 10] | min | tostring) end),
  (($cw.context_window_size // 0)
   | if . >= 1000000 then ((. / 100000 | round) as $t
                           | if $t % 10 == 0 then "\($t / 10)M"
                             else "\($t / 10 | floor).\($t % 10)M" end)
     elif . > 0 then "\(. / 1000 | round)k"
     else "" end),
  (if $label > 0 then ($label | kt) else "" end),
  (($r.exceeds_200k_tokens // false) | tostring),
  (($r.rate_limits.five_hour.used_percentage // null)
   | if . == null then "" else (round | tostring) end),
  (($r.rate_limits.five_hour.resets_at // null)
   | if . == null then "" else (floor | tostring) end),
  (if $pc == null then "false" else "true" end),
  ($pc.ttl // ""),
  (($pc.hit_ratio // null) | if . == null then "" else (. * 100 | round | tostring) end),
  # Hundredths of a percent: the rounded integer is too coarse to extrapolate
  # from early in the window (1% after 15 minutes would project to 20%).
  (($r.rate_limits.five_hour.used_percentage // null)
   | if . == null then "" else (. * 100 | round | tostring) end),
  # Not `// ""`: jq's alternative operator treats false as absent.
  ($pc.warm | if . == null then "" else tostring end),
  ($pc.caching_observed | if . == null then "" else tostring end),
  ($pc.expires_at | if . == null then "" else (floor | tostring) end),
  # User text from /rename: a newline would add a row, an escape recolour it.
  (($r.session_name // "") | gsub("[[:cntrl:]]"; "")
   | if length > 40 then .[0:39] + "…" else . end),
  (($pc.recache_tokens_if_cold // null) | if . == null then "" else kt end),
  (($r.rate_limits.seven_day.used_percentage // null)
   | if . == null then "" else (round | tostring) end),
  (($r.rate_limits.seven_day.resets_at // null)
   | if . == null then "" else (floor | tostring) end),
  (($r.rate_limits.seven_day.used_percentage // null)
   | if . == null then "" else (. * 100 | round | tostring) end),
  # Unrounded, for the rate limit log: whether these carry decimals is unknown.
  (($r.rate_limits.five_hour.used_percentage // null) | if . == null then "" else tostring end),
  (($r.rate_limits.five_hour.resets_at // null) | if . == null then "" else tostring end),
  (($r.rate_limits.seven_day.used_percentage // null) | if . == null then "" else tostring end),
  (($r.rate_limits.seven_day.resets_at // null) | if . == null then "" else tostring end)
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
    J_DIR="${f[6]-}"
    J_COST="${f[7]-}"
    J_CONTEXT="${f[8]-}"
    J_PCT="${f[9]-}"
    J_FILLED="${f[10]-}"
    J_WINDOW_SIZE="${f[11]-}"
    J_CONTEXT_FMT="${f[12]-}"
    J_EXCEEDS="${f[13]-}"
    J_RL5H="${f[14]-}"
    J_RL5H_RESET="${f[15]-}"
    J_CACHE="${f[16]-}"
    J_CACHE_TTL="${f[17]-}"
    J_CACHE_HIT="${f[18]-}"
    J_RL5H_CENTI="${f[19]-}"
    J_CACHE_WARM="${f[20]-}"
    J_CACHE_OBSERVED="${f[21]-}"
    J_CACHE_EXPIRES="${f[22]-}"
    J_SESSION_NAME="${f[23]-}"
    J_CACHE_RECACHE="${f[24]-}"
    J_RL7D="${f[25]-}"
    J_RL7D_RESET="${f[26]-}"
    J_RL7D_CENTI="${f[27]-}"
    J_RL5H_RAW="${f[28]-}"
    J_RL5H_RAW_RESET="${f[29]-}"
    J_RL7D_RAW="${f[30]-}"
    J_RL7D_RAW_RESET="${f[31]-}"

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

# current_dir rather than project_dir, so a cd into a subdirectory shows.
get_working_dir() {
    parse_input

    local dir="$J_DIR"
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

get_session_name() { parse_input; echo "$J_SESSION_NAME"; }

get_context_size() {
    parse_input

    if [ -n "$J_CONTEXT" ]; then
        echo "$J_CONTEXT"
    else
        echo "$PLACEHOLDER"
    fi
}

get_context_window_size() { parse_input; echo "$J_WINDOW_SIZE"; }

get_git_branch() {
    git branch --quiet --show-current 2>/dev/null || echo "$PLACEHOLDER"
}

get_git_status() {
    local status="" porcelain

    # Porcelain lists untracked files too, which git diff does not. No optional
    # locks: a status refresh must never collide with the user's own git.
    if ! porcelain=$(git --no-optional-locks status --porcelain 2>/dev/null); then
        echo ""
        return
    fi
    if [ -z "$porcelain" ]; then
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

# One segment for the context: tokens, window size, percentage and a bar. The
# bar tracks the percentage (which survives /compact) rather than the tokens.
get_context_with_bar() {
    parse_input

    local label="$J_CONTEXT_FMT"
    if [ -z "$label" ]; then
        label="$PLACEHOLDER"
    fi
    if [ -n "$J_WINDOW_SIZE" ] && [ -n "$J_CONTEXT_FMT" ]; then
        label+="/$J_WINDOW_SIZE"
    fi
    if [ -n "$J_PCT" ]; then
        label+=" ${J_PCT}%"
    fi

    local filled="${J_FILLED:-0}"
    local bar=""
    for ((i = 1; i <= filled; i++)); do bar+="●"; done
    for ((i = filled + 1; i <= 10; i++)); do bar+="○"; done

    echo "${label}[${bar}]"
}

percentage_color() {
    local pct="$1" orange_above="$2" red_above="$3"

    if [ -z "$pct" ]; then
        echo "$PURPLE"
    elif [ "$pct" -gt "$red_above" ]; then
        echo "$RED"
    elif [ "$pct" -gt "$orange_above" ]; then
        echo "$ORANGE"
    else
        echo "$GREEN"
    fi
}

get_context_color() { parse_input; percentage_color "$J_PCT" 40 60; }

readonly RL5H_WINDOW=18000
# Below this the projection is little more than the prior.
readonly RL5H_MIN_ELAPSED=900
# The burn rate is shrunk towards a prior window ending at RL5H_PRIOR_PCT, worth
# RL5H_PRIOR_SECONDS of observation. The window opens on the first request, so
# its start is the busiest stretch by construction; extrapolating that alone
# overshoots. The prior's weight fades as the real elapsed time grows.
readonly RL5H_PRIOR_PCT=50
readonly RL5H_PRIOR_SECONDS=1800
# The prior's usage over RL5H_PRIOR_SECONDS, in hundredths of a percent.
readonly RL5H_PRIOR_CENTI=$((RL5H_PRIOR_PCT * 100 * RL5H_PRIOR_SECONDS / RL5H_WINDOW))

# The weekly window, with the same pace logic. Usage follows the working day, so
# a few hours say little about the week: wait 6h and weigh the prior as a full
# day.
readonly RL7D_WINDOW=604800
readonly RL7D_MIN_ELAPSED=21600
readonly RL7D_PRIOR_PCT=50
readonly RL7D_PRIOR_SECONDS=86400
readonly RL7D_PRIOR_CENTI=$((RL7D_PRIOR_PCT * 100 * RL7D_PRIOR_SECONDS / RL7D_WINDOW))

# The pace functions below take one window's parameters in this order:
#   centi reset window min_elapsed prior_centi prior_seconds
# where centi is the usage in hundredths of a percent and reset its resets_at.
# Always pass them quoted: an unquoted empty centi would shift the rest.

# Seconds elapsed in the window, derived from resets_at (the window opens one
# window length before it resets). Empty while too early to extrapolate from.
rate_limit_elapsed() {
    local centi="$1" reset="$2" window="$3" min_elapsed="$4"
    if [ -z "$centi" ] || [ -z "$reset" ]; then
        return
    fi

    local remaining=$((reset - EPOCHSECONDS))
    local elapsed=$((window - remaining))
    if [ "$remaining" -le 0 ] || [ "$elapsed" -lt "$min_elapsed" ]; then
        return
    fi

    echo "$elapsed"
}

# Usage at reset: what is used so far, plus the prior-shrunk burn rate
# (centi + prior_centi) / (elapsed + prior_seconds) over the rest of the window.
rate_limit_projection() {
    local centi="$1" reset="$2" window="$3" min_elapsed="$4" prior_centi="$5" prior_seconds="$6"

    local elapsed
    elapsed=$(rate_limit_elapsed "$centi" "$reset" "$window" "$min_elapsed")
    if [ -z "$elapsed" ]; then
        echo ""
        return
    fi

    local weight=$((elapsed + prior_seconds))
    local remaining=$((window - elapsed))
    # Round half up; the numerator is in hundredths of a percent.
    echo $(((centi * weight + (centi + prior_centi) * remaining + weight * 50) / (weight * 100)))
}

# Time until 100% at the prior-shrunk burn, but only when that comes before
# the reset -- otherwise the window rolls over first and there is nothing to warn
# about.
rate_limit_warning() {
    local centi="$1" reset="$2" window="$3" min_elapsed="$4" prior_centi="$5" prior_seconds="$6"

    local elapsed
    elapsed=$(rate_limit_elapsed "$centi" "$reset" "$window" "$min_elapsed")
    if [ -z "$elapsed" ] || [ "$centi" -le 0 ]; then
        echo ""
        return
    fi

    local exhausted_in=$(((10000 - centi) * (elapsed + prior_seconds) / (centi + prior_centi)))
    if [ "$exhausted_in" -ge $((reset - EPOCHSECONDS)) ]; then
        echo ""
        return
    fi
    if [ "$exhausted_in" -lt 0 ]; then
        exhausted_in=0
    fi

    echo "⚠$(format_duration "$exhausted_in")"
}

# Time until the window rolls over. Claude Code drops a window once resets_at
# has passed, but a stale value would otherwise render as a negative countdown,
# so treat anything in the past as absent.
rate_limit_reset() {
    local reset="$1"
    if [ -z "$reset" ]; then
        echo ""
        return
    fi

    local remaining=$((reset - EPOCHSECONDS))
    if [ "$remaining" -le 0 ]; then
        echo ""
        return
    fi

    echo "↻$(format_duration "$remaining")"
}

rate_limit_label() {
    local name="$1" pct="$2" projection="$3"
    if [ -z "$pct" ]; then
        echo ""
    elif [ -n "$projection" ]; then
        echo "$name ${pct}%→${projection}%"
    else
        echo "$name ${pct}%"
    fi
}

# Coloured by where the window is heading rather than where it is: 70% with ten
# minutes left is fine, 40% an hour in is not. Falls back to the current
# usage while the projection is still too noisy to trust.
rate_limit_color() {
    local pct="$1" projection="$2"
    if [ -n "$projection" ]; then
        percentage_color "$projection" 80 100
    else
        percentage_color "$pct" 50 75
    fi
}

get_rate_limit_5h_projection() {
    parse_input
    rate_limit_projection "$J_RL5H_CENTI" "$J_RL5H_RESET" "$RL5H_WINDOW" "$RL5H_MIN_ELAPSED" "$RL5H_PRIOR_CENTI" "$RL5H_PRIOR_SECONDS"
}
get_rate_limit_5h_warning() {
    parse_input
    rate_limit_warning "$J_RL5H_CENTI" "$J_RL5H_RESET" "$RL5H_WINDOW" "$RL5H_MIN_ELAPSED" "$RL5H_PRIOR_CENTI" "$RL5H_PRIOR_SECONDS"
}
get_rate_limit_5h_reset() { parse_input; rate_limit_reset "$J_RL5H_RESET"; }
get_rate_limit_5h() { parse_input; rate_limit_label 5h "$J_RL5H" "$(get_rate_limit_5h_projection)"; }
get_rate_limit_5h_color() { parse_input; rate_limit_color "$J_RL5H" "$(get_rate_limit_5h_projection)"; }

get_rate_limit_7d_projection() {
    parse_input
    rate_limit_projection "$J_RL7D_CENTI" "$J_RL7D_RESET" "$RL7D_WINDOW" "$RL7D_MIN_ELAPSED" "$RL7D_PRIOR_CENTI" "$RL7D_PRIOR_SECONDS"
}
get_rate_limit_7d_warning() {
    parse_input
    rate_limit_warning "$J_RL7D_CENTI" "$J_RL7D_RESET" "$RL7D_WINDOW" "$RL7D_MIN_ELAPSED" "$RL7D_PRIOR_CENTI" "$RL7D_PRIOR_SECONDS"
}
get_rate_limit_7d_reset() { parse_input; rate_limit_reset "$J_RL7D_RESET"; }
get_rate_limit_7d() { parse_input; rate_limit_label 7d "$J_RL7D" "$(get_rate_limit_7d_projection)"; }
get_rate_limit_7d_color() { parse_input; rate_limit_color "$J_RL7D" "$(get_rate_limit_7d_projection)"; }

# Appends a row to $XDG_STATE_HOME/claude-code/rate-limits.tsv whenever a window
# moves forward, as data for fitting the prior constants above:
#   window  resets_at  now  used_percentage   (resets_at and usage unrounded)
# The first row per resets_at approximates when the window opened, which
# resets_at itself may not tell (it looked rounded to 10 minutes).
#
# Every open session re-renders every 30 s with the values from its own last
# response, so an idle one keeps reporting old usage. Only a later resets_at, or
# higher usage in the same window, counts as new; rate-limits.last holds the
# newest "reset centi" per window. Builtins only until something is written,
# and it never fails: a lost row beats a broken status line.
rate_limit_is_newer() {
    local reset="$1" centi="$2" last_reset="$3" last_centi="$4"
    [ -n "$reset" ] && [ -n "$centi" ] || return 1
    # An idle session can still report a window that has already reset.
    [ "$reset" -gt "$EPOCHSECONDS" ] || return 1
    [ "$reset" -gt "$last_reset" ] || { [ "$reset" -eq "$last_reset" ] && [ "$centi" -gt "$last_centi" ]; }
}

log_rate_limits() {
    local dir="${XDG_STATE_HOME:-$HOME/.local/state}/claude-code"
    local r5=0 c5=0 r7=0 c7=0 v rows=""
    { read -r r5 c5 r7 c7 <"$dir/rate-limits.last"; } 2>/dev/null || true
    for v in r5 c5 r7 c7; do
        [[ "${!v}" =~ ^[0-9]+$ ]] || printf -v "$v" 0
    done

    if rate_limit_is_newer "$J_RL5H_RESET" "$J_RL5H_CENTI" "$r5" "$c5"; then
        rows+="5h"$'\t'"$J_RL5H_RAW_RESET"$'\t'"$EPOCHSECONDS"$'\t'"$J_RL5H_RAW"$'\n'
        r5="$J_RL5H_RESET" c5="$J_RL5H_CENTI"
    fi
    if rate_limit_is_newer "$J_RL7D_RESET" "$J_RL7D_CENTI" "$r7" "$c7"; then
        rows+="7d"$'\t'"$J_RL7D_RAW_RESET"$'\t'"$EPOCHSECONDS"$'\t'"$J_RL7D_RAW"$'\n'
        r7="$J_RL7D_RESET" c7="$J_RL7D_CENTI"
    fi
    if [ -z "$rows" ]; then
        return 0
    fi

    {
        mkdir -p "$dir" &&
            printf '%s' "$rows" >>"$dir/rate-limits.tsv" &&
            printf '%s %s %s %s\n' "$r5" "$c5" "$r7" "$c7" >"$dir/rate-limits.last"
    } 2>/dev/null || true
}

format_duration() {
    local seconds="$1"
    local hours=$((seconds / 3600))
    local minutes=$(((seconds % 3600) / 60))

    if [ "$hours" -ge 24 ]; then
        echo "$((hours / 24))d$((hours % 24))h"
    elif [ "$hours" -gt 0 ]; then
        echo "${hours}h${minutes}m"
    elif [ "$minutes" -gt 0 ]; then
        echo "${minutes}m"
    else
        echo "<1m"
    fi
}

# Cold means the cached prefix is gone and the next request re-caches it. Only
# when caching was observed at all: a provider that reports no cache tokens is
# not cold, just silent. expires_at is checked too, because warm is only as
# fresh as the last render.
cache_is_cold() {
    if [ "$J_CACHE_OBSERVED" = "false" ]; then
        return 1
    fi
    if [ "$J_CACHE_WARM" = "false" ]; then
        return 0
    fi
    [ -n "$J_CACHE_EXPIRES" ] && [ "$J_CACHE_EXPIRES" -le "$EPOCHSECONDS" ]
}

# Prompt cache state: the TTL of the current cached prefix and what fraction of
# input tokens came out of the cache, or "cold" once the prefix has expired (the
# hit ratio is a session average and says nothing then). The glyph is
# deliberately constant -- the segment colour is the signal, so a healthy cache
# never looks like an alert.
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
    if cache_is_cold; then
        # What the next turn pays for. Not last_miss_cause: that explains the
        # previous miss, not why the cache went cold since.
        out+=" cold"
        if [ -n "$J_CACHE_RECACHE" ]; then
            out+=" +$J_CACHE_RECACHE"
        fi
        echo "$out"
        return
    fi

    if [ -n "$J_CACHE_HIT" ]; then
        out+=" ${J_CACHE_HIT}%"
    fi
    if [ -n "$J_CACHE_EXPIRES" ]; then
        out+=" ⧗$(format_duration $((J_CACHE_EXPIRES - EPOCHSECONDS)))"
    fi

    echo "$out"
}

# Inverted against the other segments: a *high* hit ratio is the good case.
get_cache_color() {
    parse_input

    if cache_is_cold; then
        echo "$RED"
    elif [ -z "$J_CACHE_HIT" ]; then
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
    log_rate_limits || true

    # Cache expensive calculations
    local context_color
    context_color=$(get_context_color)

    # Row 1: Session identity
    echo -en "$(segment "$RED" "$(get_model_name)")"
    echo -en "$(separator "$RED" "$ORANGE")"
    echo -en "$(segment "$ORANGE" "$(get_version)")"
    echo -en "$(separator "$ORANGE" "$PINK")"
    echo -en "$(segment "$PINK" "$(get_transcript_id)")"
    local session_name
    session_name=$(get_session_name)
    if [ -n "$session_name" ]; then
        echo -en "$(separator "$PINK" "$PINK")"
        echo -en "$(segment "$PINK" "$session_name")"
    fi
    row_end "$PINK"

    # Row 2: Location
    echo -en "$(segment "$BLUE" "$(get_working_dir)")"
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

    # Optional rate limit segments (only when the window is reported)
    local window rate_limit rate_limit_color part
    for window in 5h 7d; do
        rate_limit=$("get_rate_limit_$window")
        if [ -z "$rate_limit" ]; then
            continue
        fi
        for part in "$("get_rate_limit_${window}_warning")" "$("get_rate_limit_${window}_reset")"; do
            if [ -n "$part" ]; then
                rate_limit+=" $part"
            fi
        done
        rate_limit_color=$("get_rate_limit_${window}_color")
        echo -en "$(separator "$previous_color" "$rate_limit_color")"
        echo -en "$(segment "$rate_limit_color" "$rate_limit")"
        previous_color="$rate_limit_color"
    done

    echo -en "$(separator "$previous_color" "$context_color")"
    echo -en "$(segment "$context_color" "$(get_context_with_bar)$(get_exceeds_200k_indicator)")"
    row_end "$context_color"
}

# Only run main if script is executed directly (not sourced for testing)
# When sourced, BASH_SOURCE[0] != $0
# When executed, they're equal OR BASH_SOURCE[0] is empty (piped to bash)
if [ -z "${BASH_SOURCE[0]}" ] || [ "${BASH_SOURCE[0]}" = "${0}" ]; then
    main
fi
