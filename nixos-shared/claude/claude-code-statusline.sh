#!/usr/bin/env bash

# Info: BATS unit tests at ./claude-code-statusline.bats

# Color definitions (RGB values)
readonly RED="255;120;120"
readonly ORANGE="255;180;100"
readonly GREEN="120;220;120"
readonly BLUE="100;180;255"
readonly PURPLE="180;140;255"
readonly PINK="255;140;180"
readonly MAGENTA="255;80;200"
readonly DARK="45;45;45"
readonly DIM="110;110;110"

# ANSI escape sequences
readonly RESET=$'\033[0m'
readonly BLACK_FG=$'\033[30m'

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
  ($r.version // ""),
  ($r.transcript_path // ""),
  ($r.workspace.current_dir // $r.workspace.project_dir // ""),
  (($r.cost.total_cost_usd // 0) * 100 | round / 100 | tostring),
  (if $ctx > 0 then ($ctx | tostring) else "" end),
  (if $pct == null then "" else ($pct | round | tostring) end),
  (($cw.context_window_size // 0)
   | if . >= 1000000 then ((. / 100000 | round) as $t
                           | if $t % 10 == 0 then "\($t / 10)M"
                             else "\($t / 10 | floor).\($t % 10)M" end)
     elif . > 0 then "\(. / 1000 | round)k"
     else "" end),
  (if $label > 0 then ($label | kt) else "" end),
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
    J_VERSION="${f[3]-}"
    J_TRANSCRIPT="${f[4]-}"
    J_DIR="${f[5]-}"
    J_COST="${f[6]-}"
    J_CONTEXT="${f[7]-}"
    J_PCT="${f[8]-}"
    J_WINDOW_SIZE="${f[9]-}"
    J_CONTEXT_FMT="${f[10]-}"
    J_RL5H="${f[11]-}"
    J_RL5H_RESET="${f[12]-}"
    J_CACHE="${f[13]-}"
    J_CACHE_TTL="${f[14]-}"
    J_CACHE_HIT="${f[15]-}"
    J_RL5H_CENTI="${f[16]-}"
    J_CACHE_WARM="${f[17]-}"
    J_CACHE_OBSERVED="${f[18]-}"
    J_CACHE_EXPIRES="${f[19]-}"
    J_SESSION_NAME="${f[20]-}"
    J_CACHE_RECACHE="${f[21]-}"
    J_RL7D="${f[22]-}"
    J_RL7D_RESET="${f[23]-}"
    J_RL7D_CENTI="${f[24]-}"
    J_RL5H_RAW="${f[25]-}"
    J_RL5H_RAW_RESET="${f[26]-}"
    J_RL7D_RAW="${f[27]-}"
    J_RL7D_RAW_RESET="${f[28]-}"

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

    local model_name style_suffix indicator_suffix
    model_name=$(shorten_bedrock_model "$J_MODEL")

    # Add output style if not default
    if [ -n "$J_STYLE" ]; then
        style_suffix=" ($J_STYLE)"
    else
        style_suffix=""
    fi

    # Add indicator emojis
    indicator_suffix=""
    if [ -n "${CLAUDE_CODE_USE_BEDROCK:-}" ]; then
        indicator_suffix+="🪨"
    fi
    if [ "${ANTHROPIC_BASE_URL:-}" = "https://router.eu.requesty.ai" ]; then
        indicator_suffix+="🔑"
    fi
    if [ -n "${CLAUDE_CODE_ENABLE_TELEMETRY:-}" ]; then
        indicator_suffix+="📡"
    fi

    echo "${model_name}${style_suffix}${indicator_suffix}"
}

# Empty for a level this script does not know yet.
effort_rank() {
    case "$1" in
        low) echo 1 ;;
        medium) echo 2 ;;
        high) echo 3 ;;
        xhigh) echo 4 ;;
        max) echo 5 ;;
    esac
}

get_effort_color() {
    parse_input
    case "$(effort_rank "$J_EFFORT")" in
        1) echo "$BLUE" ;;
        2) echo "$GREEN" ;;
        3) echo "$ORANGE" ;;
        4) echo "$RED" ;;
        5) echo "$MAGENTA" ;;
        *) echo "$PURPLE" ;;
    esac
}

# A level without a rank falls back to its name rather than vanishing.
get_effort_bar() {
    parse_input

    if [ -z "$J_EFFORT" ]; then
        echo ""
        return
    fi
    local rank
    rank=$(effort_rank "$J_EFFORT")
    if [ -z "$rank" ]; then
        echo "$J_EFFORT"
        return
    fi

    local bar="" i
    for ((i = 1; i <= 5; i++)); do
        if [ "$i" -le "$rank" ]; then bar+="●"; else bar+="○"; fi
    done
    echo "[$bar]"
}

# Dark background, because the level colours vanish on the coloured segments.
effort_segment() {
    local color="$1" bar="$2"
    # Only the filled dots take the colour. An unranked level's name has no
    # brackets and no empty dots, so all of it does.
    local dots="${bar#[}" open="" close=""
    dots="${dots%]}"
    if [ "$dots" != "$bar" ]; then
        open="[" close="]"
    fi
    local filled="${dots%%○*}" dark dim lit
    bg_color "$DARK"
    dark="$REPLY"
    fg_color "$DIM"
    dim="$REPLY"
    fg_color "$color"
    lit="$REPLY"
    OUT+="$dark$dim ${open}$lit${filled}$dim${dots#"$filled"}${close} $RESET"
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
    local id="${J_TRANSCRIPT##*/}"
    id="${id%.jsonl}"
    echo "${id%%-*}"
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

# Branch, changes and distance to the upstream from one porcelain v2 call, which
# main makes once. Porcelain lists untracked files too, which git diff does not.
# No optional locks: a status refresh must never collide with the user's own
# git. A detached HEAD shows its commit, as @abc1234.
parse_git() {
    if [ -n "${G_PARSED:-}" ]; then
        return 0
    fi
    G_PARSED=1
    G_BRANCH="$PLACEHOLDER"
    G_STATUS=""

    local porcelain
    if ! porcelain=$(git --no-optional-locks status --porcelain=v2 --branch 2>/dev/null); then
        return 0
    fi

    local line head="" oid="" ab="" dirty=""
    while IFS= read -r line; do
        case "$line" in
            "# branch.oid "*) oid="${line#"# branch.oid "}" ;;
            "# branch.head "*) head="${line#"# branch.head "}" ;;
            "# branch.ab "*) ab="${line#"# branch.ab "}" ;;
            "#"* | "") ;;
            *) dirty=1 ;;
        esac
    done <<<"$porcelain"

    if [ "$head" = "(detached)" ]; then
        G_BRANCH="@${oid:0:7}"
    else
        G_BRANCH="$head"
    fi
    if [ -n "$dirty" ]; then
        G_STATUS="±"
    else
        G_STATUS="✓"
    fi

    # branch.ab reads "+<ahead> -<behind>" and is absent without an upstream.
    local ahead="${ab%% *}" behind="${ab##* }"
    ahead="${ahead#+}"
    behind="${behind#-}"
    if [ "${ahead:-0}" -gt 0 ]; then
        G_STATUS+="${ahead}↑"
    fi
    if [ "${behind:-0}" -gt 0 ]; then
        G_STATUS+="${behind}↓"
    fi
}

get_git_branch() { parse_git; echo "$G_BRANCH"; }
get_git_status() { parse_git; echo "$G_STATUS"; }

get_context_label() {
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
    echo "$label"
}

percentage_color() {
    local value="$1" orange_above="$2" red_above="$3"

    if [ -z "$value" ]; then
        echo "$PURPLE"
    elif [ "$value" -gt "$red_above" ]; then
        echo "$RED"
    elif [ "$value" -gt "$orange_above" ]; then
        echo "$ORANGE"
    else
        echo "$GREEN"
    fi
}

# The worse of two scales: the share of the window, and the size itself, which
# turns orange where exceeds_200k_tokens trips. On a 1M window the share alone
# stays green up to 400k.
get_context_color() {
    parse_input
    local share size color
    share=$(percentage_color "$J_PCT" 40 60)
    size=$(percentage_color "$J_CONTEXT" 200000 400000)
    for color in "$RED" "$ORANGE" "$GREEN"; do
        if [ "$share" = "$color" ] || [ "$size" = "$color" ]; then
            echo "$color"
            return
        fi
    done
    echo "$PURPLE"
}

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

# The rendering helpers append to OUT instead of printing, so no colour code
# costs a subshell, and nothing user-written passes through echo -e, which
# turned a "\n" in a /rename title into an extra row.
bg_color() { REPLY=$'\033[48;2;'"${1}m"; }
fg_color() { REPLY=$'\033[38;2;'"${1}m"; }

segment() {
    bg_color "$1"
    OUT+="$REPLY$BLACK_FG $2 $RESET"
}

# A solid arrow needs two different colors to be visible at all; where adjacent
# segments happen to share a background (two green ones, say) fall back to the
# hairline arrow so the boundary does not disappear.
separator() {
    if [ "$1" = "$2" ]; then
        bg_color "$2"
        OUT+="$REPLY$BLACK_FG$SEP_THIN$RESET"
    else
        local from
        fg_color "$1"
        from="$REPLY"
        bg_color "$2"
        OUT+="$from$REPLY$SEP_THICK$RESET"
    fi
}

row_end() {
    fg_color "$1"
    OUT+="$REPLY$SEP_THICK$RESET"$'\n'
}

# Claude Code indents the status line and cuts rows longer than COLUMNS - 4
# with "…" (2.1.296 at 70 columns: 66 fit, 67 got cut). Zero while COLUMNS is
# unknown, and then no row sheds anything.
row_budget() {
    if [[ "${COLUMNS:-}" =~ ^[0-9]+$ ]] && [ "$COLUMNS" -gt 4 ]; then
        ROW_BUDGET=$((COLUMNS - 4))
    else
        ROW_BUDGET=0
    fi
}

# Widths count characters, which takes a UTF-8 locale: under C, ● counts as 3.
use_utf8_locale() {
    local probe="●"
    if [ "${#probe}" -ne 1 ]; then
        export LC_ALL=C.UTF-8
    fi
}

# Columns of plain text as Claude Code measures it: one per character, two for
# the indicator emoji.
text_width() {
    local wide="${1//[^🪨🔑📡]/}"
    REPLY=$((${#1} + ${#wide}))
}

# The row being built, as parallel arrays: background, text, and for the effort
# segment the colour of its filled dots.
reset_row() {
    SEG_COLOR=()
    SEG_TEXT=()
    SEG_DOTS=()
}

add_segment() {
    SEG_COLOR+=("$1")
    SEG_TEXT+=("$2")
    SEG_DOTS+=("${3-}")
}

# A segment pads its text with a space on each side and is followed by an
# arrow: a separator, or the row's end.
row_width() {
    local text width=0
    for text in "${SEG_TEXT[@]}"; do
        text_width "$text"
        width=$((width + REPLY + 3))
    done
    REPLY="$width"
}

render_row() {
    local i previous=""
    for i in "${!SEG_TEXT[@]}"; do
        if [ -n "$previous" ]; then
            separator "$previous" "${SEG_COLOR[i]}"
        fi
        if [ -n "${SEG_DOTS[i]}" ]; then
            effort_segment "${SEG_DOTS[i]}" "${SEG_TEXT[i]}"
        else
            segment "${SEG_COLOR[i]}" "${SEG_TEXT[i]}"
        fi
        previous="${SEG_COLOR[i]}"
    done
    row_end "$previous"
}

# Builds a row at shed level 0, 1, ... until it fits ROW_BUDGET, then renders
# it. A row still too wide at its last level is left for Claude Code to cut.
fit_row() {
    local build="$1" levels="$2" level
    for ((level = 0; level < levels; level++)); do
        "$build" "$level"
        row_width
        if [ "$ROW_BUDGET" -eq 0 ] || [ "$REPLY" -le "$ROW_BUDGET" ]; then
            break
        fi
    done
    render_row
}

# Row 1, session identity. Sheds the version, then the transcript id, then
# shortens the session name to the room left.
build_row1() {
    local level="$1"
    reset_row
    add_segment "$RED" "$S_MODEL"
    if [ -n "$S_EFFORT" ]; then
        add_segment "$DARK" "$S_EFFORT" "$S_EFFORT_COLOR"
    fi
    if [ "$level" -lt 1 ]; then
        add_segment "$ORANGE" "$J_VERSION"
    fi
    if [ "$level" -lt 2 ]; then
        add_segment "$PINK" "$S_TRANSCRIPT_ID"
    fi
    if [ -z "$J_SESSION_NAME" ]; then
        return 0
    fi
    local name="$J_SESSION_NAME"
    if [ "$level" -ge 3 ]; then
        row_width
        local room=$((ROW_BUDGET - REPLY - 3))
        text_width "$name"
        if [ "$REPLY" -gt "$room" ]; then
            if [ "$room" -lt 2 ]; then
                return 0
            fi
            name="${name:0:room-1}"
            name="${name% }…"
        fi
    fi
    add_segment "$PINK" "$name"
}

# Row 2, location. Shortens a long directory to its last two parts.
build_row2() {
    local level="$1" dir="$S_WORKING_DIR"
    reset_row
    if [ "$level" -ge 1 ]; then
        local parent="${dir%/*}" short
        short="…/${parent##*/}/${dir##*/}"
        if [ "${#short}" -lt "${#dir}" ]; then
            dir="$short"
        fi
    fi
    add_segment "$BLUE" "$dir"
    add_segment "$GREEN" "$G_BRANCH$G_STATUS"
}

# Row 3, context and budget, most important first, since Claude Code cuts an
# overlong row from the right. Sheds the countdowns, then the cost, then a
# cache that is not cold, then the projections. The context, the usage and the
# ⚠ warnings always stay.
build_row3() {
    local level="$1" window text
    reset_row
    add_segment "$S_CONTEXT_COLOR" "$S_CONTEXT"
    for window in 5h 7d; do
        text="${S_RL_LABEL[$window]}"
        if [ -z "$text" ]; then
            continue
        fi
        if [ "$level" -ge 4 ]; then
            text="${text%%→*}"
        fi
        if [ -n "${S_RL_WARNING[$window]}" ]; then
            text+=" ${S_RL_WARNING[$window]}"
        fi
        if [ -n "${S_RL_RESET[$window]}" ] && [ "$level" -lt 1 ]; then
            text+=" ${S_RL_RESET[$window]}"
        fi
        add_segment "${S_RL_COLOR[$window]}" "$text"
    done
    if [ -n "$S_CACHE" ] && { [ "$level" -lt 3 ] || cache_is_cold; }; then
        text="$S_CACHE"
        if [ "$level" -ge 1 ]; then
            text="${text% ⧗*}"
        fi
        add_segment "$S_CACHE_COLOR" "$text"
    fi
    if [ "$level" -lt 2 ]; then
        add_segment "$PURPLE" "${J_COST}$"
    fi
}

# Main execution function
main() {
    use_utf8_locale
    input=$(cat)

    # Parse once here so every command substitution below inherits the values.
    parse_input
    parse_git
    log_rate_limits || true

    # What the rows can show, computed once; fit_row then picks what fits.
    S_MODEL=$(get_model_name)
    S_EFFORT=$(get_effort_bar)
    S_EFFORT_COLOR=""
    if [ -n "$S_EFFORT" ]; then
        S_EFFORT_COLOR=$(get_effort_color)
    fi
    S_TRANSCRIPT_ID=$(get_transcript_id)
    S_WORKING_DIR=$(get_working_dir)
    S_CONTEXT=$(get_context_label)
    S_CONTEXT_COLOR=$(get_context_color)
    S_CACHE=$(get_cache)
    S_CACHE_COLOR=""
    if [ -n "$S_CACHE" ]; then
        S_CACHE_COLOR=$(get_cache_color)
    fi
    # The label stays empty while a window is not reported.
    declare -gA S_RL_LABEL=() S_RL_WARNING=() S_RL_RESET=() S_RL_COLOR=()
    local window
    for window in 5h 7d; do
        S_RL_LABEL[$window]=$("get_rate_limit_$window")
        S_RL_WARNING[$window]="" S_RL_RESET[$window]="" S_RL_COLOR[$window]=""
        if [ -n "${S_RL_LABEL[$window]}" ]; then
            S_RL_WARNING[$window]=$("get_rate_limit_${window}_warning")
            S_RL_RESET[$window]=$("get_rate_limit_${window}_reset")
            S_RL_COLOR[$window]=$("get_rate_limit_${window}_color")
        fi
    done

    row_budget
    OUT=""
    fit_row build_row1 4
    fit_row build_row2 2
    fit_row build_row3 5
    printf '%s' "$OUT"
}

# Only run main if script is executed directly (not sourced for testing)
# When sourced, BASH_SOURCE[0] != $0
# When executed, they're equal OR BASH_SOURCE[0] is empty (piped to bash)
if [ -z "${BASH_SOURCE[0]}" ] || [ "${BASH_SOURCE[0]}" = "${0}" ]; then
    main
fi
