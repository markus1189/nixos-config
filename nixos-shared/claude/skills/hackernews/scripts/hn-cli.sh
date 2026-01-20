#!/usr/bin/env nix
#! nix shell nixpkgs#bash nixpkgs#curl nixpkgs#jq nixpkgs#coreutils nixpkgs#gawk nixpkgs#parallel --command bash
# shellcheck shell=bash
set -uo pipefail

# Hacker News Top Stories & Comments Viewer

readonly API_BASE="https://hacker-news.firebaseio.com/v0"
readonly ALGOLIA_API="https://hn.algolia.com/api/v1"

# Colors
readonly RESET=$'\033[0m'
readonly BOLD=$'\033[1m'
readonly DIM=$'\033[2m'
readonly ORANGE=$'\033[38;5;208m'
readonly GRAY=$'\033[38;5;245m'
readonly WHITE=$'\033[38;5;255m'
readonly CYAN=$'\033[38;5;81m'
readonly FLAME='🔥'

# Hot thresholds (matching Materialistic app)
readonly HOT_THRESHOLD=100
readonly HOT_FACTOR=3
readonly SCORE_HOT_THRESHOLD=$((HOT_THRESHOLD * HOT_FACTOR))  # 300
readonly COMMENT_HOT_THRESHOLD=$HOT_THRESHOLD                  # 100

# Terminal width for text wrapping
readonly TERM_WIDTH="${COLUMNS:-80}"

# Tree drawing characters
readonly TREE_PIPE="│"
readonly TREE_BRANCH="├─"
readonly TREE_LAST="└─"
readonly TREE_SPACE="  "

# Default values
DEFAULT_STORY_COUNT=20
DEFAULT_COMMENT_DEPTH=1
DEFAULT_MAX_COMMENTS=20
HOT_ONLY=false

# Global counter for comments fetched
COMMENTS_FETCHED=0

show_help() {
    cat <<EOF
Usage: ${0##*/} [OPTIONS] [COUNT]
       ${0##*/} --comments ID [OPTIONS]
       ${0##*/} --search QUERY [OPTIONS]

Fetch and display Hacker News stories or comments.

Modes:
  (default)         List top stories
  -c, --comments ID View comments for a story by ID
  -s, --search QUERY  Search stories via Algolia

Story Options:
  COUNT             Number of stories to display (default: ${DEFAULT_STORY_COUNT}, max: 500)
  --hot             Only show 'hot' stories (${FLAME} score >= ${SCORE_HOT_THRESHOLD} or comments >= ${COMMENT_HOT_THRESHOLD})

Search Options:
  -s, --search QUERY  Search for stories matching QUERY
  --sort MODE         Sort search results: 'date' (default) or 'popular'
  COUNT               Number of results to display (default: ${DEFAULT_STORY_COUNT}, max: 100)

Comment Options:
  -d, --depth N     Maximum comment depth to display (default: ${DEFAULT_COMMENT_DEPTH})
  -n, --max-comments N  Maximum total comments to fetch (default: ${DEFAULT_MAX_COMMENTS})

General Options:
  -h, --help        Show this help message

Examples:
  ${0##*/}                    # Show top 20 stories
  ${0##*/} 50                 # Show top 50 stories
  ${0##*/} --hot              # Show 20 hot stories
  ${0##*/} --hot 5           # Show 5 hot stories
  ${0##*/} -c 46691835        # View comments for story ID 46691835
  ${0##*/} --comments 46691835 --depth 3   # Limit to 3 levels deep
  ${0##*/} -c 46691835 -n 100              # Fetch up to 100 comments
  ${0##*/} -s "rust programming"           # Search for Rust stories (recent first)
  ${0##*/} --search "AI" --sort popular    # Search for AI stories (by popularity)
  ${0##*/} -s "nix" 50                     # Search Nix stories, show 50 results
EOF
}

# Format relative time like the app does
relative_time() {
    local timestamp=$1
    local now
    now=$(date +%s)
    local diff=$((now - timestamp))

    if ((diff < 60)); then
        echo "${diff}s"
    elif ((diff < 3600)); then
        echo "$((diff / 60))m"
    elif ((diff < 86400)); then
        echo "$((diff / 3600))h"
    elif ((diff < 604800)); then
        echo "$((diff / 86400))d"
    elif ((diff < 2592000)); then
        echo "$((diff / 604800))w"
    elif ((diff < 31536000)); then
        echo "$((diff / 2592000))mo"
    else
        echo "$((diff / 31536000))y"
    fi
}

# Extract domain from URL
get_domain() {
    local url=$1
    if [[ -z "$url" || "$url" == "null" ]]; then
        echo ""
        return
    fi
    echo "$url" | sed -E 's|^https?://([^/]+).*|\1|' | sed 's/^www\.//'
}

# Word-wrap text with prefix for each line
# Args: text, prefix, width
wrap_text() {
    local text="$1"
    local prefix="$2"
    local width=$3
    local available=$((width - ${#prefix}))

    # Strip HTML tags and decode common entities
    text=$(echo "$text" | sed -E 's/<[^>]+>//g' | \
        sed 's/&gt;/>/g; s/&lt;/</g; s/&amp;/\&/g; s/&quot;/"/g; s/&#x27;/'\''/g; s/&#39;/'\''/g; s/&nbsp;/ /g')

    # Handle empty text
    if [[ -z "$text" ]]; then
        return
    fi

    # Wrap using fold and add prefix
    echo "$text" | fold -s -w "$available" | while IFS= read -r line; do
        echo "${prefix}${line}"
    done
}

# Fetch and display a single comment recursively
# Args: comment_id, depth, max_depth, prefix, is_last, max_comments
fetch_comment() {
    local id=$1
    local depth=$2
    local max_depth=$3
    local prefix=$4
    local is_last=$5
    local max_comments=$6

    # Stop if we've reached max depth
    if ((depth > max_depth)); then
        return
    fi

    # Stop if we've fetched enough comments
    if ((COMMENTS_FETCHED >= max_comments)); then
        if ((COMMENTS_FETCHED == max_comments)); then
            echo -e "\n${DIM}... stopped after ${max_comments} comments (use -n/--max-comments to fetch more)${RESET}"
            ((COMMENTS_FETCHED++))  # Increment to avoid printing message again
        fi
        return
    fi

    local json
    if ! json=$(curl --fail -s --max-time 10 "${API_BASE}/item/${id}.json" 2>/dev/null); then
        return
    fi

    if [[ -z "$json" || "$json" == "null" ]]; then
        return
    fi

    # Parse comment fields
    local parsed
    parsed=$(echo "$json" | jq -r '[
        .by // "",
        .time // 0,
        .text // "",
        .deleted // false,
        .dead // false,
        ((.kids // []) | map(tostring) | join(","))
    ] | @tsv')

    # Use awk to parse TSV correctly (handles empty fields)
    local by time_stamp text deleted dead kids_str
    by=$(echo "$parsed" | awk -F'\t' '{print $1}')
    time_stamp=$(echo "$parsed" | awk -F'\t' '{print $2}')
    text=$(echo "$parsed" | awk -F'\t' '{print $3}')
    deleted=$(echo "$parsed" | awk -F'\t' '{print $4}')
    dead=$(echo "$parsed" | awk -F'\t' '{print $5}')
    kids_str=$(echo "$parsed" | awk -F'\t' '{print $6}')

    # Skip deleted/dead comments
    if [[ "$deleted" == "true" || "$dead" == "true" ]]; then
        return
    fi

    # Skip comments with no author (shouldn't happen but safety first)
    if [[ -z "$by" ]]; then
        return
    fi

    local time_ago
    time_ago=$(relative_time "$time_stamp")

    # Determine tree branch character
    local branch
    if [[ "$is_last" == "true" ]]; then
        branch="${TREE_LAST}"
    else
        branch="${TREE_BRANCH}"
    fi

    # Print comment header
    echo -e "${prefix}${branch} ${CYAN}${by}${RESET} ${DIM}·${RESET} ${GRAY}${time_ago}${RESET}"

    # Increment global counter
    ((COMMENTS_FETCHED++))

    # Calculate new prefix for content and children
    local content_prefix child_prefix
    if [[ "$is_last" == "true" ]]; then
        content_prefix="${prefix}${TREE_SPACE} "
        child_prefix="${prefix}${TREE_SPACE} "
    else
        content_prefix="${prefix}${TREE_PIPE}  "
        child_prefix="${prefix}${TREE_PIPE}  "
    fi

    # Print wrapped comment text
    if [[ -n "$text" ]]; then
        wrap_text "$text" "${content_prefix}" "$TERM_WIDTH"
        echo "${content_prefix}"
    fi

    # Recursively fetch child comments
    if [[ -n "$kids_str" ]]; then
        IFS=',' read -ra kids <<< "$kids_str"
        local num_kids=${#kids[@]}
        local i=0
        for kid_id in "${kids[@]}"; do
            ((i++))
            local kid_is_last="false"
            if ((i == num_kids)); then
                kid_is_last="true"
            fi
            fetch_comment "$kid_id" $((depth + 1)) "$max_depth" "$child_prefix" "$kid_is_last" "$max_comments"
        done
    fi
}

# Fetch and display comments for a story
show_comments() {
    local story_id=$1
    local max_depth=$2
    local max_comments=$3

    echo -e "${DIM}Fetching story ${story_id}...${RESET}"

    # Fetch the story first
    local json
    if ! json=$(curl --fail -s --max-time 10 "${API_BASE}/item/${story_id}.json"); then
        echo -e "${ORANGE}Error: Failed to fetch story ${story_id}. Check the ID or try again.${RESET}" >&2
        exit 1
    fi

    if [[ -z "$json" || "$json" == "null" ]]; then
        echo -e "${ORANGE}Error: Story ${story_id} not found.${RESET}" >&2
        exit 1
    fi

    # Parse story fields
    local parsed
    parsed=$(echo "$json" | jq -r '[
        .title // "untitled",
        .score // 0,
        .by // "unknown",
        .time // 0,
        .descendants // 0,
        .url // "",
        .type // "story",
        .text // "",
        ((.kids // []) | map(tostring) | join(","))
    ] | @tsv')

    # Use awk to parse TSV correctly (handles empty fields)
    local title score by time_stamp descendants url item_type text kids_str
    title=$(echo "$parsed" | awk -F'\t' '{print $1}')
    score=$(echo "$parsed" | awk -F'\t' '{print $2}')
    by=$(echo "$parsed" | awk -F'\t' '{print $3}')
    time_stamp=$(echo "$parsed" | awk -F'\t' '{print $4}')
    descendants=$(echo "$parsed" | awk -F'\t' '{print $5}')
    url=$(echo "$parsed" | awk -F'\t' '{print $6}')
    item_type=$(echo "$parsed" | awk -F'\t' '{print $7}')
    text=$(echo "$parsed" | awk -F'\t' '{print $8}')
    kids_str=$(echo "$parsed" | awk -F'\t' '{print $9}')

    # Verify it's a story-type item
    if [[ "$item_type" == "comment" ]]; then
        echo -e "${ORANGE}Error: ID ${story_id} is a comment, not a story.${RESET}" >&2
        echo -e "${DIM}Hint: Use the parent story ID to view all comments.${RESET}" >&2
        exit 1
    fi

    local time_ago domain
    time_ago=$(relative_time "$time_stamp")
    domain=$(get_domain "$url")

    # Display story header
    echo ""
    echo -e "${BOLD}${title}${RESET}"
    if [[ -n "$domain" && "$domain" != "null" ]]; then
        echo -e "${DIM}${url}${RESET}"
    fi
    echo -e "${GRAY}${score} points${RESET} ${DIM}·${RESET} ${GRAY}${by}${RESET} ${DIM}·${RESET} ${GRAY}${time_ago}${RESET} ${DIM}·${RESET} ${GRAY}${descendants} comments${RESET}"

    # If it's an Ask HN or similar with text, show it
    if [[ -n "$text" ]]; then
        echo ""
        wrap_text "$text" "" "$TERM_WIDTH"
    fi

    echo ""
    echo -e "${DIM}────────────────────────────────────────${RESET}"
    echo ""

    # Fetch and display comments
    if [[ -z "$kids_str" ]]; then
        echo -e "${GRAY}No comments yet.${RESET}"
        return
    fi

    echo -e "${DIM}Loading comments (depth: ${max_depth}, max: ${max_comments})...${RESET}"
    echo ""

    # Reset global counter
    COMMENTS_FETCHED=0

    IFS=',' read -ra kids <<< "$kids_str"
    local num_kids=${#kids[@]}
    local i=0
    for kid_id in "${kids[@]}"; do
        ((i++))
        local is_last="false"
        if ((i == num_kids)); then
            is_last="true"
        fi
        fetch_comment "$kid_id" 1 "$max_depth" "" "$is_last" "$max_comments"
    done
}

# Fetch and format a single story for list display
# shellcheck disable=SC2329
fetch_and_format_story() {
    local rank=$1
    local id=$2

    local json
    if ! json=$(curl --fail -s --max-time 10 "${API_BASE}/item/${id}.json"); then
        echo "Warning: Failed to fetch story ${id}" >&2
        return
    fi

    if [[ -z "$json" || "$json" == "null" ]]; then
        return
    fi

    # Parse all fields in a single jq call
    local parsed
    parsed=$(echo "$json" | jq -r '[
        .title // "untitled",
        .score // 0,
        .by // "unknown",
        .time // 0,
        .descendants // 0,
        .url // "",
        .type // "story",
        .id // 0
    ] | @tsv')

    local title score by time_stamp descendants url item_type story_id
    IFS=$'\t' read -r title score by time_stamp descendants url item_type story_id <<< "$parsed"

    # Skip stories with invalid/empty IDs
    if [[ -z "$story_id" || "$story_id" == "0" || "$story_id" == "null" ]]; then
        return
    fi

    local domain
    domain=$(get_domain "$url")

    local time_ago
    time_ago=$(relative_time "$time_stamp")

    # Determine if story is hot
    local is_hot=false
    if ((score >= SCORE_HOT_THRESHOLD)) || ((descendants >= COMMENT_HOT_THRESHOLD)); then
        is_hot=true
    fi

    # If hot filter is enabled and story is not hot, skip it
    if [[ "$HOT_FILTER" == "true" && "$is_hot" == "false" ]]; then
        return
    fi

    # Determine if score is hot
    local score_display
    if ((score >= SCORE_HOT_THRESHOLD)); then
        score_display="${ORANGE}${FLAME} ${score} points${RESET}"
    else
        score_display="${GRAY}${score} points${RESET}"
    fi

    # Determine if comments are hot
    local comment_display
    if ((descendants >= COMMENT_HOT_THRESHOLD)); then
        comment_display="${ORANGE}${FLAME} ${descendants}${RESET}"
    else
        comment_display="${GRAY}${descendants}${RESET}"
    fi

    # Format rank with padding
    local rank_display
    rank_display=$(printf "%3d" "$rank")

    # Domain display
    local domain_display=""
    if [[ -n "$domain" && "$domain" != "null" ]]; then
        domain_display="${DIM}(${domain})${RESET}"
    elif [[ "$item_type" == "job" ]]; then
        domain_display="${DIM}(job)${RESET}"
    else
        domain_display="${DIM}(self)${RESET}"
    fi

    # URL for display
    local display_url
    if [[ -n "$url" && "$url" != "null" ]]; then
        display_url="$url"
    else
        display_url="https://news.ycombinator.com/item?id=${story_id}"
    fi

    # HN comments URL
    local hn_url="https://news.ycombinator.com/item?id=${story_id}"

    # Output as: RANK<TAB>LINE1<UNIT_SEP>LINE2<UNIT_SEP>LINE3<UNIT_SEP>LINE4
    local line1 line2 line3 line4
    line1="${WHITE}${rank_display}${RESET} ${BOLD}${title}${RESET} ${domain_display} ${DIM}[${story_id}]${RESET}"
    line2="    ${score_display} · ${GRAY}${by}${RESET} · ${GRAY}${time_ago}${RESET} · ${comment_display} comments"
    line3="    ${DIM}${display_url}${RESET}"
    line4="    ${DIM}${hn_url}${RESET}"

    printf "%03d\t%s\x1f%s\x1f%s\x1f%s\n" "$rank" "$line1" "$line2" "$line3" "$line4"
}

# Search stories via Algolia API
# Args: query, limit, sort_mode (date|popular)
search_stories() {
    local query=$1
    local limit=$2
    local sort_mode=$3

    # URL-encode the query
    local encoded_query
    encoded_query=$(printf '%s' "$query" | jq -sRr @uri)

    # Choose endpoint based on sort mode
    local endpoint
    if [[ "$sort_mode" == "popular" ]]; then
        endpoint="search"
        echo -e "${BOLD}Hacker News Search: ${CYAN}\"${query}\"${RESET} ${DIM}(by popularity)${RESET}"
    else
        endpoint="search_by_date"
        echo -e "${BOLD}Hacker News Search: ${CYAN}\"${query}\"${RESET} ${DIM}(by date)${RESET}"
    fi
    echo -e "${DIM}Searching for up to ${limit} stories...${RESET}"
    echo ""

    # Algolia returns max 100 results per page
    local hits_per_page=$limit
    if ((hits_per_page > 100)); then
        hits_per_page=100
    fi

    # Fetch search results from Algolia
    local search_url="${ALGOLIA_API}/${endpoint}?query=${encoded_query}&tags=story&hitsPerPage=${hits_per_page}"
    local response
    if ! response=$(curl --fail -s --max-time 15 "$search_url"); then
        echo -e "${ORANGE}Error: Failed to search. Algolia API may be down or unreachable.${RESET}" >&2
        exit 1
    fi

    # Extract story IDs from search results (filter out empty/null IDs)
    local ids
    ids=$(echo "$response" | jq -r '.hits[].objectID // empty' 2>/dev/null | grep -v '^$')

    if [[ -z "$ids" ]]; then
        echo -e "${GRAY}No stories found matching \"${query}\".${RESET}"
        echo ""
        echo -e "${DIM}Tips:${RESET}"
        echo -e "${DIM}  - Try different keywords${RESET}"
        echo -e "${DIM}  - Use --sort popular for relevance-based results${RESET}"
        return
    fi

    local total_hits
    total_hits=$(echo "$response" | jq -r '.nbHits // 0')
    echo -e "${DIM}Found ${total_hits} total matches, showing top ${limit}${RESET}"
    echo ""

    # Create numbered list for parallel processing
    local rank=0
    local numbered_ids=""
    while IFS= read -r id; do
        [[ -z "$id" ]] && continue
        ((rank++))
        numbered_ids+="${rank} ${id}"$'\n'
        if ((rank >= limit)); then
            break
        fi
    done <<< "$ids"

    # Export functions and variables for parallel
    export -f fetch_and_format_story relative_time get_domain
    export API_BASE SCORE_HOT_THRESHOLD COMMENT_HOT_THRESHOLD TERM_WIDTH
    export RESET BOLD DIM ORANGE GRAY WHITE FLAME
    export HOT_FILTER="false"

    # Fetch all stories in parallel, sort by rank to preserve search order, then display
    echo -n "$numbered_ids" | \
        parallel --colsep ' ' -j 20 fetch_and_format_story {1} {2} 2>/dev/null | \
        sort -t$'\t' -k1 -n | \
        cut -f2- | \
        while IFS= read -r line; do
            echo -e "${line//$'\x1f'/$'\n'}"
            echo ""
        done

    echo -e "${DIM}Tip: Use '${0##*/} --comments ID' to view comments for a story${RESET}"
}

# Show top stories list
show_stories() {
    local limit=$1
    local hot_filter=$2

    # For hot filtering, we need to fetch more stories to find enough hot ones
    local fetch_limit=$limit
    if [[ "$hot_filter" == "true" ]]; then
        # Fetch up to 500 to find enough hot stories
        fetch_limit=500
        echo -e "${BOLD}Hacker News ${ORANGE}${FLAME} Hot${RESET}${BOLD} Stories${RESET}"
        echo -e "${DIM}Finding ${limit} hot stories...${RESET}"
    else
        echo -e "${BOLD}Hacker News Top Stories${RESET}"
        echo -e "${DIM}Fetching top ${limit} stories...${RESET}"
    fi
    echo ""

    # Fetch story IDs
    local ids raw_ids
    if ! raw_ids=$(curl --fail -s --max-time 10 "${API_BASE}/topstories.json"); then
        echo -e "${ORANGE}Error: Failed to fetch top stories. HN API may be down or unreachable.${RESET}" >&2
        exit 1
    fi

    ids=$(echo "$raw_ids" | jq -r ".[:${fetch_limit}][]")

    if [[ -z "$ids" ]]; then
        echo -e "${ORANGE}Error: No stories returned from API.${RESET}" >&2
        exit 1
    fi

    # Create numbered list for parallel processing
    local rank=0
    local numbered_ids=""
    while IFS= read -r id; do
        [[ -z "$id" ]] && continue
        ((rank++))
        numbered_ids+="${rank} ${id}"$'\n'
    done <<< "$ids"

    # Export functions and variables for parallel
    export -f fetch_and_format_story relative_time get_domain
    export API_BASE SCORE_HOT_THRESHOLD COMMENT_HOT_THRESHOLD TERM_WIDTH
    export RESET BOLD DIM ORANGE GRAY WHITE FLAME
    export HOT_FILTER="$hot_filter"

    # Fetch all stories in parallel, sort by rank, filter/limit, then display
    # shellcheck disable=SC1083
    local count=0
    echo -n "$numbered_ids" | \
        parallel --colsep ' ' -j 20 fetch_and_format_story {1} {2} 2>/dev/null | \
        sort -t$'\t' -k1 -n | \
        cut -f2- | \
        while IFS= read -r line; do
            echo -e "${line//$'\x1f'/$'\n'}"
            echo ""
            ((count++))
            if ((count >= limit)); then
                break
            fi
        done

    echo -e "${DIM}Tip: Use '${0##*/} --comments ID' to view comments for a story${RESET}"
}

# Parse arguments
MODE="stories"
STORY_ID=""
SEARCH_QUERY=""
SEARCH_SORT="date"
COMMENT_DEPTH=$DEFAULT_COMMENT_DEPTH
MAX_COMMENTS=$DEFAULT_MAX_COMMENTS
STORY_COUNT=$DEFAULT_STORY_COUNT
HOT_FILTER=false

while [[ $# -gt 0 ]]; do
    case $1 in
        -h|--help)
            show_help
            exit 0
            ;;
        -c|--comments)
            MODE="comments"
            if [[ -z "${2:-}" ]]; then
                echo "Error: --comments requires a story ID" >&2
                exit 1
            fi
            STORY_ID="$2"
            shift 2
            ;;
        -s|--search)
            MODE="search"
            if [[ -z "${2:-}" ]]; then
                echo "Error: --search requires a query string" >&2
                exit 1
            fi
            SEARCH_QUERY="$2"
            shift 2
            ;;
        --sort)
            if [[ -z "${2:-}" ]]; then
                echo "Error: --sort requires a mode (date or popular)" >&2
                exit 1
            fi
            if [[ "$2" != "date" && "$2" != "popular" ]]; then
                echo "Error: --sort must be 'date' or 'popular'" >&2
                exit 1
            fi
            SEARCH_SORT="$2"
            shift 2
            ;;
        -d|--depth)
            if [[ -z "${2:-}" ]]; then
                echo "Error: --depth requires a number" >&2
                exit 1
            fi
            COMMENT_DEPTH="$2"
            shift 2
            ;;
        -n|--max-comments)
            if [[ -z "${2:-}" ]]; then
                echo "Error: --max-comments requires a number" >&2
                exit 1
            fi
            MAX_COMMENTS="$2"
            shift 2
            ;;
        --hot)
            HOT_FILTER=true
            shift
            ;;
        -*)
            echo "Error: Unknown option: $1" >&2
            echo "Try '${0##*/} --help' for more information." >&2
            exit 1
            ;;
        *)
            # Positional argument - story count for list mode
            STORY_COUNT="$1"
            shift
            ;;
    esac
done

# Validate arguments based on mode
if [[ "$MODE" == "comments" ]]; then
    if ! [[ "$STORY_ID" =~ ^[0-9]+$ ]]; then
        echo "Error: Story ID must be a number" >&2
        exit 1
    fi
    if ! [[ "$COMMENT_DEPTH" =~ ^[0-9]+$ ]] || ((COMMENT_DEPTH < 1)); then
        echo "Error: Depth must be a positive number" >&2
        exit 1
    fi
    if ! [[ "$MAX_COMMENTS" =~ ^[0-9]+$ ]] || ((MAX_COMMENTS < 1)); then
        echo "Error: Max comments must be a positive number" >&2
        exit 1
    fi
    show_comments "$STORY_ID" "$COMMENT_DEPTH" "$MAX_COMMENTS"
elif [[ "$MODE" == "search" ]]; then
    if [[ -z "$SEARCH_QUERY" ]]; then
        echo "Error: Search query cannot be empty" >&2
        exit 1
    fi
    if ! [[ "$STORY_COUNT" =~ ^[0-9]+$ ]] || ((STORY_COUNT < 1 || STORY_COUNT > 100)); then
        echo "Error: COUNT for search must be a number between 1 and 100" >&2
        exit 1
    fi
    search_stories "$SEARCH_QUERY" "$STORY_COUNT" "$SEARCH_SORT"
else
    if ! [[ "$STORY_COUNT" =~ ^[0-9]+$ ]] || ((STORY_COUNT < 1 || STORY_COUNT > 500)); then
        echo "Error: COUNT must be a number between 1 and 500" >&2
        echo "Try '${0##*/} --help' for more information." >&2
        exit 1
    fi
    show_stories "$STORY_COUNT" "$HOT_FILTER"
fi

exit 0
