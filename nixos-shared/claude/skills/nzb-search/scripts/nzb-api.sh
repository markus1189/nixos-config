#!/usr/bin/env nix
#! nix shell nixpkgs#bash nixpkgs#curl nixpkgs#jq --command bash
set -euo pipefail

# Indexer configuration
declare -A INDEXERS=(
    ["scenenzbs"]="https://scenenzbs.com/api|api/scenenzbs"
    ["nzbgeek"]="https://api.nzbgeek.info/api|api/nzbgeek"
    ["nzbfinder"]="https://nzbfinder.ws/api|api/nzbfinder"
    ["nzbplanet"]="https://api.nzbplanet.net/api|api/nzbplanet"
)

# Default indexer
DEFAULT_INDEXER="scenenzbs"

# Parse indexer from first argument or use default
if [[ "${1:-}" =~ ^@(.+)$ ]]; then
    INDEXER="${BASH_REMATCH[1]}"
    shift
else
    INDEXER="$DEFAULT_INDEXER"
fi

# Validate indexer
if [[ -z "${INDEXERS[$INDEXER]:-}" ]]; then
    echo "Error: Unknown indexer '$INDEXER'" >&2
    echo "Available: ${!INDEXERS[*]}" >&2
    exit 1
fi

# Extract config for selected indexer
IFS='|' read -r BASE_URL PASS_PATH <<< "${INDEXERS[$INDEXER]}"
readonly BASE_URL PASS_PATH

# Get API key
readonly API_KEY="$(pass "$PASS_PATH")"

if [ -z "$API_KEY" ]; then
    echo "Error: API key not found at $PASS_PATH" >&2
    exit 1
fi

# URL encode function
urlencode() {
    local string="$1"
    local strlen=${#string}
    local encoded=""
    local pos c o

    for (( pos=0 ; pos<strlen ; pos++ )); do
        c=${string:$pos:1}
        case "$c" in
            [-_.~a-zA-Z0-9] ) o="${c}" ;;
            * ) printf -v o '%%%02x' "'$c"
        esac
        encoded+="${o}"
    done
    echo "${encoded}"
}

# Normalize JSON response to unified format across indexers
# - Converts "newznab:attr" to "attr" (NZBFinder uses newznab:attr)
# - Extracts GUID from guid."@content" URL into attr array (NZBFinder format)
# - Passes through non-JSON (XML errors) to stderr gracefully
normalize_response() {
    local input
    input=$(cat)
    # If input is not valid JSON, output as-is (handles XML error responses)
    if ! echo "$input" | jq empty 2>/dev/null; then
        echo "$input"
        return 0
    fi
    echo "$input" | jq '
      # Recursive walk to rename "newznab:attr" -> "attr"
      walk(if type == "object" and has("newznab:attr") and (has("attr") | not)
           then . + {"attr": ."newznab:attr"} | del(."newznab:attr")
           else . end)
      |
      # Normalize guid: extract UUID from guid."@content" URL into attr array
      if .channel.item then
        .channel.item |= (
          [., []] | flatten | map(
            if (.guid | type) == "object" and .guid."@content" then
              (.guid."@content" | split("/") | last) as $guid_val
              | if .attr then
                  .attr += [{"@attributes": {"name": "guid", "value": $guid_val}}]
                else
                  . + {"attr": [{"@attributes": {"name": "guid", "value": $guid_val}}]}
                end
              | .guid = $guid_val
            else . end
          )
          | if length == 1 then .[0] else . end
        )
      else . end
    '
}

# Core API call function
api() {
    local params="$1"
    curl -s "${BASE_URL}?apikey=${API_KEY}&${params}&o=json" | normalize_response
}

# Search and return results
search() {
    local query="$1"
    local extra="${2:-}"
    local encoded_query=$(urlencode "$query")
    api "t=search&q=${encoded_query}${extra}"
}

# Search across all indexers
search_all() {
    local query="$1"
    local extra="${2:-}"
    local tmpdir
    tmpdir=$(mktemp -d -t nzb-search-all.XXXXXX)
    trap "rm -rf '$tmpdir'" RETURN
    for idx in "${!INDEXERS[@]}"; do
        echo "=== $idx ===" >&2
        IFS='|' read -r url pass_path <<< "${INDEXERS[$idx]}"
        local key="$(pass "$pass_path")"
        local encoded_query=$(urlencode "$query")
        local raw
        raw=$(curl -s "${url}?apikey=${key}&t=search&q=${encoded_query}${extra}&o=json")
        # Skip non-JSON responses (XML errors like rate limits)
        if ! echo "$raw" | jq empty 2>/dev/null; then
            echo "  (skipped: non-JSON response)" >&2
            continue
        fi
        echo "$raw" | normalize_response | \
            jq --arg idx "$idx" '[.channel.item] | flatten | .[]? | . + {indexer: $idx}' \
            > "$tmpdir/$idx.json" 2>/dev/null || true
    done
    jq -s 'flatten' "$tmpdir"/*.json 2>/dev/null || echo '[]'
}

# Get result titles only
search_titles() {
    search "$@" | jq -r '[.channel.item] | flatten | .[]?.title // empty'
}

# Get result GUIDs
search_guids() {
    search "$@" | jq -r '[.channel.item] | flatten | .[]?.attr[]? | select(."@attributes".name == "guid") | ."@attributes".value'
}

# Get full result details (title, guid, size, category)
search_details() {
    search "$@" | jq -r '[.channel.item] | flatten | .[]? |
        {
            title: .title,
            guid: (.attr[]? | select(."@attributes".name == "guid") | ."@attributes".value),
            size: (.attr[]? | select(."@attributes".name == "size") | ."@attributes".value | tonumber),
            category: .category,
            pubDate: .pubDate,
            link: .link
        }'
}

# Get details for a specific NZB by GUID
details() {
    local guid="$1"
    api "t=details&id=${guid}"
}

# Get NFO for a specific NZB
nfo() {
    local guid="$1"
    api "t=getnfo&id=${guid}&raw=1"
}

# Download NZB file
download() {
    local guid="$1"
    local output="${2:-${guid}.nzb}"
    curl -s "${BASE_URL}?apikey=${API_KEY}&t=get&id=${guid}" -o "$output"
    echo "$output"
}

# TV search
tvsearch() {
    local query="$1"
    local season="${2:-}"
    local ep="${3:-}"
    local encoded_query=$(urlencode "$query")
    local params="t=tvsearch&q=${encoded_query}"
    [ -n "$season" ] && params="${params}&season=${season}"
    [ -n "$ep" ] && params="${params}&ep=${ep}"
    api "$params"
}

# Movie search
movie() {
    local query="$1"
    if [[ "$query" =~ ^[0-9]+$ ]]; then
        api "t=movie&imdbid=${query}"
    else
        local encoded_query=$(urlencode "$query")
        api "t=movie&title=${encoded_query}"
    fi
}

# Book search
book() {
    local query="$1"
    local extra="${2:-}"
    local encoded_query=$(urlencode "$query")
    api "t=book&q=${encoded_query}${extra}"
}

# Cart operations
cartadd() {
    local guid="$1"
    api "t=cartadd&id=${guid}"
}

cartdel() {
    local guid="$1"
    api "t=cartdel&id=${guid}"
}

cart() {
    curl -s "${BASE_URL}/rss?apikey=${API_KEY}&t=-2"
}

# Get server capabilities
caps() {
    api "t=caps"
}

# List available indexers
list_indexers() {
    echo "Available indexers:"
    for idx in "${!INDEXERS[@]}"; do
        if [[ "$idx" == "$DEFAULT_INDEXER" ]]; then
            echo "  $idx (default)"
        else
            echo "  $idx"
        fi
    done
}

# Filter by category
filter_category() {
    local cat="$1"
    jq --arg cat "$cat" '.channel.item[]? | select(.category | contains($cat))'
}

# Filter by minimum size (in bytes)
filter_min_size() {
    local min_size="$1"
    jq --arg size "$min_size" '.channel.item[]? |
        select((.attr[] | select(."@attributes".name == "size") | ."@attributes".value | tonumber) >= ($size | tonumber))'
}

# Sort by size
sort_by_size() {
    jq -s 'sort_by(.attr[] | select(."@attributes".name == "size") | ."@attributes".value | tonumber) | reverse | .[]'
}

# Execute command passed as first argument
case "${1:-}" in
    search|search_all|search_titles|search_guids|search_details|details|nfo|download|tvsearch|movie|book|cartadd|cartdel|cart|caps|api|list_indexers)
        cmd="$1"
        shift
        $cmd "$@"
        ;;
    *)
        # If no command, assume it's a direct API call
        if [[ -n "${1:-}" ]]; then
            api "$@"
        else
            echo "Usage: nzb-api.sh [@indexer] <command> [args...]"
            echo ""
            list_indexers
            echo ""
            echo "Commands: search, search_all, tvsearch, movie, book, details, download, cartadd, cartdel, cart, caps"
        fi
        ;;
esac
