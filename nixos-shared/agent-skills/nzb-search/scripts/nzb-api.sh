#!/usr/bin/env nix
#! nix shell nixpkgs#bash nixpkgs#curl nixpkgs#jq nixpkgs#yq-go --command bash
set -euo pipefail

# Indexer configuration
declare -A INDEXERS=(
    ["treasuremaps"]="https://treasure-maps.com/api|api/treasuremaps"
    ["nzbgeek"]="https://api.nzbgeek.info/api|api/nzbgeek"
    ["nzbfinder"]="https://nzbfinder.ws/api|api/nzbfinder"
    ["nzbplanet"]="https://api.nzbplanet.net/api|api/nzbplanet"
    ["drunkenslug"]="https://drunkenslug.com/api|api/drunkenslug"
)

DEFAULT_INDEXER="treasuremaps"

# search_all leaves these out unless named with --include: NZBFinder's free
# tier allows 15 calls per 24h, too few to spend on every fan-out.
SEARCH_ALL_SKIP=(nzbfinder)

# Where `download` puts NZBs when no output path is given
NZB_DIR="${NZB_DIR:-${TMPDIR:-/tmp}/nzb-search}"

die() {
    echo "Error: $*" >&2
    exit 1
}

# Point the global indexer state at $1. The API key is fetched lazily by
# ensure_key, so commands that never hit the network never touch `pass`.
select_indexer() {
    [[ -n "${INDEXERS[$1]:-}" ]] || die "unknown indexer '$1' (available: ${!INDEXERS[*]})"
    INDEXER="$1"
    IFS='|' read -r BASE_URL PASS_PATH <<< "${INDEXERS[$1]}"
    API_KEY=""
}

ensure_key() {
    [[ -n "$API_KEY" ]] && return 0
    API_KEY="$(pass "$PASS_PATH")" || die "could not read API key from pass at $PASS_PATH"
    [[ -n "$API_KEY" ]] || die "API key at $PASS_PATH is empty"
}

# key_curl PARAMS [curl args...]: request BASE_URL?apikey=KEY&PARAMS. The URL
# goes to curl as a config on stdin, so the key never shows up in argv (ps).
key_curl() {
    local url="${BASE_URL}?apikey=${API_KEY}&$1"
    shift
    url="${url//\\/\\\\}"
    printf 'url = "%s"\n' "${url//\"/\\\"}" | curl -K - "$@"
}

# Percent-encode as UTF-8 (a per-character printf encodes "ü" as the Latin-1
# code point %fc, which indexers reject or mismatch)
urlencode() {
    jq -rn --arg s "$1" '$s | @uri'
}

# Normalize JSON response to unified format across indexers
# - Wraps top-level .item under .channel.item (DrunkenSlug drops the channel wrapper)
# - Converts "newznab:attr" to "attr" (NZBFinder, DrunkenSlug)
# - Rewrites {_name,_value} attribute style to {"@attributes":{name,value}} (DrunkenSlug)
# - Extracts GUID from guid."@content" or guid.text URL into attr array (NZBFinder, DrunkenSlug)
# - Rewrites {_url,_length,_type} enclosure to {"@attributes":{url,length,type}} (DrunkenSlug)
# - Tags every item with .indexer so results stay attributable after merging
normalize_response() {
    jq --arg idx "$INDEXER" '
      # Step 1: DrunkenSlug — wrap top-level .item under .channel.item
      (if .item and (.channel.item == null) then
         .channel = ((.channel // {}) + {item: .item}) | del(.item)
       else . end)
      |
      # Step 2: NZBFinder/DrunkenSlug — rename "newznab:attr" -> "attr"
      walk(if type == "object" and has("newznab:attr") and (has("attr") | not)
           then . + {"attr": ."newznab:attr"} | del(."newznab:attr")
           else . end)
      |
      # Step 3: DrunkenSlug — rewrite {_name,_value} -> {"@attributes":{name,value}}
      walk(if type == "object" and has("_name") and has("_value") and (has("@attributes") | not)
           then {"@attributes": {"name": ._name, "value": ._value}}
           else . end)
      |
      # Step 4: normalize per-item guid and enclosure shapes
      if .channel.item then
        .channel.item |= (
          [., []] | flatten | map(
            # guid: handle object form with @content (NZBFinder) or text (DrunkenSlug)
            (if (.guid | type) == "object" then
              (.guid."@content" // .guid.text) as $guid_url
              | if $guid_url then
                  ($guid_url | split("/") | last) as $guid_val
                  | (if .attr then
                       .attr += [{"@attributes": {"name": "guid", "value": $guid_val}}]
                     else
                       . + {"attr": [{"@attributes": {"name": "guid", "value": $guid_val}}]}
                     end)
                  | .guid = $guid_val
                else . end
            else . end)
            |
            # enclosure: DrunkenSlug uses {_url,_length,_type}
            (if (.enclosure | type) == "object" and (.enclosure | has("_url")) and (.enclosure | has("@attributes") | not) then
              .enclosure = {"@attributes": {"url": .enclosure._url, "length": .enclosure._length, "type": .enclosure._type}}
            else . end)
            | . + {indexer: $idx}
          )
          | if length == 1 then .[0] else . end
        )
      else . end
    '
}

# NZBFinder ignores o=json and answers RSS XML. yq turns it into JSON with
# "+@name" attributes and "+content" text; map those onto the "@attributes" /
# "@content" forms the other indexers use, so normalize_response sees one shape.
xml_to_json() {
    yq -p xml -o json '.' | jq '
      (.rss // .)
      | walk(if type == "object" then
          (with_entries(select(.key | startswith("+@") | not))
           | if has("+content") then . + {"@content": ."+content"} | del(."+content") else . end)
          + (to_entries | map(select(.key | startswith("+@")) | {key: .key[2:], value})
             | if length > 0 then {"@attributes": from_entries} else {} end)
        else . end)'
}

# Fail loudly on indexer errors instead of passing them on as "no results".
# Newznab reports errors as XML (<error code=".." description=".."/>) even
# when o=json was asked for; some indexers send a JSON error object instead.
check_response() {
    local body="$1"
    if jq empty 2>/dev/null <<< "$body"; then
        local err
        err=$(jq -r '
          if type != "object" then empty
          elif .error then (.error."@attributes".description // .error.description // (.error | tostring))
          elif (.channel == null and .item == null and ."@attributes".code != null)
            then (."@attributes".description // ."@attributes".code)
          # NZBgeek: bad key answers an empty channel carrying account status
          elif .channel.account."@attributes".status
            then .channel.account."@attributes".status
          else empty end' <<< "$body")
        [[ -z "$err" ]] || die "$INDEXER: $err"
        return 0
    fi
    if [[ "$body" =~ \<error[^\>]*description=\"([^\"]*)\" ]]; then
        die "$INDEXER: ${BASH_REMATCH[1]}"
    fi
    die "$INDEXER: unexpected non-JSON response: ${body:0:200}"
}

# Core API call function. NZB_DRY_RUN=1 prints the request (without the key)
# instead of sending it, for tests and for checking what a command would ask.
api() {
    local params="$1"
    if [[ -n "${NZB_DRY_RUN:-}" ]]; then
        echo "${BASE_URL}?${params}&o=json"
        return 0
    fi
    ensure_key
    local body
    body=$(key_curl "${params}&o=json" -sS --max-time 30) \
        || die "$INDEXER: request failed"
    if [[ "$body" == *"<rss"* ]] && ! jq empty 2>/dev/null <<< "$body"; then
        body=$(xml_to_json <<< "$body") || die "$INDEXER: unparseable XML response"
    fi
    check_response "$body"
    normalize_response <<< "$body"
}

# Split trailing args: those starting with "&" are concatenated into EXTRA,
# the rest land in POSITIONAL (e.g. tvsearch's season and episode).
EXTRA=""
POSITIONAL=()
split_extra() {
    EXTRA=""
    POSITIONAL=()
    local a
    for a in "$@"; do
        if [[ "$a" == \&* ]]; then
            EXTRA+="$a"
        else
            POSITIONAL+=("$a")
        fi
    done
}

# search QUERY [&extra...]
search() {
    local query="$1"
    shift
    split_extra "$@"
    api "t=search&q=$(urlencode "$query")${EXTRA}"
}

# search_all [--include NAME]... QUERY [&extra...]
# Same output shape as search ({channel:{item:[...]}}, each item tagged with
# .indexer); failing indexers are reported on stderr and skipped. Exits
# non-zero only when every queried indexer failed.
search_all() {
    local -a include=()
    while [[ "${1:-}" == --include ]]; do
        include+=("$2")
        shift 2
    done
    local query="$1"
    shift
    SEARCH_ALL_TMP=$(mktemp -d -t nzb-search-all.XXXXXX)
    trap 'rm -r "$SEARCH_ALL_TMP"' EXIT
    local tmpdir="$SEARCH_ALL_TMP" idx
    for idx in "${!INDEXERS[@]}"; do
        if [[ " ${SEARCH_ALL_SKIP[*]} " == *" $idx "* && " ${include[*]} " != *" $idx "* ]]; then
            echo "  ($idx skipped: not in search_all by default, add --include $idx)" >&2
            continue
        fi
        (
            select_indexer "$idx"
            search "$query" "$@" > "$tmpdir/$idx.json"
        ) || { rm -f "$tmpdir/$idx.json"; echo "  ($idx skipped: see error above)" >&2; } &
    done
    wait
    # Failed indexers leave no file behind; none at all means nothing answered
    local -a answered=("$tmpdir"/*.json)
    [[ -e "${answered[0]}" ]] \
        || die "search_all: every indexer failed, see errors above"
    jq -s '{channel: {item: [.[] | [.channel.item] | flatten | .[] | select(. != null)]}}' "${answered[@]}"
}

# results [--sort grabs|size|none] [--table]
# Reads a search/search_all/tvsearch/movie/book response on stdin and emits
# flat records: indexer, title, guid, size_gb, grabs, resolution, subs,
# category, pubDate. Default sort: grabs, most first.
results() {
    local sort="grabs" table=""
    while [[ $# -gt 0 ]]; do
        case "$1" in
            --sort) sort="$2"; shift 2 ;;
            --table) table=1; shift ;;
            *) die "results: unknown option '$1'" ;;
        esac
    done
    [[ "$sort" =~ ^(grabs|size|none)$ ]] || die "results: --sort must be grabs, size or none"
    local records
    records=$(jq --arg sort "$sort" '
      def attrs($n): [(.attr // [])[]? | select(."@attributes".name == $n) | ."@attributes".value];
      def attr($n): attrs($n) | first;
      def lastattr($n): attrs($n) | last;
      [.channel.item] | flatten | map(select(. != null)) | map({
        indexer,
        title,
        guid: ((attr("guid") // .guid) | if type == "string" then split("/") | last else . end),
        size_gb: ((.size // attr("size") // .enclosure."@attributes".length)
                  | if . == null then null else (tonumber / 1073741824 * 100 | floor / 100) end),
        grabs: (attr("grabs") | if . == null then null else tonumber end),
        resolution: (((.title // "") | [match("2160p|1080p|720p|576p|480p"; "i").string | ascii_downcase] | first)
                     // (attr("resolution") | if . == null then null
                         else ([capture("\\d+x(?<h>\\d+)").h] | first | if . then "\(.)p" else null end) end)),
        subs: attr("subs"),
        # parent and child categories both appear (2000, 2050); keep the child
        category: (lastattr("category") // .category),
        pubDate
      })
      | if $sort == "grabs" then sort_by(-(.grabs // -1))
        elif $sort == "size" then sort_by(-(.size_gb // 0))
        else . end')
    if [[ -z "$table" ]]; then
        echo "$records"
        return 0
    fi
    jq -r 'to_entries[] | .key as $i | .value |
      "\($i + 1). \(.title)\n   \(if .size_gb then "\(.size_gb) GB" else "? GB" end) · grabs \(.grabs // "?") · \(.resolution // "res ?")\(if .subs then " · subs: \(.subs)" else "" end) · \(.indexer)\n   guid \(.guid) · \(.pubDate)"' \
        <<< "$records"
}

# Get details for a specific NZB by GUID
details() {
    api "t=details&id=$1"
}

# Get NFO for a specific NZB (raw text, not JSON)
nfo() {
    ensure_key
    key_curl "t=getnfo&id=$1&raw=1" -sSf --max-time 30 \
        || die "$INDEXER: no NFO for $1"
}

# download GUID [OUTPUT]  (default: $NZB_DIR/GUID.nzb) — prints the path
# Validates the payload: indexers answer limits and premium-only releases
# with an XML error under HTTP 200, which would otherwise be saved as .nzb.
download() {
    local guid="$1"
    local output="${2:-}"
    if [[ -z "$output" ]]; then
        (umask 077 && mkdir -p "$NZB_DIR")
        output="$NZB_DIR/${guid}.nzb"
    fi
    ensure_key
    # NZBs run to a few MB: a looser limit than the API calls
    key_curl "t=get&id=${guid}" -sSfL --max-time 120 -o "$output" \
        || { rm -f "$output"; die "$INDEXER: download of $guid failed"; }
    if ! grep -q '<nzb' "$output"; then
        local body
        body=$(head -c 400 "$output")
        rm -f "$output"
        check_response "$body"
        die "$INDEXER: $guid did not return an NZB: ${body:0:200}"
    fi
    echo "$output"
}

# tvsearch QUERY [SEASON [EP]] [&extra...]
tvsearch() {
    local query="$1"
    shift
    split_extra "$@"
    local params
    params="t=tvsearch&q=$(urlencode "$query")"
    [[ -n "${POSITIONAL[0]:-}" ]] && params+="&season=${POSITIONAL[0]}"
    [[ -n "${POSITIONAL[1]:-}" ]] && params+="&ep=${POSITIONAL[1]}"
    api "${params}${EXTRA}"
}

# movie TITLE|IMDB_ID [&extra...]  (IMDb ID with or without the tt prefix)
movie() {
    local query="$1"
    shift
    split_extra "$@"
    # title=, not q=: Treasure Maps matches q= against everything (release
    # group names included), title= against the movie title
    if [[ "$query" =~ ^(tt)?([0-9]+)$ ]]; then
        api "t=movie&imdbid=${BASH_REMATCH[2]}${EXTRA}"
    else
        api "t=movie&title=$(urlencode "$query")${EXTRA}"
    fi
}

# book QUERY [&extra...]
book() {
    local query="$1"
    shift
    split_extra "$@"
    api "t=book&q=$(urlencode "$query")${EXTRA}"
}

# Cart operations (Treasure Maps only)
cartadd() {
    api "t=cartadd&id=$1"
}

cartdel() {
    api "t=cartdel&id=$1"
}

# Get server capabilities. DrunkenSlug answers t=caps&o=json with literal
# null, so fall back to the XML document there.
caps() {
    local out
    ensure_key
    out=$(api "t=caps")
    if [[ "$out" == "null" ]]; then
        key_curl "t=caps" -sS --max-time 30
    else
        echo "$out"
    fi
}

list_indexers() {
    echo "Available indexers:"
    local idx
    for idx in "${!INDEXERS[@]}"; do
        if [[ "$idx" == "$DEFAULT_INDEXER" ]]; then
            echo "  $idx (default)"
        else
            echo "  $idx"
        fi
    done
}

usage() {
    echo "Usage: nzb-api.sh [@indexer] <command> [args...]"
    echo ""
    list_indexers
    echo ""
    echo "Commands: search, search_all, tvsearch, movie, book, results, details, nfo,"
    echo "          download, cartadd, cartdel, caps, api, list_indexers"
}

# Parse indexer from first argument or use default
if [[ "${1:-}" =~ ^@(.+)$ ]]; then
    select_indexer "${BASH_REMATCH[1]}"
    shift
else
    select_indexer "$DEFAULT_INDEXER"
fi

case "${1:-}" in
    search|search_all|results|details|nfo|download|tvsearch|movie|book|cartadd|cartdel|caps|api|list_indexers)
        cmd="$1"
        shift
        "$cmd" "$@"
        ;;
    normalize)
        # stdin (JSON or RSS XML) -> normalized JSON; for tests and for raw
        # responses saved by hand
        body=$(cat)
        if [[ "$body" == *"<rss"* ]] && ! jq empty 2>/dev/null <<< "$body"; then
            body=$(xml_to_json <<< "$body")
        fi
        normalize_response <<< "$body"
        ;;
    "")
        usage
        ;;
    *)
        usage >&2
        die "unknown command '$1'"
        ;;
esac
