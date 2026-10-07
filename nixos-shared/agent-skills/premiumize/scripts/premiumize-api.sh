#!/usr/bin/env nix
#! nix shell nixpkgs#bash nixpkgs#curl nixpkgs#jq nixpkgs#bc --command bash
# shellcheck shell=bash
set -euo pipefail

# Override is for tests only; the API key is sent to whatever this points at.
readonly BASE_URL="${PREMIUMIZE_BASE_URL:-https://www.premiumize.me/api}"
readonly DEFAULT_FOLDER_NAME="AgentSkill"

# --- Helpers ---

# Lazy: usage/help must not trigger a gpg prompt.
api_key() {
    printf '%s' "${PREMIUMIZE_API_KEY:-$(pass api/premiumize)}"
}

# Escape a value for a double-quoted curl config string.
cfg_escape() {
    local s=$1
    s=${s//\\/\\\\}
    s=${s//\"/\\\"}
    s=${s//$'\n'/\\n}
    s=${s//$'\r'/\\r}
    s=${s//$'\t'/\\t}
    printf '%s' "$s"
}

# api GET|POST|FORM <endpoint> [key=value...]
# Everything, including the API key, goes to curl as a config on stdin: nothing
# secret lands in argv (visible in `ps`), and curl does the UTF-8 percent
# encoding. FORM is multipart; a value starting with "@" is a file upload.
# Replies with {"status":"error"} exit non-zero with the message on stderr.
api() {
    local method=$1 endpoint=$2
    shift 2
    local key
    key=$(api_key)
    if [ -z "$key" ]; then
        echo "Error: no API key (set PREMIUMIZE_API_KEY or store it at api/premiumize in pass)" >&2
        exit 1
    fi
    {
        printf 'url = "%s"\n' "$(cfg_escape "${BASE_URL}/${endpoint}")"
        [ "$method" = GET ] && echo get
        local kv path
        for kv in "apikey=${key}" "$@"; do
            if [ "$method" = FORM ] && [[ "${kv#*=}" == @* ]]; then
                # Quote the path for curl's -F parser (it splits on ; and ,).
                path=${kv#*=@}
                path=${path//\\/\\\\}
                path=${path//\"/\\\"}
                printf 'form = "%s"\n' "$(cfg_escape "${kv%%=*}=@\"${path}\"")"
            elif [ "$method" = FORM ]; then
                printf 'form-string = "%s"\n' "$(cfg_escape "$kv")"
            else
                printf 'data-urlencode = "%s"\n' "$(cfg_escape "$kv")"
            fi
        done
    } | curl -sS --fail-with-body --config - |
        jq -e 'if .status == "error" then error(.message // "API error") else . end'
}

format_size() {
    local bytes="${1:-0}"
    if [ "$bytes" -ge 1073741824 ] 2>/dev/null; then
        echo "$(echo "scale=2; $bytes / 1073741824" | bc) GB"
    elif [ "$bytes" -ge 1048576 ] 2>/dev/null; then
        echo "$(echo "scale=1; $bytes / 1048576" | bc) MB"
    elif [ "$bytes" -ge 1024 ] 2>/dev/null; then
        echo "$(echo "scale=0; $bytes / 1024" | bc) KB"
    else
        echo "${bytes} B"
    fi
}

# Looked up by name rather than pinned: a pinned ID is account-specific and
# goes stale if the folder is recreated.
default_folder_id() {
    local id
    id=$(api GET "folder/list" | jq -r --arg n "$DEFAULT_FOLDER_NAME" \
        'first(.content[]? | select(.type == "folder" and .name == $n) | .id) // empty')
    if [ -z "$id" ]; then
        echo "Error: no root folder named '$DEFAULT_FOLDER_NAME'; create it or pass a folder_id" >&2
        exit 1
    fi
    echo "$id"
}

format_timestamp() {
    local ts="${1:-0}"
    if [ "$ts" -gt 0 ] 2>/dev/null; then
        date -d "@$ts" '+%Y-%m-%d %H:%M' 2>/dev/null || echo "$ts"
    else
        echo "N/A"
    fi
}

# --- Transfer Commands ---

cmd_transfers() {
    api GET "transfer/list" | jq .
}

cmd_transfers_pretty() {
    api GET "transfer/list" | jq -r '
        .transfers[]? |
        "\(.name)\n  ID: \(.id)\n  Status: \(.status)" +
        (if .progress then "  Progress: \(.progress * 100 | floor)%" else "" end) +
        (if .message and .message != "" then "\n  Message: \(.message)" else "" end) +
        "\n"
    '
}

cmd_transfer_create() {
    local src="${1:-}"
    local folder_id="${2:-}"
    if [ -z "$src" ]; then
        echo "Usage: transfer-create <url|magnet> [folder_id]" >&2
        exit 1
    fi
    [ -n "$folder_id" ] || folder_id=$(default_folder_id)
    api POST "transfer/create" "src=$src" "folder_id=$folder_id" | jq .
}

cmd_transfer_create_file() {
    local file="${1:-}"
    local folder_id="${2:-}"
    if [ -z "$file" ] || [ ! -f "$file" ]; then
        echo "Usage: transfer-create-file <nzb/dlc file> [folder_id]" >&2
        exit 1
    fi
    [ -n "$folder_id" ] || folder_id=$(default_folder_id)
    api FORM "transfer/create" "file=@$file" "folder_id=$folder_id" | jq .
}

cmd_transfer_delete() {
    local id="${1:-}"
    if [ -z "$id" ]; then
        echo "Usage: transfer-delete <transfer_id>" >&2
        exit 1
    fi
    api POST "transfer/delete" "id=$id" | jq .
}

cmd_transfer_clear() {
    api POST "transfer/clearfinished" | jq .
}

cmd_directdl() {
    local src="${1:-}"
    if [ -z "$src" ]; then
        echo "Usage: directdl <url|magnet>" >&2
        exit 1
    fi
    api POST "transfer/directdl" "src=$src" | jq .
}

cmd_directdl_pretty() {
    local src="${1:-}"
    if [ -z "$src" ]; then
        echo "Usage: directdl-pretty <url|magnet>" >&2
        exit 1
    fi
    api POST "transfer/directdl" "src=$src" | jq -r '
        if .status == "success" then
            "Direct Download Links:\n" +
            (.content[]? |
                "  \(.path)\n    Size: \(if .size then (.size / 1073741824 * 100 | floor / 100 | tostring) + " GB" else "Unknown" end)\n    Link: \(.link)\n    Stream: \(.stream_link // "N/A")\n    Transcode: \(.transcode_status // "N/A")\n"
            )
        else
            "Error: \(.message // "Unknown error")"
        end
    '
}

# --- Cache Commands ---

cmd_cache_check() {
    if [ $# -eq 0 ]; then
        echo "Usage: cache-check <url1> [url2] [url3] ..." >&2
        exit 1
    fi
    local params=()
    for item in "$@"; do
        params+=("items[]=$item")
    done
    api GET "cache/check" "${params[@]}" | jq .
}

cmd_cache_check_pretty() {
    if [ $# -eq 0 ]; then
        echo "Usage: cache-check-pretty <url1> [url2] ..." >&2
        exit 1
    fi
    local items=("$@")
    local params=()
    for item in "${items[@]}"; do
        params+=("items[]=$item")
    done
    local result
    result=$(api GET "cache/check" "${params[@]}")
    local i=0
    for item in "${items[@]}"; do
        local cached transcoded filename filesize
        cached=$(echo "$result" | jq -r ".response[$i] // false")
        transcoded=$(echo "$result" | jq -r ".transcoded[$i] // false")
        filename=$(echo "$result" | jq -r ".filename[$i] // \"N/A\"")
        filesize=$(echo "$result" | jq -r ".filesize[$i] // \"0\"")
        echo "$item"
        echo "  Cached: $cached"
        echo "  Transcoded: $transcoded"
        echo "  Filename: $filename"
        echo "  Size: $(format_size "$filesize")"
        echo ""
        i=$((i + 1))
    done
}

# --- Folder Commands ---

cmd_folder_list() {
    local id="${1:-}"
    local params=()
    [ -n "$id" ] && params+=("id=$id")
    params+=("includebreadcrumbs=true")
    api GET "folder/list" "${params[@]}" | jq .
}

cmd_folder_list_pretty() {
    local id="${1:-}"
    local params=()
    [ -n "$id" ] && params+=("id=$id")
    params+=("includebreadcrumbs=true")
    api GET "folder/list" "${params[@]}" | jq -r '
        "Folder: \(.name // "Root")" +
        "\nID: \(.folder_id // "root")" +
        (if .breadcrumbs then "\nPath: " + ([.breadcrumbs[]?.name] | join(" / ")) else "" end) +
        "\n" +
        (if .content then
            (.content | sort_by(.type) | reverse | map(
                (if .type == "folder" then "\n📁 " else "\n📄 " end) +
                "\(.name)" +
                (if .size and .size > 0 then "  (\(.size / 1048576 | floor) MB)" else "" end) +
                "\n   ID: \(.id)" +
                (if .link then "\n   Link: \(.link)" else "" end)
            ) | join(""))
        else "\n  (empty)" end)
    '
}

cmd_folder_create() {
    local name="${1:-}"
    local parent_id="${2:-}"
    if [ -z "$name" ]; then
        echo "Usage: folder-create <name> [parent_id]" >&2
        exit 1
    fi
    local args=("name=$name")
    [ -n "$parent_id" ] && args+=("parent_id=$parent_id")
    api POST "folder/create" "${args[@]}" | jq .
}

cmd_folder_rename() {
    local id="${1:-}"
    local name="${2:-}"
    if [ -z "$id" ] || [ -z "$name" ]; then
        echo "Usage: folder-rename <folder_id> <new_name>" >&2
        exit 1
    fi
    api POST "folder/rename" "id=$id" "name=$name" | jq .
}

cmd_folder_delete() {
    local id="${1:-}"
    if [ -z "$id" ]; then
        echo "Usage: folder-delete <folder_id>" >&2
        exit 1
    fi
    api POST "folder/delete" "id=$id" | jq .
}

cmd_folder_paste() {
    local target_id="${1:-}"
    shift || true
    if [ -z "$target_id" ]; then
        echo "Usage: folder-paste <target_folder_id> [--files id1 id2] [--folders id1 id2]" >&2
        exit 1
    fi
    local args=("id=$target_id")
    local mode=""
    for arg in "$@"; do
        case "$arg" in
            --files) mode="files" ;;
            --folders) mode="folders" ;;
            *)
                case "$mode" in
                    files) args+=("files[]=$arg") ;;
                    folders) args+=("folders[]=$arg") ;;
                    *) echo "Specify --files or --folders before IDs" >&2; exit 1 ;;
                esac
                ;;
        esac
    done
    api POST "folder/paste" "${args[@]}" | jq .
}

cmd_folder_search() {
    local query="${1:-}"
    if [ -z "$query" ]; then
        echo "Usage: folder-search <query>" >&2
        exit 1
    fi
    api GET "folder/search" "q=$query" | jq .
}

cmd_folder_search_pretty() {
    local query="${1:-}"
    if [ -z "$query" ]; then
        echo "Usage: folder-search-pretty <query>" >&2
        exit 1
    fi
    api GET "folder/search" "q=$query" | jq -r '
        "Search results for: \(.name // "unknown")\n" +
        ([.content[]? |
            (if .type == "folder" then "\n📁 " else "\n📄 " end) +
            "\(.name)" +
            (if .size and .size > 0 then "  (\(.size / 1048576 | floor) MB)" else "" end) +
            "\n   ID: \(.id)" +
            (if .link then "\n   Link: \(.link)" else "" end)
        ] | join(""))
    '
}

cmd_folder_uploadinfo() {
    local id="${1:-}"
    local params=()
    [ -n "$id" ] && params+=("id=$id")
    api GET "folder/uploadinfo" "${params[@]}" | jq .
}

# --- Item Commands ---

cmd_item_listall() {
    api GET "item/listall" | jq .
}

cmd_item_details() {
    local id="${1:-}"
    if [ -z "$id" ]; then
        echo "Usage: item-details <item_id>" >&2
        exit 1
    fi
    api GET "item/details" "id=$id" | jq .
}

cmd_item_details_pretty() {
    local id="${1:-}"
    if [ -z "$id" ]; then
        echo "Usage: item-details-pretty <item_id>" >&2
        exit 1
    fi
    api GET "item/details" "id=$id" | jq -r '
        "\(.name)\n" +
        "  Type: \(.type // "unknown")\n" +
        "  Size: \(if .size then (.size / 1048576 | floor | tostring) + " MB" else "Unknown" end)\n" +
        "  MIME: \(.mime_type // "unknown")\n" +
        "  Created: \(.created_at // 0)\n" +
        "  Virus: \(.virus_scan // "N/A")\n" +
        (if .vcodec then "  Video: \(.vcodec) \(.resx // "?")x\(.resy // "?")\n" else "" end) +
        (if .acodec then "  Audio: \(.acodec)\n" else "" end) +
        (if .duration then "  Duration: \(.duration)\n" else "" end) +
        (if .transcode_status then "  Transcode: \(.transcode_status)\n" else "" end) +
        (if .link then "  Download: \(.link)\n" else "" end) +
        (if .stream_link then "  Stream: \(.stream_link)\n" else "" end)
    '
}

cmd_item_delete() {
    local id="${1:-}"
    if [ -z "$id" ]; then
        echo "Usage: item-delete <item_id>" >&2
        exit 1
    fi
    api POST "item/delete" "id=$id" | jq .
}

cmd_item_rename() {
    local id="${1:-}"
    local name="${2:-}"
    if [ -z "$id" ] || [ -z "$name" ]; then
        echo "Usage: item-rename <item_id> <new_name>" >&2
        exit 1
    fi
    api POST "item/rename" "id=$id" "name=$name" | jq .
}

# --- Zip Commands ---

cmd_zip_generate() {
    if [ $# -eq 0 ]; then
        echo "Usage: zip-generate [--files id1 id2] [--folders id1 id2]" >&2
        exit 1
    fi
    local args=()
    local mode=""
    for arg in "$@"; do
        case "$arg" in
            --files) mode="files" ;;
            --folders) mode="folders" ;;
            *)
                case "$mode" in
                    files) args+=("files[]=$arg") ;;
                    folders) args+=("folders[]=$arg") ;;
                    *) echo "Specify --files or --folders before IDs" >&2; exit 1 ;;
                esac
                ;;
        esac
    done
    api POST "zip/generate" "${args[@]}" | jq .
}

# --- Account Commands ---

cmd_account() {
    api GET "account/info" | jq .
}

cmd_account_pretty() {
    api GET "account/info" | jq -r '
        "Account Info\n" +
        "  Customer ID: \(.customer_id)\n" +
        "  Premium Until: \(if .premium_until then (.premium_until | localtime | strftime("%Y-%m-%d")) else "N/A" end)\n" +
        "  Fair Use: \(.limit_used * 100 | floor)%\n" +
        "  Booster Points: \(.booster_points // "N/A")\n" +
        "  Space Used: \(if .space_used then (.space_used / 1073741824 * 100 | floor / 100 | tostring) + " GB" else "Unknown" end)"
    '
}

# --- Services ---

cmd_services() {
    api GET "services/list" | jq .
}

# --- Download file to disk ---

cmd_download() {
    local item_id="${1:-}"
    local output="${2:-}"
    if [ -z "$item_id" ]; then
        echo "Usage: download <item_id> [output_path]" >&2
        exit 1
    fi
    # Get item details to find the download link
    local details
    details=$(api GET "item/details" "id=$item_id")
    local link name
    link=$(echo "$details" | jq -r '.link // empty')
    name=$(echo "$details" | jq -r '.name // "download"')
    if [ -z "$link" ]; then
        echo "Error: No download link found for item $item_id" >&2
        echo "$details" | jq . >&2
        exit 1
    fi
    [ -z "$output" ] && output="$name"
    echo "Downloading: $name" >&2
    echo "To: $output" >&2
    curl -L --fail -C - -o "$output" "$link"
    echo "Done: $output"
}

# --- Main ---

case "${1:-}" in
    # Transfer operations
    transfers)          shift; cmd_transfers "$@" ;;
    transfers-pretty)   shift; cmd_transfers_pretty "$@" ;;
    transfer-create)    shift; cmd_transfer_create "$@" ;;
    transfer-create-file) shift; cmd_transfer_create_file "$@" ;;
    transfer-delete)    shift; cmd_transfer_delete "$@" ;;
    transfer-clear)     shift; cmd_transfer_clear "$@" ;;
    directdl)           shift; cmd_directdl "$@" ;;
    directdl-pretty)    shift; cmd_directdl_pretty "$@" ;;

    # Cache operations
    cache-check)        shift; cmd_cache_check "$@" ;;
    cache-check-pretty) shift; cmd_cache_check_pretty "$@" ;;

    # Folder operations
    folder-list)        shift; cmd_folder_list "$@" ;;
    folder-list-pretty) shift; cmd_folder_list_pretty "$@" ;;
    folder-create)      shift; cmd_folder_create "$@" ;;
    folder-rename)      shift; cmd_folder_rename "$@" ;;
    folder-delete)      shift; cmd_folder_delete "$@" ;;
    folder-paste)       shift; cmd_folder_paste "$@" ;;
    folder-search)      shift; cmd_folder_search "$@" ;;
    folder-search-pretty) shift; cmd_folder_search_pretty "$@" ;;
    folder-uploadinfo)  shift; cmd_folder_uploadinfo "$@" ;;

    # Item operations
    item-listall)       shift; cmd_item_listall "$@" ;;
    item-details)       shift; cmd_item_details "$@" ;;
    item-details-pretty) shift; cmd_item_details_pretty "$@" ;;
    item-delete)        shift; cmd_item_delete "$@" ;;
    item-rename)        shift; cmd_item_rename "$@" ;;

    # Zip operations
    zip-generate)       shift; cmd_zip_generate "$@" ;;

    # Account & services
    account)            shift; cmd_account "$@" ;;
    account-pretty)     shift; cmd_account_pretty "$@" ;;
    services)           shift; cmd_services "$@" ;;

    # Download to disk
    download)           shift; cmd_download "$@" ;;

    *)
        cat <<'EOF'
Usage: premiumize-api.sh <command> [args...]

Transfer Management:
  transfers              List all transfers (JSON)
  transfers-pretty       List all transfers (formatted)
  transfer-create <url|magnet> [folder_id]    Add download
  transfer-create-file <nzb|dlc> [folder_id]  Add download from file
  transfer-delete <id>   Delete a transfer
  transfer-clear         Clear finished transfers
  directdl <url|magnet>  Get direct download link (JSON)
  directdl-pretty <url>  Get direct download link (formatted)

Cache:
  cache-check <url1> [url2] ...        Check cache status (JSON)
  cache-check-pretty <url1> [url2] ... Check cache status (formatted)

Folder Management:
  folder-list [folder_id]         List folder contents (JSON)
  folder-list-pretty [folder_id]  List folder contents (formatted)
  folder-create <name> [parent_id]  Create folder
  folder-rename <id> <new_name>     Rename folder
  folder-delete <id>                Delete folder
  folder-paste <target_id> [--files id...] [--folders id...]  Move items
  folder-search <query>           Search files (JSON)
  folder-search-pretty <query>    Search files (formatted)
  folder-uploadinfo [folder_id]   Get upload token/URL

Item Management:
  item-listall           List all files (JSON)
  item-details <id>      Show file details (JSON)
  item-details-pretty <id>  Show file details (formatted)
  item-delete <id>       Delete a file
  item-rename <id> <name>  Rename a file

Other:
  zip-generate [--files id...] [--folders id...]  Generate zip
  download <item_id> [output_path]  Download file to disk
  account                Account info (JSON)
  account-pretty         Account info (formatted)
  services               List supported services
EOF
        ;;
esac
