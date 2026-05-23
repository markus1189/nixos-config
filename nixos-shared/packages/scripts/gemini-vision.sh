#!/usr/bin/env nix
#! nix shell nixpkgs#bash nixpkgs#curl nixpkgs#jq nixpkgs#coreutils --command bash
# shellcheck shell=bash
set -euo pipefail

# Analyze images with a text prompt using Gemini via Portkey API.
# Usage: gemini-vision.sh <prompt> <image1> [image2 ...]
# Images can be local files (png/jpg/jpeg/gif/webp/heic/heif) or http(s) URLs.

SCRIPT_NAME="$(basename "$0")"
readonly SCRIPT_NAME
readonly MAX_IMAGE_SIZE=$((20 * 1024 * 1024)) # 20MB per image
readonly MAX_IMAGES=16
readonly MODEL="@vertex-eu-global/gemini-3.5-flash"

usage() {
  cat >&2 <<EOF
Usage: $SCRIPT_NAME <prompt> <image1> [image2 ...]

Arguments:
  prompt    Text prompt / question about the image(s)
  imageN    Path to local image file OR http(s):// URL
            Supported file types: .png .jpg .jpeg .gif .webp .heic .heif

Limits:
  - Max ${MAX_IMAGES} images per call
  - Max $(numfmt --to=iec $MAX_IMAGE_SIZE) per local image file

Examples:
  $SCRIPT_NAME "What's in this image?" screenshot.png
  $SCRIPT_NAME "Compare these mockups" a.png b.png c.jpg
  $SCRIPT_NAME "Transcribe the text" https://example.com/doc.png ./page.jpg
  $SCRIPT_NAME "Describe diagram" diagram.webp > description.md
EOF
  exit 1
}

error_exit() {
  echo "Error: $1" >&2
  exit "${2:-1}"
}

if [[ $# -eq 0 ]] || [[ "$1" == "-h" ]] || [[ "$1" == "--help" ]]; then
  usage
fi

if [[ $# -lt 2 ]]; then
  echo "Error: need a prompt and at least one image" >&2
  usage
fi

PROMPT="$1"
shift
IMAGES=("$@")

if [[ -z "$PROMPT" ]]; then
  error_exit "Prompt is empty"
fi

if (( ${#IMAGES[@]} > MAX_IMAGES )); then
  error_exit "Too many images (${#IMAGES[@]}), max ${MAX_IMAGES}"
fi

detect_mime() {
  case "${1,,}" in
    *.png)         echo image/png ;;
    *.jpg|*.jpeg)  echo image/jpeg ;;
    *.gif)         echo image/gif ;;
    *.webp)        echo image/webp ;;
    *.heic|*.heif) echo image/heic ;;
    *) echo image/jpeg
       echo "Warning: unknown extension for $1, assuming JPEG" >&2 ;;
  esac
}

# Fail-fast validation pass: check every file before any expensive work.
for img in "${IMAGES[@]}"; do
  if [[ "$img" =~ ^[Hh][Tt][Tt][Pp][Ss]?:// ]]; then
    continue
  fi
  if [[ ! -f "$img" ]]; then
    error_exit "File not found: $img"
  fi
  if [[ ! -r "$img" ]]; then
    error_exit "File not readable: $img"
  fi
  size="$(stat --format=%s "$img")"
  if (( size > MAX_IMAGE_SIZE )); then
    error_exit "File too large ($(numfmt --to=iec "$size")): $img, max $(numfmt --to=iec $MAX_IMAGE_SIZE)"
  fi
done

# Create temp directory with cleanup trap
WORK_DIR="$(mktemp -d -t gemini-vision.XXXXXX)" || error_exit "Failed to create temp directory"
trap 'rm -rf "$WORK_DIR"' EXIT

# Get API key
if ! PORTKEY_API_KEY="$(pass api/portkey-claude 2>/dev/null)"; then
  error_exit "Failed to retrieve API key from pass (api/portkey-claude)"
fi

if [[ -z "$PORTKEY_API_KEY" ]]; then
  error_exit "API key is empty"
fi

echo "--- Analyzing ${#IMAGES[@]} image(s) ---" >&2

# Build one JSON content item per image into $WORK_DIR/items/
mkdir -p "$WORK_DIR/items"
i=0
for img in "${IMAGES[@]}"; do
  item_file="$WORK_DIR/items/item-$(printf '%04d' "$i").json"
  if [[ "$img" =~ ^[Hh][Tt][Tt][Pp][Ss]?:// ]]; then
    echo "Image $i: URL $img" >&2
    jq -n --arg url "$img" \
      '{type:"image_url", image_url:{url:$url}}' > "$item_file"
  else
    mime="$(detect_mime "$img")"
    echo "Image $i: encoding $(basename "$img") ($mime)" >&2
    url_file="$WORK_DIR/items/url-$(printf '%04d' "$i").txt"
    # printf without \n + base64 -w0 → NO trailing newline in the data URL.
    # Never use `echo` here: a stray \n would corrupt the embedded URL.
    { printf 'data:%s;base64,' "$mime"; base64 -w0 "$img"; } > "$url_file" \
      || error_exit "Failed to encode $img"
    jq -n --rawfile url "$url_file" \
      '{type:"image_url", image_url:{url:$url}}' > "$item_file"
  fi
  i=$((i + 1))
done

# Slurp all per-image items into one JSON array file
if ! jq -s '.' "$WORK_DIR"/items/item-*.json > "$WORK_DIR/content-array.json"; then
  error_exit "Failed to assemble content array"
fi

# Assemble final payload: image items first, then the text prompt
echo "Building API payload..." >&2
TMPPAYLOAD="$WORK_DIR/payload.json"
if ! jq -n \
  --arg model "$MODEL" \
  --arg prompt "$PROMPT" \
  --slurpfile images "$WORK_DIR/content-array.json" \
  '{
    model: $model,
    messages: [{
      role: "user",
      content: ($images[0] + [{type:"text", text:$prompt}])
    }],
    max_tokens: 65536
  }' > "$TMPPAYLOAD"; then
  error_exit "Failed to build JSON payload"
fi

# Call the API with timeout, writing response to temp file
echo "Calling Gemini API..." >&2
RESPONSE_FILE="$WORK_DIR/response.json"
HTTP_CODE="$(curl -sS -w "%{http_code}" --max-time 600 \
  -o "$RESPONSE_FILE" \
  https://api.portkey.ai/v1/chat/completions \
  -H "Content-Type: application/json" \
  -H "x-portkey-api-key: $PORTKEY_API_KEY" \
  -d @"$TMPPAYLOAD" 2>"$WORK_DIR/curl_error.log")" || {
  if [[ -s "$WORK_DIR/curl_error.log" ]]; then
    cat "$WORK_DIR/curl_error.log" >&2
  fi
  error_exit "API request failed (curl error)"
}

if [[ "$HTTP_CODE" -ne 200 ]]; then
  echo "Error: API returned HTTP $HTTP_CODE" >&2
  jq . "$RESPONSE_FILE" 2>/dev/null || cat "$RESPONSE_FILE" >&2
  error_exit "API request failed with HTTP $HTTP_CODE"
fi

# Check for API error in response before extracting content
ERROR_MSG="$(jq -r '.error.message // empty' "$RESPONSE_FILE")"
if [[ -n "$ERROR_MSG" ]]; then
  error_exit "API error: $ERROR_MSG"
fi

# Extract the content
CONTENT="$(jq -r '.choices[0].message.content // empty' "$RESPONSE_FILE")"

if [[ -z "$CONTENT" ]]; then
  echo "Error: No content returned. API response:" >&2
  jq . "$RESPONSE_FILE" 2>/dev/null || cat "$RESPONSE_FILE" >&2
  error_exit "Empty response from API"
fi

echo "$CONTENT"
echo "--- Done ---" >&2
