#!/usr/bin/env nix
#! nix shell nixpkgs#bash nixpkgs#curl nixpkgs#jq nixpkgs#coreutils nixpkgs#pass nixpkgs#gnupg nixpkgs#file --command bash
# shellcheck shell=bash
set -euo pipefail

usage() {
  cat >&2 <<EOF
Usage: $0 [opts] <prompt>

  -m, --model      flash (default, Gemini 3.1 Flash Image)
                   pro   (Gemini 3 Pro Image)
                   or any full OpenRouter model id
  -o, --out        output path (default: nano-banana-<timestamp>.<ext> in
                   \`stuff-today --default\`, else \${TMPDIR:-/tmp})
  -a, --aspect     1:1 2:3 3:2 3:4 4:3 4:5 5:4 9:16 16:9 21:9
                   flash also: 1:4 4:1 1:8 8:1
  -s, --size       1K (default), 2K, 4K   (flash also: 0.5K)
  -i, --image FILE input image, repeatable (max 5 for pro)
  --no-stream      disable SSE streaming

Prompt must be a single (quoted) argument.
API key is read from \`pass api/openrouter/image-editing\`.
EOF
  exit 1
}

MODEL="flash"
OUTPUT=""
USER_OUTPUT=0
PROMPT=""
ASPECT=""
SIZE=""
STREAM=1
IMAGES=()
POSITIONAL=()
API_URL="${NANO_BANANA_API_URL:-https://openrouter.ai/api/v1/chat/completions}"
# Overridable for tests; the 300s default covers slow 4K pro generations.
MAX_TIME="${NANO_BANANA_MAX_TIME:-300}"

while [ $# -gt 0 ]; do
  case "$1" in
    -m|--model) MODEL="$2"; shift 2 ;;
    -o|--out|--output) OUTPUT="$2"; USER_OUTPUT=1; shift 2 ;;
    -a|--aspect) ASPECT="$2"; shift 2 ;;
    -s|--size) SIZE="$2"; shift 2 ;;
    -i|--image) IMAGES+=("$2"); shift 2 ;;
    --no-stream) STREAM=0; shift ;;
    -h|--help) usage ;;
    --) shift; POSITIONAL+=("$@"); break ;;
    -*) echo "Unknown flag: $1" >&2; usage ;;
    *) POSITIONAL+=("$1"); shift ;;
  esac
done

if [ "${#POSITIONAL[@]}" -gt 1 ]; then
  echo "Expected one prompt argument, got ${#POSITIONAL[@]}: quote the prompt, e.g. \"${POSITIONAL[*]}\"" >&2
  exit 1
fi
PROMPT="${POSITIONAL[0]:-}"
[ -n "$PROMPT" ] || usage

case "$MODEL" in
  flash) MODEL="google/gemini-3.1-flash-image-preview" ;;
  pro)   MODEL="google/gemini-3-pro-image-preview" ;;
esac

is_flash() { [[ "$MODEL" == *flash* ]]; }
is_pro()   { [[ "$MODEL" == *pro* ]]; }

# Aspect ratio validation.
case "$ASPECT" in
  ""|1:1|2:3|3:2|3:4|4:3|4:5|5:4|9:16|16:9|21:9) ;;
  1:4|4:1|1:8|8:1)
    is_flash || { echo "aspect_ratio $ASPECT is flash-only (got $MODEL)" >&2; exit 1; }
    ;;
  *) echo "Unknown --aspect: $ASPECT" >&2; exit 1 ;;
esac

# Size validation.
case "$SIZE" in
  ""|1K|2K|4K) ;;
  0.5K)
    is_flash || { echo "image_size 0.5K is flash-only (got $MODEL)" >&2; exit 1; }
    ;;
  *) echo "Unknown --size: $SIZE" >&2; exit 1 ;;
esac

# Image input validation.
if [ "${#IMAGES[@]}" -gt 0 ]; then
  if is_pro && [ "${#IMAGES[@]}" -gt 5 ]; then
    echo "pro supports at most 5 input images (got ${#IMAGES[@]})" >&2; exit 1
  fi
  for IMG in "${IMAGES[@]}"; do
    [ -r "$IMG" ] || { echo "input image not readable: $IMG" >&2; exit 1; }
  done
fi

if [ -z "$OUTPUT" ]; then
  # Never default to the cwd: it may be a public git repo.
  OUT_DIR=""
  if command -v stuff-today >/dev/null 2>&1; then
    OUT_DIR=$(stuff-today --default 2>/dev/null || true)
  fi
  if [ -z "$OUT_DIR" ] || ! mkdir -p "$OUT_DIR" 2>/dev/null; then
    OUT_DIR="${TMPDIR:-/tmp}"
  fi
  OUTPUT="$OUT_DIR/nano-banana-$(date +%Y%m%d-%H%M%S).png"
fi

API_KEY=$(pass api/openrouter/image-editing)
[ -n "$API_KEY" ] || { echo "pass api/openrouter/image-editing returned empty" >&2; exit 1; }

# Build the request body in temp files so the base64 image data never has to
# travel through argv (--arg/--argjson) — easily blows past ARG_MAX otherwise.
CONTENT_FILE=$(mktemp -t claude-code.XXXXXX.json)
BODY_FILE=$(mktemp -t claude-code.XXXXXX.json)
trap 'rm -f "$CONTENT_FILE" "$BODY_FILE" "$CONTENT_FILE.tmp"' EXIT

jq -n --arg p "$PROMPT" '[{type:"text", text:$p}]' > "$CONTENT_FILE"
for IMG in "${IMAGES[@]}"; do
  MIME=$(file --mime-type -b "$IMG")
  jq --rawfile url <(printf 'data:%s;base64,' "$MIME"; base64 -w0 "$IMG") \
    '. + [{type:"image_url", image_url:{url:$url}}]' \
    < "$CONTENT_FILE" > "$CONTENT_FILE.tmp"
  mv "$CONTENT_FILE.tmp" "$CONTENT_FILE"
done

jq -n \
  --slurpfile content "$CONTENT_FILE" \
  --arg m "$MODEL" \
  --argjson stream "$([ "$STREAM" = 1 ] && echo true || echo false)" \
  --arg aspect "$ASPECT" \
  --arg size "$SIZE" \
  '{
     model: $m,
     modalities: ["image","text"],
     stream: $stream,
     messages: [{role:"user", content: $content[0]}],
     image_config: (
       (if $aspect != "" then {aspect_ratio:$aspect} else {} end)
       + (if $size != "" then {image_size:$size} else {} end)
     )
   } | if .image_config == {} then del(.image_config) else . end' \
  > "$BODY_FILE"

if [ "$STREAM" = 1 ]; then
  echo "[$(date +%H:%M:%S)] streaming from $MODEL ..." >&2

  DATA_URL=""
  RAW=""
  # `|| [ -n "$line" ]`: an error body usually has no trailing newline.
  while IFS= read -r line || [ -n "$line" ]; do
    if [[ "$line" != data:* ]]; then
      # Non-SSE lines: an HTTP error body (401/402/429) arrives this way.
      # Blank lines and `:` keep-alive comments are SSE framing, not body.
      [[ -z "${line//[[:space:]]/}" || "$line" == :* ]] || RAW+="$line"$'\n'
      continue
    fi
    payload="${line#data:}"
    payload="${payload# }"
    [[ "$payload" == "[DONE]" ]] && break
    jq -e . >/dev/null 2>&1 <<<"$payload" || continue

    err=$(jq -r '.error.message // empty' <<<"$payload")
    if [ -n "$err" ]; then
      echo >&2
      echo "API error: $err" >&2
      exit 1
    fi

    reasoning=$(jq -r '.choices[0].delta.reasoning // empty' <<<"$payload")
    [ -n "$reasoning" ] && printf '%s' "$reasoning" >&2
    text=$(jq -r '.choices[0].delta.content // empty' <<<"$payload")
    [ -n "$text" ] && printf '%s' "$text" >&2

    url=$(jq -r '
      (.choices[0].delta.images[0].image_url.url
        // .choices[0].message.images[0].image_url.url
        // empty)' <<<"$payload")
    [ -n "$url" ] && DATA_URL="$url" && echo "[$(date +%H:%M:%S)] image received" >&2
  done < <(curl -N -sS --connect-timeout 15 --max-time "$MAX_TIME" "$API_URL" \
    -H "Content-Type: application/json" \
    -H "Accept: text/event-stream" \
    -d @"$BODY_FILE" \
    --config <(printf 'header = "Authorization: Bearer %s"\n' "$API_KEY"))

  echo >&2
  if [ -z "$DATA_URL" ] && [ -n "$RAW" ]; then
    echo "No image returned. Response body:" >&2
    { jq . <<<"$RAW" 2>/dev/null || printf '%s' "$RAW"; } >&2
    echo >&2
    exit 1
  fi
else
  RESPONSE=$(curl -sS --connect-timeout 15 --max-time "$MAX_TIME" "$API_URL" \
    -H "Content-Type: application/json" \
    -d @"$BODY_FILE" \
    --config <(printf 'header = "Authorization: Bearer %s"\n' "$API_KEY"))

  if jq -e '.error' >/dev/null 2>&1 <<<"$RESPONSE"; then
    echo "API error:" >&2
    jq '.error' >&2 <<<"$RESPONSE"
    exit 1
  fi
  DATA_URL=$(jq -r '.choices[0].message.images[0].image_url.url // empty' <<<"$RESPONSE")
fi

unset API_KEY

if [ -z "${DATA_URL:-}" ]; then
  echo "No image returned." >&2
  exit 1
fi

# Detect the actual MIME type from the data URL prefix and pick a matching
# extension when the user didn't pin a filename.
RESP_MIME=$(printf '%s' "${DATA_URL:0:64}" | sed -n 's|^data:\([^;]*\);base64,.*|\1|p')
if [ "$USER_OUTPUT" = 0 ]; then
  case "$RESP_MIME" in
    image/jpeg) OUTPUT="${OUTPUT%.png}.jpg" ;;
    image/webp) OUTPUT="${OUTPUT%.png}.webp" ;;
  esac
else
  case "$RESP_MIME:${OUTPUT,,}" in
    image/png:*.png|image/jpeg:*.jpg|image/jpeg:*.jpeg|image/webp:*.webp) ;;
    :*) ;;
    *) echo "warning: response is ${RESP_MIME} but saving to pinned path $OUTPUT" >&2 ;;
  esac
fi

printf '%s' "$DATA_URL" | sed 's|^data:[^;]*;base64,||' | base64 -d > "$OUTPUT"
echo "Saved: $OUTPUT (model: $MODEL, ${RESP_MIME:-unknown})"
