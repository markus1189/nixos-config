#!/usr/bin/env nix
#! nix shell nixpkgs#bash nixpkgs#curl nixpkgs#jq nixpkgs#coreutils nixpkgs#ffmpeg --command bash
# shellcheck shell=bash
set -euo pipefail

# Transcribe audio files using Gemini 3.5 Flash via OpenRouter
# Usage: transcribe.sh <audio-file> [prompt]

readonly SCRIPT_NAME="$(basename "$0")"
readonly MAX_FILE_SIZE=$((200 * 1024 * 1024)) # 200MB

readonly DEFAULT_PROMPT='You are an expert audio transcriber. Transcribe the provided audio accurately and provide a useful summary.

## Transcript

Provide a verbatim transcript with speaker identification where applicable:
- Use "Speaker 1", "Speaker 2", etc. for unknown speakers
- Use actual names if speakers are identified or introduce themselves
- Include timestamps in [MM:SS] format at natural breaks (every 1-2 minutes or at topic changes)
- For single-speaker content (voice notes, lectures), just transcribe without speaker labels

## Summary

After the transcript, provide a structured summary:

### Key Points
- Main topics, ideas, or information covered

### Action Items (if any)
- [ ] Specific actions mentioned or implied

### Questions/Follow-ups (if any)
- Unresolved questions or items needing attention

Adapt the summary structure to the content type (meeting, interview, voice note, lecture, podcast, etc.)'

usage() {
  cat >&2 <<EOF
Usage: $SCRIPT_NAME <audio-file> [prompt]

Arguments:
  audio-file    Audio file to transcribe (mp3, wav, ogg, m4a, flac, webm)
  prompt        Optional system prompt (default: generic transcribe + summary)

Examples:
  $SCRIPT_NAME meeting.mp3
  $SCRIPT_NAME voice-note.m4a
  $SCRIPT_NAME interview.wav "Transcribe this interview verbatim"
  $SCRIPT_NAME podcast.mp3 "Transcribe and extract key insights"

Supported formats: .mp3, .wav, .ogg, .m4a, .flac, .webm
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

FILE="$1"
SYSTEM_PROMPT="${2:-$DEFAULT_PROMPT}"

# Validate file exists and is readable
if [[ ! -f "$FILE" ]]; then
  error_exit "File not found: $FILE"
fi

if [[ ! -r "$FILE" ]]; then
  error_exit "File not readable: $FILE"
fi

# Validate file size
FILE_SIZE="$(stat --format=%s "$FILE")"
if (( FILE_SIZE > MAX_FILE_SIZE )); then
  error_exit "File too large ($(numfmt --to=iec "$FILE_SIZE")), max $(numfmt --to=iec $MAX_FILE_SIZE)"
fi

# Create temp directory with cleanup trap
WORK_DIR="$(mktemp -d -t transcribe.XXXXXX)" || error_exit "Failed to create temp directory"
trap 'rm -rf "$WORK_DIR"' EXIT

# Get API key
if ! OPENROUTER_API_KEY="$(pass api/openrouter/transcribe 2>/dev/null)"; then
  error_exit "Failed to retrieve API key from pass (api/openrouter/transcribe)"
fi

if [[ -z "$OPENROUTER_API_KEY" ]]; then
  error_exit "API key is empty"
fi

readonly MODEL="google/gemini-3.5-flash"

readonly BASENAME="$(basename "$FILE")"
echo "--- Transcribing: $BASENAME ---" >&2

# Downsample to 16kHz mono MP3. Keeps the base64 payload under OpenRouter's
# inline-size limit and is plenty of fidelity for speech. Also normalizes every
# input format (wav/ogg/m4a/flac/webm/...) to a single mp3 payload.
TMPMP3="$WORK_DIR/audio.mp3"
readonly FMT="mp3"

echo "Downsampling audio (16kHz mono)..." >&2
if ! ffmpeg -y -i "$FILE" -ac 1 -ar 16000 -b:a 32k "$TMPMP3" >/dev/null 2>&1; then
  error_exit "Failed to downsample audio (ffmpeg)"
fi

# Base64 encode to temp file (too large for argv)
TMPB64="$WORK_DIR/audio.b64"
TMPPAYLOAD="$WORK_DIR/payload.json"

echo "Encoding audio file..." >&2
if ! base64 -w0 "$TMPMP3" > "$TMPB64"; then
  error_exit "Failed to encode audio file"
fi

# Build JSON payload using jq --rawfile to avoid argv limits
echo "Building API payload..." >&2
if ! jq -n \
  --arg model "$MODEL" \
  --arg system "$SYSTEM_PROMPT" \
  --arg fmt "$FMT" \
  --rawfile audio "$TMPB64" \
  --arg fname "$BASENAME" \
  '{
    model: $model,
    messages: [
      {role: "system", content: $system},
      {role: "user", content: [
        {
          type: "input_audio",
          input_audio: {
            data: $audio,
            format: $fmt
          }
        },
        {
          type: "text",
          text: ("Please process this audio file: " + $fname)
        }
      ]}
    ],
    max_tokens: 60000,
    reasoning: { effort: "low" }
  }' > "$TMPPAYLOAD"; then
  error_exit "Failed to build JSON payload"
fi
# NOTE: gemini-3.5-flash forces reasoning ("cannot be disabled"). Left at default
# effort it burns the whole output budget thinking and returns empty content, so
# we pin effort=low — it then emits the transcript directly with ~0 reasoning tokens.

# Call the API with timeout, writing response to temp file
echo "Calling OpenRouter API (this may take a few minutes)..." >&2
RESPONSE_FILE="$WORK_DIR/response.json"
HTTP_CODE="$(curl -sS -w "%{http_code}" --max-time 600 \
  -o "$RESPONSE_FILE" \
  https://openrouter.ai/api/v1/chat/completions \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer $OPENROUTER_API_KEY" \
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

# Check for API error in response before extracting transcript
ERROR_MSG="$(jq -r '.error.message // empty' "$RESPONSE_FILE")"
if [[ -n "$ERROR_MSG" ]]; then
  error_exit "API error: $ERROR_MSG"
fi

# Extract the transcript content
TRANSCRIPT="$(jq -r '.choices[0].message.content // empty' "$RESPONSE_FILE")"

if [[ -z "$TRANSCRIPT" ]]; then
  echo "Error: No transcript returned. API response:" >&2
  jq . "$RESPONSE_FILE" 2>/dev/null || cat "$RESPONSE_FILE" >&2
  error_exit "Empty transcript from API"
fi

echo "$TRANSCRIPT"
echo "" >&2
echo "--- Done: $BASENAME ---" >&2
