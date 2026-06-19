#!/usr/bin/env nix
#! nix shell nixpkgs#bash nixpkgs#curl nixpkgs#jq nixpkgs#coreutils nixpkgs#ffmpeg --command bash
# shellcheck shell=bash
set -euo pipefail

# Transcribe audio files using Gemini 3.5 Flash via OpenRouter
# Usage: transcribe.sh [-o OUTPUT.md] <audio-file> [prompt]
#
# Long recordings (> CHUNK_THRESHOLD_SEC) are automatically split into chunks,
# transcribed in parallel, and reassembled. For chunked recordings the script
# emits a transcript ONLY (no summary) ending in a marker comment — the invoking
# agent is expected to read it and write one merged summary itself.

readonly SCRIPT_NAME="$(basename "$0")"
readonly MAX_FILE_SIZE=$((200 * 1024 * 1024)) # 200MB

# --- Tunables -----------------------------------------------------------------
readonly CHUNK_THRESHOLD_SEC=$((45 * 60)) # recordings longer than this get chunked
readonly CHUNK_LEN_SEC=$((45 * 60))       # target length per chunk (~45 min)
readonly MAX_PARALLEL=2                    # concurrent chunk transcriptions
readonly MAX_ATTEMPTS=3                     # curl attempts per request
readonly RETRY_BASE_SLEEP=4                 # backoff base: 4s, 8s, 16s
readonly CURL_MAX_TIME=600                  # per-request timeout (s)
readonly CHUNK_MARKER='<!-- chunked transcript: no summary included; the agent should read this and append a single merged "## Summary" -->'

readonly MODEL="google/gemini-3.5-flash"

# Default prompt: transcript + summary, used for short (single-shot) files.
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

# Transcript-only prompt: used per chunk on the long path. No summary — the
# agent assembles one unified summary across all parts afterwards.
readonly CHUNK_PROMPT='You are an expert audio transcriber. Transcribe the provided audio accurately.

## Transcript

Provide a verbatim transcript with speaker identification where applicable:
- Use "Speaker 1", "Speaker 2", etc. for unknown speakers
- Use actual names if speakers are identified or introduce themselves
- Include timestamps in [MM:SS] format at natural breaks (every 1-2 minutes or at topic changes)
- For single-speaker content (voice notes, lectures), just transcribe without speaker labels

Do NOT produce a summary — transcript only. This audio is one slice of a longer
recording and may begin or end mid-sentence.'

usage() {
  cat >&2 <<EOF
Usage: $SCRIPT_NAME [-o OUTPUT.md] <audio-file> [prompt]

Arguments:
  audio-file    Audio file to transcribe (mp3, wav, ogg, m4a, flac, webm)
  prompt        Optional system prompt (default: generic transcribe + summary)

Options:
  -o OUTPUT.md  Write transcript atomically to OUTPUT.md (only on full success;
                a failed run never creates or clobbers it). Without -o the
                transcript goes to stdout.

Examples:
  $SCRIPT_NAME meeting.mp3
  $SCRIPT_NAME meeting.mp3 -o meeting.md
  $SCRIPT_NAME interview.wav "Transcribe this interview verbatim"
  $SCRIPT_NAME podcast.mp3 "Transcribe and extract key insights" -o podcast.md

Supported formats: .mp3, .wav, .ogg, .m4a, .flac, .webm
EOF
  exit 1
}

error_exit() {
  echo "Error: $1" >&2
  exit "${2:-1}"
}

# --- Argument parsing ---------------------------------------------------------
# Accept -o/--output anywhere; keep positional FILE [PROMPT] backward-compatible.
OUTPUT_PATH=""
POSITIONAL=()
while [[ $# -gt 0 ]]; do
  case "$1" in
    -h|--help) usage ;;
    -o|--output)
      [[ $# -ge 2 ]] || error_exit "-o requires an argument"
      OUTPUT_PATH="$2"; shift 2 ;;
    -o*) OUTPUT_PATH="${1#-o}"; shift ;;
    --) shift; while [[ $# -gt 0 ]]; do POSITIONAL+=("$1"); shift; done ;;
    -*) error_exit "Unknown option: $1" ;;
    *) POSITIONAL+=("$1"); shift ;;
  esac
done
set -- "${POSITIONAL[@]+"${POSITIONAL[@]}"}"

[[ $# -ge 1 ]] || usage

FILE="$1"
SYSTEM_PROMPT="${2:-$DEFAULT_PROMPT}"

# Validate file exists and is readable
[[ -f "$FILE" ]] || error_exit "File not found: $FILE"
[[ -r "$FILE" ]] || error_exit "File not readable: $FILE"

# Validate file size
FILE_SIZE="$(stat --format=%s "$FILE")"
if (( FILE_SIZE > MAX_FILE_SIZE )); then
  error_exit "File too large ($(numfmt --to=iec "$FILE_SIZE")), max $(numfmt --to=iec $MAX_FILE_SIZE)"
fi

# Validate -o destination directory is writable up front (before any API cost)
if [[ -n "$OUTPUT_PATH" ]]; then
  OUT_DIR="$(dirname "$OUTPUT_PATH")"
  [[ -d "$OUT_DIR" ]] || error_exit "Output directory does not exist: $OUT_DIR"
  [[ -w "$OUT_DIR" ]] || error_exit "Output directory not writable: $OUT_DIR"
fi

# Create temp directory with cleanup trap
WORK_DIR="$(mktemp -d -t transcribe.XXXXXX)" || error_exit "Failed to create temp directory"
trap 'rm -rf "$WORK_DIR"' EXIT

# Per-request usage accumulator (one compact JSON object per successful call).
# transcribe_one appends here; totals are summed after all requests finish.
readonly USAGE_FILE="$WORK_DIR/usage.jsonl"

# Get API key
if ! OPENROUTER_API_KEY="$(pass api/openrouter/transcribe 2>/dev/null)"; then
  error_exit "Failed to retrieve API key from pass (api/openrouter/transcribe)"
fi
[[ -n "$OPENROUTER_API_KEY" ]] || error_exit "API key is empty"

readonly BASENAME="$(basename "$FILE")"

# --- Helpers ------------------------------------------------------------------

# curl_with_retry <payload_file> <response_out>
# Echoes the final HTTP code on success (caller inspects it). Returns non-zero
# only on exhausted retries / non-retryable transport errors.
curl_with_retry() {
  local payload="$1" resp="$2"
  local attempt=1 http_code curl_rc
  local errlog="$WORK_DIR/curl_err.$$.$RANDOM.log"

  while (( attempt <= MAX_ATTEMPTS )); do
    set +e
    http_code="$(curl -sS -w '%{http_code}' --max-time "$CURL_MAX_TIME" \
      -o "$resp" \
      https://openrouter.ai/api/v1/chat/completions \
      -H "Content-Type: application/json" \
      -H "Authorization: Bearer $OPENROUTER_API_KEY" \
      -d @"$payload" 2>"$errlog")"
    curl_rc=$?
    set -e

    if (( curl_rc == 0 )) && [[ "$http_code" =~ ^[0-9]+$ ]]; then
      case "$http_code" in
        429|500|502|503|504)
          echo "Attempt $attempt/$MAX_ATTEMPTS: retryable HTTP $http_code" >&2 ;;
        *)
          printf '%s' "$http_code"; return 0 ;; # 200 or terminal 4xx -> hand back
      esac
    else
      # curl transport error: 28=timeout, 6/7=DNS/connect, 35/52/55/56=TLS/recv, 18=partial
      case "$curl_rc" in
        28|6|7|35|52|55|56|18)
          echo "Attempt $attempt/$MAX_ATTEMPTS: retryable curl error rc=$curl_rc" >&2
          [[ -s "$errlog" ]] && cat "$errlog" >&2 ;;
        *)
          echo "Non-retryable curl error rc=$curl_rc" >&2
          [[ -s "$errlog" ]] && cat "$errlog" >&2
          return 1 ;;
      esac
    fi

    if (( attempt < MAX_ATTEMPTS )); then
      local sleep_s=$(( RETRY_BASE_SLEEP * (1 << (attempt - 1)) ))
      echo "Backing off ${sleep_s}s..." >&2
      sleep "$sleep_s"
    fi
    ((attempt++))
  done

  echo "Exhausted $MAX_ATTEMPTS attempts" >&2
  return 1
}

# transcribe_one <audio_file> <system_prompt> <slug>
# Prints the transcript text to stdout. Returns non-zero on hard failure.
# <slug> namespaces temp files so parallel calls never collide.
# If <audio_file> is not already a 16k mono mp3, it is downsampled first.
transcribe_one() {
  local audio="$1" prompt="$2" slug="$3"
  local mp3="$audio"
  local b64="$WORK_DIR/${slug}.b64"
  local payload="$WORK_DIR/${slug}.payload.json"
  local resp="$WORK_DIR/${slug}.response.json"

  if [[ "$audio" != *.16kmono.mp3 ]]; then
    mp3="$WORK_DIR/${slug}.16kmono.mp3"
    if ! ffmpeg -y -i "$audio" -ac 1 -ar 16000 -b:a 32k "$mp3" >/dev/null 2>&1; then
      echo "ffmpeg downsample failed ($slug)" >&2; return 1
    fi
  fi

  if ! base64 -w0 "$mp3" > "$b64"; then
    echo "base64 encode failed ($slug)" >&2; return 1
  fi

  if ! jq -n \
    --arg model "$MODEL" \
    --arg system "$prompt" \
    --arg fmt "mp3" \
    --rawfile audio "$b64" \
    --arg fname "$(basename "$audio")" \
    '{
      model: $model,
      messages: [
        {role: "system", content: $system},
        {role: "user", content: [
          {type: "input_audio", input_audio: {data: $audio, format: $fmt}},
          {type: "text", text: ("Please process this audio file: " + $fname)}
        ]}
      ],
      max_tokens: 60000,
      reasoning: { effort: "low" },
      usage: { include: true }
    }' > "$payload"; then
    echo "jq payload build failed ($slug)" >&2; return 1
  fi
  # NOTE: gemini-3.5-flash forces reasoning ("cannot be disabled"). Left at
  # default effort it burns the whole output budget thinking and returns empty
  # content, so we pin effort=low — it emits the transcript with ~0 reasoning.

  # Request loop. curl_with_retry already handles transport/5xx retries; this
  # outer loop additionally retries a 200-with-empty-content response, which
  # gemini-3.5-flash occasionally returns (it burns the budget on reasoning).
  # An empty 200 is transient, so retrying usually recovers it.
  local attempt=1 http_code err_msg text
  while (( attempt <= MAX_ATTEMPTS )); do
    http_code="$(curl_with_retry "$payload" "$resp")" || return 1

    if [[ "$http_code" -ne 200 ]]; then
      echo "API returned HTTP $http_code ($slug):" >&2
      jq . "$resp" >&2 2>/dev/null || cat "$resp" >&2
      return 1 # terminal (e.g. 4xx) — curl_with_retry already exhausted 5xx
    fi

    err_msg="$(jq -r '.error.message // empty' "$resp")"
    [[ -n "$err_msg" ]] && { echo "API error ($slug): $err_msg" >&2; return 1; }

    text="$(jq -r '.choices[0].message.content // empty' "$resp")"
    if [[ -n "$text" ]]; then
      # Record usage/cost for this call (usage.include=true yields .usage.cost in
      # credits = USD). Single small line append is atomic enough for the parallel
      # chunk path. Best-effort: never fail the transcription over accounting.
      jq -c '.usage // {}' "$resp" >> "$USAGE_FILE" 2>/dev/null || true
      printf '%s' "$text"; return 0
    fi

    echo "Empty transcript on 200 ($slug), attempt $attempt/$MAX_ATTEMPTS" >&2
    if (( attempt < MAX_ATTEMPTS )); then
      sleep $(( RETRY_BASE_SLEEP * (1 << (attempt - 1)) ))
    fi
    ((attempt++))
  done

  echo "Empty transcript from API after $MAX_ATTEMPTS attempts ($slug). Last response:" >&2
  jq . "$resp" >&2 2>/dev/null || cat "$resp" >&2
  return 1
}

# seconds -> "Hh:MMm"
fmt_offset() { printf '%dh:%02dm' $(( $1 / 3600 )) $(( ($1 % 3600) / 60 )); }

# --- Probe duration -----------------------------------------------------------
echo "--- Transcribing: $BASENAME ---" >&2
DURATION_SEC="$(ffprobe -v error -show_entries format=duration \
  -of default=noprint_wrappers=1:nokey=1 "$FILE" 2>/dev/null | cut -d. -f1)"
[[ "$DURATION_SEC" =~ ^[0-9]+$ ]] || DURATION_SEC=0 # probe failed -> simple path

# --- Transcribe ---------------------------------------------------------------
if (( DURATION_SEC <= CHUNK_THRESHOLD_SEC )); then
  # Simple path: single request, model returns transcript + summary (or whatever
  # the custom prompt asks for). Identical behavior to the original script.
  echo "Single-pass transcription (duration ${DURATION_SEC}s)..." >&2
  TRANSCRIPT="$(transcribe_one "$FILE" "$SYSTEM_PROMPT" "main")" \
    || error_exit "Transcription failed"
else
  # Long path: downsample once, segment, transcribe chunks in parallel.
  echo "Long recording (${DURATION_SEC}s) — chunking into ~$((CHUNK_LEN_SEC / 60))min pieces..." >&2

  FULL_MP3="$WORK_DIR/full.16kmono.mp3"
  ffmpeg -y -i "$FILE" -ac 1 -ar 16000 -b:a 32k "$FULL_MP3" >/dev/null 2>&1 \
    || error_exit "Failed to downsample audio (ffmpeg)"

  # -c copy is safe: source is the CBR mp3 we just produced; cuts land on frame
  # boundaries. Chunk filenames carry the .16kmono.mp3 suffix so transcribe_one
  # skips re-downsampling them.
  ffmpeg -y -i "$FULL_MP3" -f segment -segment_time "$CHUNK_LEN_SEC" \
    -c copy "$WORK_DIR/chunk_%03d.16kmono.mp3" >/dev/null 2>&1 \
    || error_exit "Failed to segment audio (ffmpeg)"

  mapfile -t CHUNKS < <(printf '%s\n' "$WORK_DIR"/chunk_*.16kmono.mp3 | sort)
  NUM_CHUNKS=${#CHUNKS[@]}
  (( NUM_CHUNKS > 0 )) || error_exit "Segmentation produced no chunks"
  echo "Transcribing $NUM_CHUNKS chunks (parallelism $MAX_PARALLEL)..." >&2

  # When the default prompt is in use, chunks get the transcript-only prompt and
  # the agent writes the merged summary. A custom prompt is passed through as-is.
  CHUNK_SYS="$SYSTEM_PROMPT"
  IS_DEFAULT=0
  if [[ "$SYSTEM_PROMPT" == "$DEFAULT_PROMPT" ]]; then
    CHUNK_SYS="$CHUNK_PROMPT"; IS_DEFAULT=1
  fi

  declare -a PART_FILES=()
  running=0
  for i in "${!CHUNKS[@]}"; do
    idx="$(printf '%03d' "$i")"
    out="$WORK_DIR/part_${idx}.txt"
    PART_FILES+=("$out")
    part_num=$(( i + 1 ))
    offset=$(( i * CHUNK_LEN_SEC ))
    {
      if body="$(transcribe_one "${CHUNKS[$i]}" "$CHUNK_SYS" "chunk${idx}")"; then
        { printf '## Part %d — starts at %s\n\n' "$part_num" "$(fmt_offset "$offset")"
          printf '%s\n' "$body"; } > "$out"
      else
        printf '__CHUNK_FAILED__%d\n' "$part_num" > "$out"
      fi
    } &
    if (( ++running >= MAX_PARALLEL )); then wait -n; ((running--)); fi
  done
  wait

  # Fail closed: if any chunk failed after retries, produce no output.
  for f in "${PART_FILES[@]}"; do
    if [[ "$(head -c 16 "$f" 2>/dev/null)" == "__CHUNK_FAILED__"* ]]; then
      error_exit "One or more chunks failed to transcribe; aborting (no output written)"
    fi
  done

  TRANSCRIPT="$(cat "${PART_FILES[@]}")"
  if (( IS_DEFAULT )); then
    TRANSCRIPT="${TRANSCRIPT}"$'\n\n'"${CHUNK_MARKER}"
  fi
fi

# --- Usage / cost -------------------------------------------------------------
# Sum usage across all requests (one line per chunk on the long path). cost is in
# OpenRouter credits (1 credit = 1 USD). Reported to stderr always, and stamped
# into the output as an invisible HTML comment so it travels with the transcript.
# Caveat: only successful 200s are counted; cost of retried empty-content 200s
# (rare) is not included, so this is a slight under-count in that case.
USAGE_FOOTER=""
if [[ -s "$USAGE_FILE" ]]; then
  read -r U_PROMPT U_COMPLETION U_COST < <(
    jq -rs 'reduce .[] as $u ([0,0,0];
      [ .[0] + ($u.prompt_tokens // 0),
        .[1] + ($u.completion_tokens // 0),
        .[2] + ($u.cost // 0) ]) | "\(.[0]) \(.[1]) \(.[2])"' "$USAGE_FILE"
  )
  printf -- '--- Usage: %s prompt + %s completion tokens, cost $%.4f (%s) ---\n' \
    "$U_PROMPT" "$U_COMPLETION" "$U_COST" "$MODEL" >&2
  USAGE_FOOTER="$(printf '<!-- transcribe usage: model=%s prompt_tokens=%s completion_tokens=%s cost_usd=%.6f -->' \
    "$MODEL" "$U_PROMPT" "$U_COMPLETION" "$U_COST")"
fi
if [[ -n "$USAGE_FOOTER" ]]; then
  TRANSCRIPT="${TRANSCRIPT}"$'\n\n'"${USAGE_FOOTER}"
fi

# --- Emit ---------------------------------------------------------------------
if [[ -n "$OUTPUT_PATH" ]]; then
  # Atomic: stage in the destination dir so mv is a same-fs rename, and only
  # replace the destination on full success. A failure above already exited
  # without touching OUTPUT_PATH.
  stage="$(mktemp "$OUT_DIR/.transcribe.XXXXXX")" || error_exit "Failed to stage output near destination"
  chmod 644 "$stage" 2>/dev/null || true # mktemp makes 0600; match normal .md perms
  printf '%s\n' "$TRANSCRIPT" > "$stage" || { rm -f "$stage"; error_exit "Failed to write staged output"; }
  mv -f "$stage" "$OUTPUT_PATH" || { rm -f "$stage"; error_exit "Failed to move output into place"; }
  echo "Wrote $OUTPUT_PATH" >&2
else
  printf '%s\n' "$TRANSCRIPT"
fi

echo "" >&2
echo "--- Done: $BASENAME ---" >&2
