---
name: transcribe-audio
description: "Transcribes audio files (mp3, wav, ogg, m4a, flac, webm) using Gemini 3.5 Flash via OpenRouter, saves transcripts as markdown, and supports follow-up analysis. Use when the user asks to transcribe audio, summarize a meeting recording, check a voice note, extract action items from a recording, asks what was discussed in an audio file, or mentions processing audio files in any way."
---

# Audio Transcription

Transcribe audio files to markdown and support post-processing (Q&A, action items, summaries).

## Workflow

### 1. Identify Audio Files

Find audio files matching the user's request:
- Single file: user specifies path directly
- Batch: `find <dir> -maxdepth 1 -type f \( -name "*.mp3" -o -name "*.wav" -o -name "*.ogg" -o -name "*.m4a" -o -name "*.flac" -o -name "*.webm" \) | sort`

### 2. Check for Existing Transcripts

For each audio file, check if a sibling `.md` file exists (e.g. `meeting.mp3` → `meeting.md`):
- **Exists + user wants transcription**: Ask whether to re-transcribe or use existing
- **Exists + user wants analysis**: Read the existing `.md` directly — no need to transcribe
- **Does not exist**: Proceed with transcription

### 3. Transcribe

Run the script for each file, using `-o` so the transcript is written **atomically**:

```bash
./scripts/transcribe.sh <audio-file> [custom-prompt] -o <output.md>
```

- Output file: same name as audio, with `.md` extension, same directory
- Default prompt handles speaker identification, timestamps, summary, action items
- Pass a custom prompt as second argument when the user requests different output or a focused transcription (see below)
- **Always prefer `-o <output.md>` over `> <output.md>`.** With `-o` the script
  writes a staging file and only renames it into place on full success, so a
  failed run (API 5xx, timeout) never creates or clobbers the `.md`. A bare `>`
  redirect truncates the file *before* the script runs, leaving a silent empty
  transcript on failure. Without `-o` the transcript still goes to stdout for
  ad-hoc use.

The script logs progress to stderr; with `-o` it writes the transcript to the file itself.

### 3a. Long recordings (auto-chunking) — agent writes the summary

Recordings longer than ~45 min are **automatically split into ~45-min chunks**,
transcribed in parallel, and reassembled into `## Part N — starts at Hh:Mm`
sections (each part's `[MM:SS]` timestamps restart at 0; add the part offset for
absolute time). For these, with the **default** prompt, the script emits a
**transcript only** and ends the file with this marker:

```
<!-- chunked transcript: no summary included; the agent should read this and append a single merged "## Summary" -->
```

When you see that marker, **you (the agent) must read the assembled transcript
and append one merged `## Summary`** to the file — Key Points, Action Items
(`- [ ]`), and Questions/Follow-ups, **deduplicated across all parts**. Do this
yourself; do not make another transcription call. (A custom prompt is passed
through per chunk and no marker is added — handle per the user's request.)

#### Focused Transcription

When the user asks about a **specific topic** (e.g. "tell me about the Miro discussion", "what was said about budgets?"), pass a focused prompt as the second argument instead of doing a full transcription and then grep/reading:

```bash
./scripts/transcribe.sh <audio-file> "Focus on the parts of this audio that discuss <TOPIC>. Provide:
1. A detailed transcript of just those sections (with speaker labels and timestamps)
2. A summary of what was said about <TOPIC>
3. Any decisions, action items, or open questions related to <TOPIC>
Skip unrelated parts of the audio." -o <output-focus.md>
```

- **Output file for focused transcripts**: use a suffix to avoid overwriting the full transcript, e.g. `meeting.focus-miro.md`
- **When to use**: The user asks about a specific topic AND there is no existing full transcript to search, OR the user explicitly asks to re-transcribe with a focus
- **When NOT to use**: A full transcript already exists — just read it and answer the question directly

### 4. Spot-check

After transcription, verify the output before reporting success:
- The `.md` exists and is **non-empty**, and contains real transcript text
  (speaker labels / timestamps), not an error dump or apology.
- For chunked recordings, confirm every `## Part N` is present and that you have
  appended the merged `## Summary` (see 3a).
- The script exits non-zero and writes nothing on failure (atomic `-o`), so a
  non-zero exit means *re-run*, not "ship the empty file". Transient API errors
  (5xx / timeout) are already retried up to 3× internally; a persistent failure
  is worth surfacing to the user.

### 5. Post-Processing

After transcription (or when an existing transcript is available), support any follow-up:
- Read the `.md` file and answer questions about the content
- Extract action items or TODOs
- Provide additional summaries or analysis
- Compare across multiple transcripts

## Key Details

- **Supported formats**: `.mp3`, `.wav`, `.ogg`, `.m4a`, `.flac`, `.webm`
- **API**: `google/gemini-3.5-flash` via OpenRouter (key from `pass api/openrouter/transcribe`)
- **Preprocessing**: input is downsampled to 16kHz mono mp3 (ffmpeg) before upload — keeps payloads under the inline-size limit
- **Reasoning**: pinned to `effort: low` (the model forces reasoning; default effort returns empty content)
- **Auto-chunking**: recordings over ~45 min (`ffprobe` duration) are split into
  ~45-min chunks, transcribed up to 2-in-parallel, and reassembled with
  `## Part N — starts at Hh:Mm` headers. Single-pass transcription of very long
  audio otherwise exceeds the request timeout. Chunked output is transcript-only
  + a marker; the agent writes the merged summary (see 3a).
- **Retry**: each request retries up to 3× with exponential backoff (4/8/16s) on
  transient failures (HTTP 429/500/502/503/504, curl timeout/connection errors).
  Non-retryable errors (other 4xx, bad payload) fail fast.
- **Timeout**: 600s per request — chunks keep each request well under it
- **Atomic output**: `-o FILE` stages to a temp file in the destination dir and
  renames on success only; a failed run never leaves an empty/partial `.md`
- **Max file size**: 200MB per file

**Script Execution:** Scripts should be executed from the skill directory.
All scripts use Nix shebangs so no manual dependency installation is required.
