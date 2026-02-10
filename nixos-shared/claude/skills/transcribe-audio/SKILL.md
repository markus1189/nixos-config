---
name: transcribe-audio
description: "Transcribes audio files (mp3, wav, ogg, m4a, flac, webm) using Gemini API via Portkey, saves transcripts as markdown, and supports follow-up analysis. Use when the user asks to transcribe audio, summarize a meeting recording, check a voice note, extract action items from a recording, asks what was discussed in an audio file, or mentions processing audio files in any way."
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

Run the script for each file:

```bash
./scripts/transcribe.sh <audio-file> [custom-prompt] > <output.md>
```

- Output file: same name as audio, with `.md` extension, same directory
- Default prompt handles speaker identification, timestamps, summary, action items
- Pass a custom prompt as second argument only when the user explicitly requests different output

The script outputs the transcript to stdout and progress to stderr. Capture stdout to the `.md` file.

### 4. Post-Processing

After transcription (or when an existing transcript is available), support any follow-up:
- Read the `.md` file and answer questions about the content
- Extract action items or TODOs
- Provide additional summaries or analysis
- Compare across multiple transcripts

## Key Details

- **Supported formats**: `.mp3`, `.wav`, `.ogg`, `.m4a`, `.flac`, `.webm`
- **API**: Gemini via Portkey (key from `pass api/portkey-claude`)
- **Timeout**: 600s per file — long recordings take time
- **Max file size**: 200MB per file

**Script Execution:** Scripts should be executed from the skill directory.
All scripts use Nix shebangs so no manual dependency installation is required.
