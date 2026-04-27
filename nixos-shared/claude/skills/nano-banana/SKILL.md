---
name: nano-banana
description: "Generates images from text prompts using Google's Nano Banana models (Gemini 3.1 Flash Image / Gemini 3 Pro Image) via OpenRouter. Use when the user asks to generate, create, or make an image/picture from a text description, or mentions Nano Banana."
---

# nano-banana

Generates or edits an image from a text prompt (and optional input images) and saves it. Streams reasoning and progress to stderr so the user gets feedback during the ~10-60s generation.

## Usage

Invoke the wrapper script with the user's prompt:

```bash
./scripts/nano-banana.sh "<prompt>"
```

The script handles the API key (`pass api/openrouter`), SSE streaming, and saving. It writes the PNG to the current working directory unless `-o` is given. After it returns, the last stdout line is `Saved: <path> (model: <id>)` — read the path with the Read tool to display the image to the user.

## Model selection

- **Default (`flash`)** — Gemini 3.1 Flash Image. Use for almost everything: cheap, fast, newest, "Pro-level quality at Flash speed."
- **`-m pro`** — Gemini 3 Pro Image. Use only when the user explicitly asks for high quality, 4K, professional / commercial work, multi-subject identity preservation (3+ recognizable people), or precise in-image text rendering.

## Output controls

- `-a, --aspect` — `1:1`, `2:3`, `3:2`, `3:4`, `4:3`, `4:5`, `5:4`, `9:16`, `16:9`, `21:9`. Flash-only adds `1:4`, `4:1`, `1:8`, `8:1`.
- `-s, --size` — `1K` (default), `2K`, `4K`. Flash-only adds `0.5K`.
- `-i, --image FILE` — input image for editing/blending; repeatable. Pro accepts up to 5 (identity preservation across subjects).

The script validates aspect/size against the chosen model and rejects unsupported combos before hitting the API.

## Common invocations

```bash
./scripts/nano-banana.sh "a red panda eating ramen"
./scripts/nano-banana.sh -a 16:9 -s 2K "wide cinematic mountain vista"
./scripts/nano-banana.sh -m pro -s 4K -o product.png "studio shot of a wireless mouse"
./scripts/nano-banana.sh -i photo.jpg "make it look like a watercolor painting"
./scripts/nano-banana.sh -m pro -i a.jpg -i b.jpg -i c.jpg "blend these three subjects into a group portrait"
./scripts/nano-banana.sh --no-stream "..."   # quieter, no SSE
```

## Failure modes

- `pass api/openrouter returned empty` — entry missing in the user's password store.
- `API error: ...` — surfaced from OpenRouter (rate limit, model unavailable, content policy). Report it verbatim.
- `No image returned` — model produced text only (e.g. refused). Show the response if helpful.

**Script Execution:** Scripts should be executed from the skill directory. All scripts use Nix shebangs so no manual dependency installation is required.

## Fixes

<!-- Add only for failures actually observed in use. -->
