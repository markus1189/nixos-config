# Pi Agent Configuration Guide

> **See also:** [`docs/model-inventory.md`](../../../docs/model-inventory.md) — the canonical catalog of every
> model used across this repo (pi agent, opencode, gptel, vision/transcribe/banana scripts), with the exact
> files to touch when a model updates. Keep it in sync when you add or retire models here.

## Converting Requesty Model Slugs to models.json Entries

Requesty routes requests to different providers via a unified EU gateway. This guide shows how to configure these models in pi agent's `models.json`.

### Model Slug Format

Requesty model slugs follow this pattern:
```
<provider>/<model-id>[@<region>]
```

Examples:
- `vertex/claude-sonnet-4-6@europe-west1`
- `bedrock/claude-opus-4-8@eu-central-1`
- `azure/gpt-5.4@swedencentral`
- `nebius/kimi-k3`

Only models on the organization's **Approved Models / Access List** are routable. `/v1/models`
returns exactly that list when called with a key (110 slugs; 785 without one, checked 2026-10-13),
the same for the `agent` and `playground` keys. Newest first, key kept out of argv:

```zsh
curl -sS -H @<(printf 'Authorization: Bearer %s\n' "$(pass api/requesty/agent)") \
  https://router.eu.requesty.ai/v1/models \
  | jq -r '.data | sort_by(-.created)[] | "\(.created|strftime("%F"))  \(.id)"'
```

`created` is when Requesty listed the slug, not the vendor release date.

### Step 1: Find Model Specifications

Use the models.dev API to get accurate model specifications:

```bash
# Search for a specific model
curl -s https://models.dev/api.json | jq '.anthropic.models[] | select(.id == "claude-sonnet-4-6")'

# Or browse by provider
curl -s https://models.dev/api.json | jq '.openai.models[] | select(.id | contains("gpt-5.4"))'
```

The API returns specifications like:
```json
{
  "id": "claude-sonnet-4-6",
  "name": "Claude Sonnet 4.6",
  "reasoning": true,
  "modalities": {
    "input": ["text", "image", "pdf"],
    "output": ["text"]
  },
  "cost": {
    "input": 3,
    "output": 15,
    "cache_read": 0.3,
    "cache_write": 3.75
  },
  "limit": {
    "context": 200000,
    "output": 64000
  }
}
```

### Step 2: Map to models.json Schema

Pi agent's schema has some differences from the API format:

| models.dev API | models.json | Notes |
|----------------|-------------|-------|
| `id` | `id` | Use the Requesty `provider/model` slug, not the base model ID |
| `name` | `name` | Add provider context (e.g., "(Requesty/Vertex EU)") |
| `reasoning` | `reasoning` | Direct mapping |
| `modalities.input` | `input` | Array, but **exclude "pdf"** - schema only accepts "text" and "image" |
| `limit.context` | `contextWindow` | Direct mapping |
| `limit.output` | `maxTokens` | Direct mapping |

No `cost` here: prices come from Requesty's `/v1/models`, not models.dev, and live in
[`requesty-prices/prices.json`](../../requesty-prices/prices.json), which pi and opencode both read.
Add the model with `nixos-shared/requesty-prices/update.py <slug>`; eval fails for a model without
a price. The same run writes `limits.json`, opencode's context and output limits.
`update.py --check` exits 1 when either table has drifted from the router.

### Step 3: Create models.json Entry

Location: `nixos-shared/home-manager/pi-agent/models.json` (deployed to `~/.pi/agent/models.json` with prices added)

Requesty supports the Anthropic SDK directly (Bearer auth, no custom header). Use the bare
host for `anthropic-messages` and the `/v1` suffix for the OpenAI-style endpoints:

```json
{
  "providers": {
    "requesty-anthropic": {
      "baseUrl": "https://router.eu.requesty.ai",
      "apiKey": "$REQUESTY_API_KEY_CC",
      "api": "anthropic-messages",
      "authHeader": true,
      "models": [
        {
          "id": "vertex/claude-sonnet-4-6@europe-west1",
          "name": "Claude Sonnet 4.6 (Requesty/Vertex EU)",
          "reasoning": true,
          "input": ["text", "image"],
          "contextWindow": 200000,
          "maxTokens": 64000
        }
      ]
    },
    "requesty-openai": {
      "baseUrl": "https://router.eu.requesty.ai/v1",
      "apiKey": "$REQUESTY_API_KEY_CC",
      "api": "openai-completions",
      "authHeader": true,
      "models": [
        {
          "id": "azure/gpt-5.4@swedencentral",
          "name": "GPT-5.4 (Requesty/Azure EU)",
          "reasoning": true,
          "thinkingLevelMap": { "minimal": "low", "xhigh": "high" },
          "input": ["text", "image"],
          "contextWindow": 400000,
          "maxTokens": 128000
        }
      ]
    }
  }
}
```

### Step 4: Set Environment Variable

Pi agent resolves API keys from environment variables:

```bash
export REQUESTY_API_KEY_CC="your-requesty-api-key"
```

The `pi` shell alias wires this up automatically from `pass api/requesty/agent`. For manual use, add
the export to `~/.bashrc` or `~/.zshrc` for persistence.

### Common Schema Validation Errors

**Error**: `/providers/requesty-anthropic/models/0/input/2: must be equal to constant`
- **Cause**: Using "pdf" in input array
- **Fix**: Remove "pdf", only use "text" and "image"

**Error**: `Cannot read properties of undefined (reading 'startsWith')`
- **Cause**: The model is registered under an `openai-responses` provider (`api:
  "openai-responses"`, slug prefixed `openai-responses/`). pi's Responses parser reads
  `event.arguments` on the `response.function_call_arguments.done` stream event, but Requesty
  emits that event with `delta` and **no** `arguments` field when a tool is invoked, so pi crashes
  on the first real tool call. Every reasoning model on that route is affected.
- **Affected**: the reasoning GPT and Gemini models previously under `requesty-openai`.
- **Fix**: Register all Requesty models (GPT, Gemini, open-weight) under an
  `openai-completions` provider using the **bare** slug (no `openai-responses/` prefix) e.g.
  `azure/gpt-6-sol@swedencentral`
- **Note**: This only fails when tools are enabled, so a `--no-tools` smoke test will not catch it.
  Verify new models *with* tools. Verified 2026-08: bare GPT-5.6 Sol/Terra/Luna, 5.5 and 5.4
  slugs all accept tools + `reasoning_effort` on `/v1/chat/completions`; GPT-6 Sol/Luna too (2026-09).

### Supported APIs

The `api` field determines the protocol:
- `anthropic-messages`: Native Anthropic format (Claude models — recommended)
- `openai-completions`: OpenAI-compatible chat completions — **use this for every non-Claude
  Requesty model (GPT, Gemini, open-weight)**. Reasoning models get a `thinkingLevelMap` such as
  `{ "minimal": "low", "xhigh": "high" }`.
- `openai-responses`: OpenAI Responses format. **Avoid for Requesty** — pi crashes on tool calls
  (see the `startsWith` error above).
- `google-generative-ai`: Google Gemini format (AI Studio, not Requesty)

### Quick Reference: Common Requesty Providers

| Provider | Slug Prefix | Example |
|----------|-------------|---------|
| Vertex AI (EU) | `vertex/` | `vertex/claude-sonnet-4-6@europe-west1` |
| Bedrock (EU) | `bedrock/` | `bedrock/claude-opus-4-8@eu-central-1` |
| Azure OpenAI (EU) | `azure/` | `azure/gpt-5.4@swedencentral` |
| Mistral | `mistral/` | `mistral/mistral-medium-latest` |
| OpenWeight (Nebius/Inceptron) | `nebius/`, `inceptron/` | `nebius/kimi-k3` |

### `supports_vision` from `/v1/models` is not trustworthy

The router's `supports_vision` flag is metadata, not behaviour. A text-only model behind a
vision-accepting gateway returns **HTTP 200 and hallucinates a description** instead of erroring —
so a passing smoke test proves nothing. Verified 2026-07 by sending a PNG containing the single
word "CAKE":

| Model | `supports_vision` | Actual |
|---|---|---|
| `tensorx/kimi-k2.7-code` | true | read "CAKE" — real vision |
| `inceptron/kimi-k2.7-Code` | true | read "CAKE" — real vision |
| `tensorx/minimax-m3` | true | read "CAKE" — real vision |
| `sference/kimi-k3` | false | HTTP 400 — honest rejection |
| `tensorx/deepseek-v4-pro` | false | HTTP 200, answered "Elephant" — **silently blind** |
| `tensorx/deepseek-v4-flash` | false | HTTP 200, answered "horizon" — **silently blind** |
| `tensorx/qwen3.8` | true | HTTP 400 — honest rejection (checked 2026-09) |
| `mistral/mistral-large-4` | true | read "CAKE" — real vision (checked 2026-10) |
| `tensorx/qwen3.8-flash-next` | true | read "CAKE" — real vision (checked 2026-09) |
| `sference/deepseek-v4.1-flash` | true | read "CAKE" — real vision (checked 2026-09) |

Only put `"image"` in `input` after a model has read known text out of a test image. Generate one
with `magick -size 200x100 xc:white -pointsize 48 -fill black -annotate +20+65 "CAKE" test.png` and
send it as a base64 `image_url` data URL.

### Audio: `input_audio` is dropped silently

Requesty discards OpenAI-style `input_audio` parts (any `format`) with HTTP 200; the prompt-token
count shows nothing arrived. A `file` part with a data URL is delivered:

```json
{"type": "file", "file": {"filename": "say.mp3", "file_data": "data:audio/mpeg;base64,…"}}
```

Checked 2026-10 with a 4 s espeak clip (`espeak-ng -v de -w say.wav "<sentence>"`):

| Model | `input_audio` | `file` data URL |
|---|---|---|
| `vertex/gemini-4-argon@eu` | answered "NO AUDIO" | verbatim transcript |
| `vertex/gemini-3.8-flash@eu` | **invented** an unrelated English sentence | verbatim transcript |

Use a sentence the model cannot guess, and tell it to say "NO AUDIO" if it hears none.

### Open-weight capacity varies by route

The same model is often listed under several providers at different prices. Cheaper is not always
better: `inceptron/kimi-k2.7-Code` ($0.75/$3.50) returned **429 Too Many Requests** on 2 of 4 calls
during testing, while `tensorx/kimi-k2.7-code` ($1.25/$4.50) was reliable (both since retired
with K2.7). Load-test a cheap route before relying on it for long agent runs. The inceptron slug
capitalised `Code` and the tensorx one did not — the IDs are case-sensitive.

Also beware `max_output_tokens` from the router: inceptron/tensorx report it equal to the full
context window (e.g. 1048576), which is nonsense. Cap `maxTokens` at 128000, matching the other
entries.

### Verification

Test your configuration:

```bash
# Validate schema
pi --provider requesty-anthropic --model vertex/claude-sonnet-4-6@europe-west1

# Interactive test
pi
> /model
# Select your Requesty model
```

If validation fails, check:
1. Input array only contains "text" and/or "image"
2. JSON syntax is valid (trailing commas, quotes, brackets)
3. The model is on the organization's Approved Models / Access List
4. Environment variable `REQUESTY_API_KEY_CC` is set
