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
- `nebius/moonshotai/kimi-k2.5`

Only models on the organization's **Approved Models / Access List** are routable. Check the
Requesty dashboard (or the `cc Playground` group's access list) for the current set.

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
| `cost.input` | `cost.input` | Direct mapping ($/M tokens) |
| `cost.output` | `cost.output` | Direct mapping ($/M tokens) |
| `cost.cache_read` | `cost.cacheRead` | **Required field** - use 0 if not provided |
| `cost.cache_write` | `cost.cacheWrite` | **Required field** - use 0 if not provided |
| `limit.context` | `contextWindow` | Direct mapping |
| `limit.output` | `maxTokens` | Direct mapping |

### Step 3: Create models.json Entry

Location: `~/.pi/agent/models.json`

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
          "cost": {
            "input": 3,
            "output": 15,
            "cacheRead": 0.3,
            "cacheWrite": 3.75
          },
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
          "cost": {
            "input": 1.75,
            "output": 14,
            "cacheRead": 0.175,
            "cacheWrite": 0
          },
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

**Error**: `/providers/requesty-anthropic/models/0/cost: must have required property 'cacheRead'`
- **Cause**: Missing cache pricing fields
- **Fix**: Add `"cacheRead": 0` and `"cacheWrite": 0` if not applicable

**Error**: `Cannot read properties of undefined (reading 'startsWith')`
- **Cause**: The model is registered under an `openai-responses` provider (`api:
  "openai-responses"`, slug prefixed `openai-responses/`). pi's Responses parser reads
  `event.arguments` on the `response.function_call_arguments.done` stream event, but Requesty
  emits that event with `delta` and **no** `arguments` field when a tool is invoked, so pi crashes
  on the first real tool call. Every reasoning model on that route is affected.
- **Affected**: the reasoning GPT and Gemini models previously under `requesty-openai`.
- **Fix**: Register all Requesty models (GPT, Gemini, open-weight) under an
  `openai-completions` provider using the **bare** slug (no `openai-responses/` prefix) e.g.
  `azure/gpt-5.6-sol@swedencentral`
- **Note**: This only fails when tools are enabled, so a `--no-tools` smoke test will not catch it.
  Verify new models *with* tools. Verified 2026-08: bare GPT-5.6 Sol/Terra/Luna, 5.5 and 5.4
  slugs all accept tools + `reasoning_effort` on `/v1/chat/completions`.

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
| OpenWeight (Nebius/Inceptron) | `nebius/`, `inceptron/` | `nebius/moonshotai/kimi-k2.5` |

Enumerate the current access list rather than guessing at slugs:

```bash
curl -s https://router.eu.requesty.ai/v1/models \
  -H "Authorization: Bearer $(pass api/requesty/agent)" | jq -r '.data[].id' | sort
```

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

Only put `"image"` in `input` after a model has read known text out of a test image. Generate one
with `magick -size 200x100 xc:white -pointsize 48 -fill black -annotate +20+65 "CAKE" test.png` and
send it as a base64 `image_url` data URL.

### Open-weight capacity varies by route

The same model is often listed under several providers at different prices. Cheaper is not always
better: `inceptron/kimi-k2.7-Code` ($0.75/$3.50) returned **429 Too Many Requests** on 2 of 4 calls
during testing, while `tensorx/kimi-k2.7-code` ($1.25/$4.50) was reliable. Both are configured;
prefer the tensorx route for long agent runs. Note the inceptron slug capitalises `Code` and the
tensorx one does not — the IDs are case-sensitive.

Also beware `max_output_tokens` from the router: inceptron/tensorx report it equal to the full
context window (e.g. 1048576), which is nonsense. Cap `maxTokens` at 128000, matching the other
entries.

**Gemini on the EU list is thin** (checked 2026-07): only `vertex/gemini-2.5-flash@europe-west1`,
`vertex/gemini-2.5-pro@europe-west1` and `vertex/gemini-3.1-flash-lite@eu`. There is no Gemini 3.x
pro or non-lite flash, so `gemini-2.5-pro` stays the Gemini entry here despite its age —
3.1-flash-lite has `supports_reasoning: false` and is not an agent-tier replacement.

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
1. All required cost fields present (`input`, `output`, `cacheRead`, `cacheWrite`)
2. Input array only contains "text" and/or "image"
3. JSON syntax is valid (trailing commas, quotes, brackets)
4. The model is on the organization's Approved Models / Access List
5. Environment variable `REQUESTY_API_KEY_CC` is set
