# Pi Agent Configuration Guide

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
- `azure/openai-responses/gpt-5.4@swedencentral`
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
      "api": "openai-responses",
      "authHeader": true,
      "models": [
        {
          "id": "azure/openai-responses/gpt-5.4@swedencentral",
          "name": "GPT-5.4 (Requesty/Azure EU)",
          "reasoning": true,
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

**Error**: `400 Function tools with reasoning_effort are not supported for <model> in /v1/chat/completions. Please use /v1/responses instead.`
- **Cause**: A reasoning GPT model configured under an `openai-completions` provider. Requesty
  exposes GPT models under both a bare slug (chat completions) and an `openai-responses/` slug;
  the bare one rejects tools + `reasoning_effort`, which is every pi request.
- **Fix**: Use the `openai-responses/...` slug under an `openai-responses` provider. Do not add
  the bare GPT slugs — same price, no web search, unusable with tools.
- **Note**: This only fails when tools are enabled, so a `--no-tools` smoke test will not catch it.
  Verify new models *with* tools.

### Supported APIs

The `api` field determines the protocol:
- `anthropic-messages`: Native Anthropic format (Claude models — recommended)
- `openai-responses`: OpenAI Responses format (GPT / Gemini via the `/v1` endpoint)
- `openai-completions`: OpenAI-compatible chat completions
- `google-generative-ai`: Google Gemini format

### Quick Reference: Common Requesty Providers

| Provider | Slug Prefix | Example |
|----------|-------------|---------|
| Vertex AI (EU) | `vertex/` | `vertex/claude-sonnet-4-6@europe-west1` |
| Bedrock (EU) | `bedrock/` | `bedrock/claude-opus-4-8@eu-central-1` |
| Azure OpenAI (EU) | `azure/` | `azure/openai-responses/gpt-5.4@swedencentral` |
| Mistral | `mistral/` | `mistral/mistral-medium-latest` |
| OpenWeight (Nebius/Inceptron) | `nebius/`, `inceptron/` | `nebius/moonshotai/kimi-k2.5` |

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
