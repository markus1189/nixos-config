# Pi Agent Configuration Guide

## Converting Portkey Model Slugs to models.json Entries

Portkey uses virtual key slugs to route requests to different providers. This guide shows how to configure these models in pi agent's `models.json`.

### Model Slug Format

Portkey virtual key slugs follow this pattern:
```
@<provider>/<model-id>
```

Examples:
- `@vertex-ai/anthropic.claude-sonnet-4-5@20250929`
- `@azure-openai-foundry/gpt-5.1-codex`
- `@openai/gpt-4`

### Step 1: Find Model Specifications

Use the models.dev API to get accurate model specifications:

```bash
# Search for a specific model
curl -s https://models.dev/api.json | jq '.anthropic.models[] | select(.id == "claude-sonnet-4-5-20250929")'

# Or browse by provider
curl -s https://models.dev/api.json | jq '.openai.models[] | select(.id | contains("gpt-5.1-codex"))'
```

The API returns specifications like:
```json
{
  "id": "claude-sonnet-4-5-20250929",
  "name": "Claude Sonnet 4.5",
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
| `id` | `id` | Use Portkey slug, not base model ID |
| `name` | `name` | Add provider context (e.g., "via Portkey/Vertex") |
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

```json
{
  "providers": {
    "portkey": {
      "baseUrl": "https://api.portkey.ai/v1",
      "apiKey": "PORTKEY_API_KEY_CC",
      "api": "openai-completions",
      "authHeader": true,
      "models": [
        {
          "id": "@vertex-ai/anthropic.claude-sonnet-4-5@20250929",
          "name": "Claude Sonnet 4.5 (Portkey/Vertex)",
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
        },
        {
          "id": "@azure-openai-foundry/gpt-5.1-codex",
          "name": "GPT-5.1 Codex (Portkey/Azure)",
          "reasoning": true,
          "input": ["text", "image"],
          "cost": {
            "input": 1.25,
            "output": 10,
            "cacheRead": 0.125,
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
export PORTKEY_API_KEY_CC="your-portkey-api-key"
```

Add to `~/.bashrc` or `~/.zshrc` for persistence.

### Common Schema Validation Errors

**Error**: `/providers/portkey/models/0/input/2: must be equal to constant`
- **Cause**: Using "pdf" in input array
- **Fix**: Remove "pdf", only use "text" and "image"

**Error**: `/providers/portkey/models/0/cost: must have required property 'cacheRead'`
- **Cause**: Missing cache pricing fields
- **Fix**: Add `"cacheRead": 0` and `"cacheWrite": 0` if not applicable

### Supported APIs

The `api` field determines the protocol:
- `openai-completions`: OpenAI-compatible (recommended for Portkey)
- `openai-responses`: Alternative OpenAI format
- `anthropic-messages`: Native Anthropic format
- `google-generative-ai`: Google Gemini format

### Quick Reference: Common Portkey Providers

| Provider | Slug Prefix | Example |
|----------|-------------|---------|
| Vertex AI (Anthropic) | `@vertex-ai/` | `@vertex-ai/anthropic.claude-sonnet-4-5@20250929` |
| Azure OpenAI Foundry | `@azure-openai-foundry/` | `@azure-openai-foundry/gpt-5.1-codex` |
| OpenAI | `@openai/` | `@openai/gpt-4o` |
| Bedrock | `@bedrock/` | `@bedrock/anthropic.claude-sonnet-4-5` |

### Verification

Test your configuration:

```bash
# Validate schema
pi --provider portkey --model @vertex-ai/anthropic.claude-sonnet-4-5@20250929

# Interactive test
pi
> /model
# Select your Portkey model
```

If validation fails, check:
1. All required cost fields present (`input`, `output`, `cacheRead`, `cacheWrite`)
2. Input array only contains "text" and/or "image"
3. JSON syntax is valid (trailing commas, quotes, brackets)
4. Environment variable `PORTKEY_API_KEY_CC` is set
