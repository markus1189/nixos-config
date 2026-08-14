# Model Inventory & Update Pointers

**Purpose:** Every AI model used across this repo, with the *exact file/line* to touch when a model
releases/updates. Glance at the consumer list under each model; update all matching pointers.

**Key stores:** Two gateways — **Requesty EU** (`router.eu.requesty.ai`) for agents+vision, **OpenRouter**
for image/transcribe/emacs. Model slugs appear in **multiple independent files** — updating one is not enough.

---

## 1. Master catalogs (add/remove models here first)

| File | What it holds |
|------|---------------|
| `nixos-shared/home-manager/pi-agent/models.json` | **The canonical model catalog** — pi agent's full provider+model+pricelist. Symlinked to `~/.pi/agent/models.json`. |
| `nixos-shared/home-manager/pi-agent/extensions/model-shortcuts.ts` | F1–F9 keybindings → provider/modelId pairs (Requesty + fallback openrouter). |
| `laptop/home.nix` (~line 78) | **opencode** provider block (`@ai-sdk/openai-compatible`, Requesty EU) with its own model list. |
| `nixos-shared/home-manager/zsh/default.nix` (~line 20) | **gptel alias + claude-code wrapper** — defines haiku/sonnet/opus vertex models + `ANTHROPIC_BASE_URL`. |
| `nixos-shared/packages/emacs/emacs-config.el` (~line 1530) | **gptel** model list (OpenRouter). |
| `nixos-shared/packages/scripts/gemini-vision.sh:21` | `MODEL` const for the vision script. |
| `nixos-shared/claude/skills/transcribe-audio/scripts/transcribe.sh:26` | `MODEL` const for transcription. |
| `nixos-shared/claude/skills/nano-banana/scripts/nano-banana.sh:24,51-52` | image model mapping (`flash`/`pro`). |
| `nixos-shared/claude/claude-code-statusline.sh:19,61` | parses bedrock model names + gateway URL. |

> **Useful commands to find every mention of a model before/after editing:**
> - `rg -n "claude-sonnet|gemini-2.5|gpt-5.6" --ignore-case` (whole repo)
> - Slugs appear in `.nix`, `.json`, `.sh`, `.ts`, `.el`, `.md` — search all globs.

---

## 2. Model Family → Every Location

### Anthropic Claude (Requesty EU via Vertex / Bedrock / OAuth)

| Model slug used | Consumers (files) |
|-----------------|-------------------|
| `vertex/claude-opus-5@eu` | `pi-agent/models.json` (requesty-anthropic), `model-shortcuts.ts` (F2), `laptop/home.nix` (opencode), `zsh/default.nix` (opus-vertex alias) |
| `vertex/claude-sonnet-5@eu` | `pi-agent/models.json`, `model-shortcuts.ts` (F3), `laptop/home.nix`, `zsh/default.nix` (sonnet-vertex) |
| `vertex/claude-opus-4-7@eu` | `pi-agent/models.json` (requesty-anthropic) |
| `vertex/claude-haiku-4-5@europe-west1` | `pi-agent/models.json`, `zsh/default.nix` (haiku-vertex), `laptop/home.nix` |
| `vertex/claude-sonnet-4.6@eu` (older) | `laptop/home.nix` (opencode; comment) |
| `bedrock/claude-opus-4-8@eu-central-1` | `pi-agent/models.json`, `laptop/home.nix` (opencode) |
| `bedrock/claude-sonnet-4-6@eu-central-1` | `pi-agent/models.json` (both requesty-anthropic + requesty-claude-thinking), `laptop/home.nix` |
| `bedrock/claude-haiku-4-5@eu-central-1` | `pi-agent/models.json` |

**gptel/Emacs (OpenRouter slug style, separate):**
- `anthropic/claude-opus-4`, `anthropic/claude-sonnet-4`, `anthropic/claude-3.7-sonnet`, `anthropic/claude-3.7-sonnet:thinking` → `emacs-config.el:1532-1536`.

### Google Gemini (Requesty EU Vertex + OpenRouter)

| Model | Consumers |
|-------|-----------|
| `vertex/gemini-2.5-pro@europe-west1` | `pi-agent/models.json` (requesty-google), `laptop/home.nix` (opencode) |
| `vertex/gemini-3.1-flash-lite@eu` | `pi-agent/models.json` |
| `vertex/gemini-3.7-flash@eu` | `pi-agent/models.json` |
| `vertex/gemini-2.5-flash@europe-west1` | `gemini-vision.sh:21` (Requesty /chat) |
| `google/gemini-2.5-pro-preview`, `google/gemini-2.5-flash-preview` | `emacs-config.el` (gptel, OpenRouter) |
| `google/gemini-3.1-flash-image-preview` | `nano-banana.sh:51` (flash, OpenRouter) |
| `google/gemini-3-pro-image-preview` | `nano-banana.sh:52` (pro, OpenRouter) |
| `google/gemini-3.6-flash` | `transcribe.sh:26` (OpenRouter) |

### OpenAI GPT (Requesty EU via Azure)

| Model | Consumers |
|-------|-----------|
| `azure/gpt-5.6-sol@swedencentral` | `pi-agent/models.json` (requesty-openai) |
| `azure/gpt-5.6-terra@swedencentral` | `pi-agent/models.json` |
| `azure/gpt-5.6-luna@swedencentral` | `pi-agent/models.json` |
| `azure/gpt-5.4@swedencentral` | `pi-agent/models.json`, `laptop/home.nix` (opencode) |
| `azure/gpt-5.5@swedencentral` | `pi-agent/models.json` |

**gptel/Emacs (OpenRouter):** `openai/gpt-4.1`, `gpt-4.1-mini`, `gpt-4.1-nano`, `gpt-4o`, `gpt-4o-mini` — `emacs-config.el:1530-1533`.

### Chinese open-weight (GLM / Kimi / DeepSeek / MiniMax) via Requesty EU

| Model | Consumers |
|-------|-----------|
| `tensorx/glm-5.2`, `inceptron/glm-5.2`, `sference/glm-5.2` | `pi-agent/models.json` (requesty-completions); F4 key → `sference/glm-5.2` in `model-shortcuts.ts` |
| `sference/kimi-k3`, `tensorx/kimi-k3` (vision) | `pi-agent/models.json` |
| `tensorx/kimi-k2.7-code`, `inceptron/kimi-k2.7-Code` | `pi-agent/models.json` |
| `nebius/moonshotai/kimi-k2.5` | `laptop/home.nix` (opencode) |
| `tensorx/deepseek-v4-pro`, `deepseek-v4-flash`, `sference/deepseek-v4-flash-0731` | `pi-agent/models.json`; F1 → `sference/deepseek-v4-flash-0731` in `model-shortcuts.ts` |
| `tensorx/minimax-m3` | `pi-agent/models.json` |

**gptel/Emacs (OpenRouter):** `deepseek/deepseek-r1:free`, `deepseek/deepseek-chat-v3-0324`, `mistralai/codestral-2501`, `meta-llama/llama-3.3-70b-instruct` — `emacs-config.el:1538-1546`.

---

## 3. Quick "what to update when X releases"

> **Rule of thumb:** a model lives in **1–4 places**. Catalog + keybindings + (maybe) opencode or gptel.
> `models.json` is always the anchor.

- **Claude sonnet/opus/haiku bump** → `models.json` (requesty-anthropic) + `model-shortcuts.ts` (if pinned) + `laptop/home.nix` (opencode) + `zsh/default.nix` (aliases). Watch the **rd git** `@eu`/`@europe-west1` region slug and the thinking/adaptive caveats in `laptop/home.nix` comments.
- **New Gemini tier** → `models.json` (requesty-google); if it becomes the vision/transcribe/banana workhorse, also `gemini-vision.sh`, `transcribe.sh`, `nano-banana.sh`.
- **New GPT-5.x** → `models.json` (requesty-openai, keep sol/terra/luna triad) + optionally `laptop/home.nix`.
- **New GLM/Kimi/DeepSeek/MiniMax** → `models.json` (requesty-completions) + `model-shortcuts.ts` if shortcut-bound.
- **Any OpenRouter model** (gptel/Emacs, transcribe, banana) → `emacs-config.el`, `transcribe.sh`, `nano-banana.sh` — these are *independent* of the Requesty EU catalog.

---

## 4. Auth / key pointers

| Key secret | Used by |
|-----------|---------|
| `pass api/requesty/playground` | `gemini-vision.sh` |
| `pass api/requesty/claude-code` | `zsh/default.nix` claude wrapper |
| `REQUESTY_API_KEY_CC` (env) | `pi-agent/models.json`, `laptop/home.nix` (opencode) |
| `pass api/openrouter/transcribe` | `transcribe.sh` |
| `pass api/openrouter/image-editing` | `nano-banana.sh` |
| `pass api/kagi/search` | `kagi-extract.sh` |

---

## 5. Model-shortcut key map (pi agent) — from `model-shortcuts.ts`

| Key | Requesty primary | OpenRouter fallback |
|-----|------------------|---------------------|
| F1 | `sference/deepseek-v4-flash-0731` | `deepseek/deepseek-v4-flash-0731` |
| F2 | `vertex/claude-opus-5@eu` | `anthropic/claude-opus-5` |
| F3 | `vertex/claude-sonnet-5@eu` | `anthropic/claude-sonnet-5` |
| F4 | `sference/glm-5.2` | — |
| F5–F9 | (empty) | (empty) |