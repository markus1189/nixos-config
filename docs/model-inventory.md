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
| `nixos-shared/home-manager/zsh/default.nix` (~line 45) | `oc` / `pi` / `pi-glados` aliases — inject `REQUESTY_API_KEY_CC` from `pass api/requesty/agent`. No model slugs. |
| `nixos-shared/packages/emacs/emacs-config.el` (~line 1550) | **gptel** model list (OpenRouter). |
| `nixos-shared/packages/scripts/gemini-vision.sh:20` | `MODEL` const for the vision script. |
| `nixos-shared/agent-skills/transcribe-audio/scripts/transcribe.sh:26` | `MODEL` const for transcription. |
| `nixos-shared/agent-skills/nano-banana/scripts/nano-banana.sh:24,51-52` | image model mapping (`flash`/`pro`). |
| `nixos-shared/claude/claude-code-statusline.sh:19,61` | parses bedrock model names + gateway URL. |

> **Useful commands to find every mention of a model before/after editing:**
> - `rg -n "claude-sonnet|gemini-2.5|gpt-5.6" --ignore-case` (whole repo)
> - Slugs appear in `.nix`, `.json`, `.sh`, `.ts`, `.el`, `.md` — search all globs.

---

## 2. Model Family → Every Location

### Anthropic Claude (Requesty EU via Vertex / Bedrock / OAuth)

| Model slug used | Consumers (files) |
|-----------------|-------------------|
| `bedrock/claude-opus-5-5@eu-central-1` | `pi-agent/models.json` (requesty-anthropic), `model-shortcuts.ts` (F2 primary), `laptop/home.nix` (opencode) — Bedrock only, no Vertex slug on Requesty EU (checked 2026-09) |
| `vertex/claude-opus-5@eu` | `pi-agent/models.json` (requesty-anthropic), `model-shortcuts.ts` (F2 cycle), `laptop/home.nix` (opencode) |
| `vertex/claude-sonnet-5@eu` | `pi-agent/models.json`, `model-shortcuts.ts` (F3), `laptop/home.nix` |
| `vertex/claude-haiku-4-5@europe-west1` | `pi-agent/models.json`, `laptop/home.nix` |
| `bedrock/claude-haiku-4-5@eu-central-1` | `pi-agent/models.json` |

**gptel/Emacs (OpenRouter slug style, separate):**
- `anthropic/claude-sonnet-5`, `anthropic/claude-opus-5.5`, `anthropic/claude-opus-5` + `openai/gpt-5.2`, `openai/gpt-5.1`, `openai/gpt-5-mini` → `emacs-config.el:1555-1561`.
- OpenRouter uses a dot (`claude-opus-5.5`), Requesty a dash (`claude-opus-5-5`).

### Google Gemini (Requesty EU Vertex + OpenRouter)

| Model | Consumers |
|-------|-----------|
| `vertex/gemini-3.5-flash@eu` | `pi-agent/models.json` (requesty-google), `laptop/home.nix` (opencode) |
| `vertex/gemini-3.1-flash-lite@eu` | `pi-agent/models.json` |
| `vertex/gemini-3.7-flash@eu` | `pi-agent/models.json` |
| `vertex/gemini-3.8-flash@eu` | `pi-agent/models.json` (requesty-google), `laptop/home.nix` (opencode), `gemini-vision.sh:20` (Requesty /chat) — vision + tools verified 2026-09 |
| `google/gemini-3.5-flash`, `google/gemini-3.1-flash-lite` | `emacs-config.el` (gptel, OpenRouter) |
| `google/gemini-3.1-flash-image-preview` | `nano-banana.sh:51` (flash, OpenRouter) |
| `google/gemini-3-pro-image-preview` | `nano-banana.sh:52` (pro, OpenRouter) |
| `google/gemini-3.6-flash` | `transcribe.sh:26` (OpenRouter) |

### OpenAI GPT (Requesty EU via Azure)

| Model | Consumers |
|-------|-----------|
| `azure/gpt-6-sol@swedencentral` | `pi-agent/models.json` (requesty-openai) |
| `azure/gpt-5.6-terra@swedencentral` | `pi-agent/models.json` |
| `azure/gpt-6-luna@swedencentral` | `pi-agent/models.json` |
| `azure/gpt-5.4@swedencentral` | `pi-agent/models.json`, `laptop/home.nix` (opencode) |
| `azure/gpt-5.5@swedencentral` | `pi-agent/models.json` |

GPT-6 has no Terra tier, so `gpt-5.6-terra` stays as the middle option.

**gptel/Emacs (OpenRouter):** `openai/gpt-5.2`, `gpt-5.1`, `gpt-5-mini` — `emacs-config.el:1555-1557`.

### Chinese open-weight (GLM / Kimi / DeepSeek / MiniMax / Qwen) via Requesty EU

| Model | Consumers |
|-------|-----------|
| `tensorx/glm-5.2`, `inceptron/glm-5.2`, `sference/glm-5.2` | `pi-agent/models.json` (requesty-completions) |
| `tensorx/glm-5.3`, `sference/glm-5.3` | `pi-agent/models.json` (requesty-completions); F4 key → `sference/glm-5.3` in `model-shortcuts.ts` |
| `sference/glm-5.3-flash` | `pi-agent/models.json` (requesty-completions, vision); F4 key cycle → `model-shortcuts.ts` |
| `tensorx/glm-5.3-flash` | `pi-agent/models.json` (requesty-completions, **text-only**) — tools + reasoning work, but images are silently dropped (verified 2026-09; Requesty's capability flags claim no tools/reasoning, which is wrong) |
| `sference/kimi-k3`, `tensorx/kimi-k3` (vision) | `pi-agent/models.json` |
| `tensorx/kimi-k2.7-code`, `inceptron/kimi-k2.7-Code` | `pi-agent/models.json` |
| `sference/deepseek-v4.1-flash` (vision) | `pi-agent/models.json` (requesty-completions); F1 default in `model-shortcuts.ts` — vision + tools verified 2026-09 |
| `tensorx/deepseek-v4-pro-0813`, `tensorx/deepseek-v4-flash-0731`, `sference/deepseek-v4-flash-0731` | `pi-agent/models.json`; F1 cycle after v4.1-flash in `model-shortcuts.ts` |
| `tensorx/minimax-m3` | `pi-agent/models.json` |
| `tensorx/qwen3.8`, `tensorx/qwen3.8-flash-next` (vision) | `pi-agent/models.json` |

**gptel/Emacs (OpenRouter):** `deepseek/deepseek-v4-pro`, `deepseek/deepseek-v4-flash`, `mistralai/codestral-2508`, `meta-llama/llama-3.3-70b-instruct` — `emacs-config.el:1563-1568`.

---

## 3. Quick "what to update when X releases"

> **Rule of thumb:** a model lives in **1–4 places**. Catalog + keybindings + (maybe) opencode or gptel.
> `models.json` is always the anchor.

- **Claude sonnet/opus/haiku bump** → `models.json` (requesty-anthropic) + `model-shortcuts.ts` (if pinned) + `laptop/home.nix` (opencode) + `emacs-config.el` (gptel, OpenRouter slug). Watch the **rd git** `@eu`/`@europe-west1` region slug and the thinking/adaptive caveats in `laptop/home.nix` comments.
- **New Gemini tier** → `models.json` (requesty-google); if it becomes the vision/transcribe/banana workhorse, also `gemini-vision.sh`, `transcribe.sh`, `nano-banana.sh`.
- **New GPT-x** → `models.json` (requesty-openai, sol/terra/luna tiers) + optionally `laptop/home.nix`.
- **New GLM/Kimi/DeepSeek/MiniMax** → `models.json` (requesty-completions) + `model-shortcuts.ts` if shortcut-bound.
- **Any OpenRouter model** (gptel/Emacs, transcribe, banana) → `emacs-config.el`, `transcribe.sh`, `nano-banana.sh` — these are *independent* of the Requesty EU catalog.

---

## 4. Auth / key pointers

| Key secret | Used by |
|-----------|---------|
| `pass api/requesty/playground` | `gemini-vision.sh` |
| `pass api/requesty/agent` | `zsh/default.nix` (`oc`/`pi` aliases → `REQUESTY_API_KEY_CC`) |
| `REQUESTY_API_KEY_CC` (env) | `pi-agent/models.json`, `laptop/home.nix` (opencode) |
| `pass api/openrouter/transcribe` | `transcribe.sh` |
| `pass api/openrouter/image-editing` | `nano-banana.sh` |

---

## 5. Model-shortcut key map (pi agent) — from `model-shortcuts.ts`

| Key | Requesty primary | OpenRouter fallback |
|-----|------------------|---------------------|
| F1 | `sference/deepseek-v4.1-flash` → `sference/deepseek-v4-flash-0731` → `tensorx/deepseek-v4-flash-0731` → `tensorx/deepseek-v4-pro-0813` | — |
| F2 | `bedrock/claude-opus-5-5@eu-central-1` → `vertex/claude-opus-5@eu` | `anthropic/claude-opus-5.5` |
| F3 | `vertex/claude-sonnet-5@eu` | `anthropic/claude-sonnet-5` |
| F4 | `sference/glm-5.3` → `sference/glm-5.3-flash` | — |
| F5–F9 | (empty) | (empty) |