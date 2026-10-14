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
| `laptop/home.nix` (~line 103) | **opencode** provider `requesty-cc` (`@ai-sdk/openai-compatible`, Requesty EU): a hand-picked subset of `models.json`. Vet new entries with `pi-agent/ADD_MODEL.md` (access list, tools, CAKE test). |
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
| `bedrock/claude-opus-5-5@eu-central-1` | `pi-agent/models.json` (requesty-anthropic), `model-shortcuts.ts` (F2), `laptop/home.nix` (opencode) — Bedrock only, no Vertex slug on Requesty EU (checked 2026-09) |
| `vertex/claude-sonnet-5-5@eu` | `pi-agent/models.json`, `model-shortcuts.ts` (F3), `laptop/home.nix` (opencode) — Vertex only, no Bedrock slug (checked 2026-10) |
| `vertex/claude-haiku-5-5@eu` | `pi-agent/models.json`, `laptop/home.nix` (opencode) — adaptive thinking verified 2026-10 |

**gptel/Emacs (OpenRouter slug style, separate):**
- `anthropic/claude-sonnet-5`, `anthropic/claude-opus-5.5`, `anthropic/claude-opus-5` + `openai/gpt-5.2`, `openai/gpt-5.1`, `openai/gpt-5-mini` → `emacs-config.el:1555-1561`.
- OpenRouter uses a dot (`claude-opus-5.5`), Requesty a dash (`claude-opus-5-5`).

### Google Gemini (Requesty EU Vertex + OpenRouter)

| Model | Consumers |
|-------|-----------|
| `vertex/gemini-3.8-flash@eu` | `pi-agent/models.json` (requesty-google), `laptop/home.nix` (opencode, vision), `gemini-vision.sh:20` (Requesty /chat) — vision + tools verified 2026-09 |
| `vertex/gemini-3.5-flash-lite@eu` | `pi-agent/models.json` (requesty-google), `laptop/home.nix` (opencode, vision) — vision + opencode tool run verified 2026-10 |
| `vertex/gemini-4-argon@eu` | `pi-agent/models.json` (requesty-google), `laptop/home.nix` (opencode, vision) — CAKE, streaming tools + `reasoning_effort`, pi and opencode two-tool runs verified 2026-10. Some prompts (e.g. "read /etc/hostname") come back HTTP 200 with empty `choices` and 0 tokens, no error |
| `google/gemini-3.5-flash`, `google/gemini-3.1-flash-lite` | `emacs-config.el` (gptel, OpenRouter) |
| `google/gemini-3.1-flash-image-preview` | `nano-banana.sh:51` (flash, OpenRouter) |
| `google/gemini-3-pro-image-preview` | `nano-banana.sh:52` (pro, OpenRouter) |
| `google/gemini-3.6-flash` | `transcribe.sh:26` (OpenRouter) |

### OpenAI GPT (Requesty EU via Azure)

| Model | Consumers |
|-------|-----------|
| `azure/gpt-6.1-sol@swedencentral` | `pi-agent/models.json` (requesty-openai), `laptop/home.nix` (opencode) — streaming tools + `reasoning_effort` verified 2026-10 |
| `azure/gpt-6-luna@swedencentral` | `pi-agent/models.json`, `laptop/home.nix` (opencode) |

**gptel/Emacs (OpenRouter):** `openai/gpt-5.2`, `gpt-5.1`, `gpt-5-mini` — `emacs-config.el:1555-1557`.

### Chinese open-weight (GLM / Kimi / DeepSeek / MiniMax / Qwen) via Requesty EU

| Model | Consumers |
|-------|-----------|
| `sference/deepseek-v4.1-flash` (vision) | `pi-agent/models.json` (requesty-completions), `laptop/home.nix` (opencode); F1 default in `model-shortcuts.ts` |
| `sference/glm-5.3` | `pi-agent/models.json`, `laptop/home.nix` (opencode, text-only); F4 primary |
| `sference/glm-5.3-flash` (vision) | `pi-agent/models.json`, `laptop/home.nix` (opencode); F4 cycle |
| `tensorx/kimi-k3` (vision) | `pi-agent/models.json`, `laptop/home.nix` (opencode) |
| `tensorx/minimax-m3` (vision) | `pi-agent/models.json`, `laptop/home.nix` (opencode) |
| `tensorx/qwen3.8` (text-only), `tensorx/qwen3.8-flash-next` (vision) | `pi-agent/models.json`, `laptop/home.nix` (opencode) |
| `tensorx/glm-5.3`, `sference/kimi-k3` (text-only) | `pi-agent/models.json` — alternate routes, pi only |
| `tensorx/glm-5.3-flash` | `pi-agent/models.json` (**text-only**) — tools + reasoning work, but images are silently dropped (verified 2026-09; Requesty's capability flags claim no tools/reasoning, which is wrong) |

Every opencode entry above passed a two-tool-call `opencode2 run` (read + shell, reasoning replayed) and, where marked vision, the CAKE test (2026-10).

**gptel/Emacs (OpenRouter):** `deepseek/deepseek-v4-pro`, `deepseek/deepseek-v4-flash`, `mistralai/codestral-2508`, `meta-llama/llama-3.3-70b-instruct` — `emacs-config.el:1563-1568`.

### Mistral via Requesty EU

| Model | Consumers |
|-------|-----------|
| `mistral/mistral-large-4` | `pi-agent/models.json` (requesty-completions, vision) — vision, tools, streaming tool calls + `reasoning_effort` verified 2026-10; router reports `max_output_tokens: 0`, capped at 128000 |

Mistral wants reasoning replayed as a `ThinkChunk` in `content` ([docs](https://docs.mistral.ai/studio/conversations/reasoning.md), "Multi-turn"), but Requesty rejects that (400 `unknown unmarshal error`) and passes its own `reasoning_content` through untranslated (422 `extra_forbidden`). Route-wide: `mistral/mistral-medium-3-5` and `mistral/glm-5-2` 422 too, while azure, vertex and sference accept a replayed `reasoning_content` (checked 2026-10). Requesty documents neither field ([schema](https://docs.requesty.ai/api-reference/endpoint/chat-completions-create.md): `content` is a string), so this is a gap, not a broken contract. pi therefore replays thinking as plain text (`compat.requiresThinkingAsText`), which keeps the trace but is off-spec. opencode has no such switch and 422s after the first tool call, so the model is not in its provider block. pi's native `mistral-conversations` API is no way out: it ignores Requesty's `reasoning_content` and would silently drop all reasoning.

---

## 3. Quick "what to update when X releases"

> **Rule of thumb:** a model lives in **1–4 places**. Catalog + keybindings + (maybe) opencode or gptel.
> `models.json` is always the anchor.
>
> **Policy:** keep only the newest generation of every family (Claude, GPT, Gemini, open-weight), in both pi and opencode.
> opencode mirrors pi's newest-generation entries (one route per model).
> Prices in `models.json` come from the keyed `/v1/models` (see `ADD_MODEL.md`).

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
| F1 | `sference/deepseek-v4.1-flash` | — |
| F2 | `bedrock/claude-opus-5-5@eu-central-1` | `anthropic/claude-opus-5.5` |
| F3 | `vertex/claude-sonnet-5-5@eu` | `anthropic/claude-sonnet-5.5` |
| F4 | `sference/glm-5.3` → `sference/glm-5.3-flash` | — |
| F5–F9 | (empty) | (empty) |