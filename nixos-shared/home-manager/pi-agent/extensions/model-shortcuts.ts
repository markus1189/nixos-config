// setModel lives on the ExtensionAPI (pi), not the handler ctx; the ctx has
// modelRegistry for lookup. See dist/core/extensions/runner.js.
import type {
  ExtensionAPI,
  ExtensionContext,
  ModelRegistry,
} from "@earendil-works/pi-coding-agent";

type ModelRef = { provider: string; modelId: string };

const SHORTCUT_KEYS = [
  "f1",
  "f2",
  "f3",
  "f4",
  "f5",
  "f6",
  "f7",
  "f8",
  "f9",
] as const;

// Ordered per slot; repeats cycle through the list, an empty slot is unbound.
const SLOTS: Record<(typeof SHORTCUT_KEYS)[number], ModelRef[]> = {
  f1: [
    {
      provider: "requesty-completions",
      modelId: "sference/deepseek-v4-flash-0731",
    },
    { provider: "openrouter", modelId: "deepseek/deepseek-v4-flash-0731" },
  ],
  f2: [
    { provider: "requesty-anthropic", modelId: "vertex/claude-opus-5@eu" },
    { provider: "openrouter", modelId: "anthropic/claude-opus-5" },
  ],
  f3: [
    { provider: "requesty-anthropic", modelId: "vertex/claude-sonnet-5@eu" },
    { provider: "openrouter", modelId: "anthropic/claude-sonnet-5" },
  ],
  f4: [{ provider: "requesty-completions", modelId: "sference/glm-5.2" }],
  f5: [],
  f6: [],
  f7: [],
  f8: [],
  f9: [],
};

function resolve(registry: ModelRegistry, ref: ModelRef) {
  return registry.find(ref.provider, ref.modelId);
}

export default function (pi: ExtensionAPI) {
  for (const key of SHORTCUT_KEYS) {
    const refs = SLOTS[key];
    if (refs.length === 0) continue;

    pi.registerShortcut(key, {
      description: `Cycle model for slot ${key}: ${refs.map((r) => r.modelId).join(" -> ")}`,
      handler: async (ctx: ExtensionContext) => {
        // Cycle anchored to the current active model: a press always starts at
        // the slot's first entry (or advances if already on one). No counter.
        const current = ctx.model;
        const curIdx = current
          ? refs.findIndex(
              (r) =>
                r.modelId === current.id && r.provider === current.provider,
            )
          : -1;
        const nextIdx = curIdx >= 0 ? (curIdx + 1) % refs.length : 0;
        const ref = refs[nextIdx];

        const model = resolve(ctx.modelRegistry, ref);
        if (!model) {
          ctx.ui.notify(
            `Model not found: ${ref.provider}/${ref.modelId}`,
            "error",
          );
          return;
        }

        const ok = await pi.setModel(model);
        ctx.ui.notify(
          ok
            ? `Switched to ${model.provider}/${model.id}`
            : `No API key for ${ref.provider}/${ref.modelId}`,
          ok ? "info" : "error",
        );
      },
    });
  }
}
