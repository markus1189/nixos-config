/**
 * Fresh Session - carry last assistant response to a clean session
 *
 * Usage:
 *   /fresh                    - new session with last assistant message as context
 *   /fresh now do X instead   - same, but pre-fills editor with args
 */

import type { ExtensionAPI, SessionEntry } from "@mariozechner/pi-coding-agent";

export default function (pi: ExtensionAPI) {
  pi.registerCommand("fresh", {
    description: "New session seeded with the last assistant response",
    handler: async (args, ctx) => {
      const branch = ctx.sessionManager.getBranch();
      const lastAssistant = [...branch].reverse().find((e) => {
        if (e.type !== "message" || e.message.role !== "assistant")
          return false;
        return e.message.content.some(
          (c) => c.type === "text" && c.text.trim(),
        );
      }) as (SessionEntry & { type: "message" }) | undefined;

      if (!lastAssistant) {
        ctx.ui.notify("No assistant message with text content found", "error");
        return;
      }

      const text = lastAssistant.message.content
        .filter((c): c is { type: "text"; text: string } => c.type === "text")
        .map((c) => c.text)
        .join("\n");

      const parentSession = ctx.sessionManager.getSessionFile();
      const result = await ctx.newSession({
        parentSession,
        setup: async (sm) => {
          sm.appendMessage({
            role: "user",
            content: [{ type: "text", text }],
            timestamp: Date.now(),
          });
        },
      });

      if (result.cancelled) return;

      const trimmed = args.trim();
      if (trimmed) {
        ctx.ui.setEditorText(trimmed);
      }

      ctx.ui.notify("Fresh session ready", "info");
    },
  });
}
