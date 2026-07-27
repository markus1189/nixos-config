/**
 * Fresh Session - carry last assistant response to a clean session
 *
 * Usage:
 *   /fresh                    - new session with last assistant message as context
 *   /fresh now do X instead   - same, but pre-fills editor with args
 */

import type { ExtensionAPI, SessionEntry } from "@earendil-works/pi-coding-agent";

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
      // Only plain data may cross into withSession: the old ctx/pi and anything
      // extracted from them (sessionManager, ...) are stale after replacement.
      const trimmed = args.trim();

      await ctx.newSession({
        parentSession,
        setup: async (sm) => {
          sm.appendMessage({
            role: "user",
            content: [{ type: "text", text: `[Note: This was the last assistant message from the previous session, sent here as a user message for context.]\n\n${text}` }],
            timestamp: Date.now(),
          });
        },
        withSession: async (freshCtx) => {
          if (trimmed) {
            freshCtx.ui.setEditorText(trimmed);
          }
          freshCtx.ui.notify("Fresh session ready", "info");
        },
      });
    },
  });
}
