/**
 * Popout - branch to root of current session, seed context with last reply
 *
 * The session tree is preserved in place; the leaf is moved to the root so the
 * outgoing work stays reachable via /tree. A custom message (role: custom,
 * participates in LLM context) carries only the LAST assistant reply forward.
 * The editor is prefilled with /popout args so the user's new prompt follows
 * that context.
 *
 *   /popout              - root + last assistant reply as context
 *   /popout now do X     - same, editor prefilled with args
 */

import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";

export default function (pi: ExtensionAPI) {
  pi.registerCommand("popout", {
    description: "Branch to session root; keep tree; seed context with last reply",
    handler: async (args, ctx) => {
      const trimmed = args.trim();

      // Root -> leaf.
      const branch = ctx.sessionManager.getBranch();

      // Last assistant message that has visible text content.
      const lastAssistant = [...branch].reverse().find(
        (e): e is import("@earendil-works/pi-coding-agent").SessionMessageEntry =>
          e.type === "message" &&
          e.message.role === "assistant" &&
          e.message.content.some((c) => c.type === "text" && c.text.trim()),
      );

      const text = lastAssistant?.message.content
        .filter((c): c is { type: "text"; text: string } => c.type === "text")
        .map((c) => c.text)
        .join("\n")
        .trim();

      if (!text) {
        ctx.ui.notify("No assistant message with text content found", "error");
        return;
      }

      // Branch to the root: the tree is preserved, history stays reachable via /tree.
      //
      // "Root" = the end of the leading settings run (model_change / thinking_level_change),
      // NOT entry [0]. Those entries project to zero context messages, so the popped-out
      // branch is just as empty either way - but they carry the model and thinking level,
      // which are re-derived from the branch path when the session is resumed later.
      // Fork above them and a resumed popout branch comes back with thinking level "off".
      const firstMessageIdx = branch.findIndex(
        (e) => e.type === "message" || e.type === "custom_message",
      );
      // No settings prefix (or no messages at all): fall back to entry [0].
      const branchRoot = firstMessageIdx > 0 ? branch[firstMessageIdx - 1] : branch[0];
      if (branchRoot) {
        await ctx.waitForIdle();
        const { cancelled } = await ctx.navigateTree(branchRoot.id, {
          summarize: false, // never summarize: the branch IS the context
          label: "popout",
        });
        if (cancelled) {
          ctx.ui.notify("Popout cancelled", "info");
          return;
        }
      }

      // Seed context with the last assistant reply. Custom message => role: custom => sent to LLM.
      // No options: idle + no triggerTurn is the only path that appends the entry to the
      // session (persisted at the new leaf) and renders it, without firing a turn.
      // Do NOT pass deliverAs: "nextTurn" - that short-circuits ahead of the idle path, keeps
      // the message in memory only, and injects it AFTER the next user prompt.
      //
      // The framing is load-bearing, not decoration: convertToLlm() flattens role "custom"
      // into a plain role "user" message, so without it the model receives the carried reply
      // as if the user had typed it, with no hint of where it came from. It renders as a
      // distinct block in the UI, but the model only ever sees the text.
      pi.sendMessage({
        customType: "popout-context",
        content: `[popout] Your own last reply, carried into a fresh branch as the only context:\n\n${text}`,
        display: true,
      });

      // Unconditional: navigateTree prefills the editor from the target when the target is a
      // user/custom message, so an empty args must actively clear it, not leave it be.
      ctx.ui.setEditorText(trimmed);
      ctx.ui.notify("Popped out — last reply seeded as context", "info");
    },
  });
}
