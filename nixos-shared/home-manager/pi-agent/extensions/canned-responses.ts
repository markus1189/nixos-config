/**
 * Canned Responses Extension
 *
 * alt+r opens a picker of canned responses; picking one sends it immediately
 * as a user message, triggering the agent turn.
 *
 * ui.select returns the chosen label, so RESPONSES maps a short display label
 * to the message actually sent.
 */

import type {
  ExtensionAPI,
  ExtensionContext,
} from "@earendil-works/pi-coding-agent";

const RESPONSES: Record<string, string> = {
  "Go ahead": "Go ahead",
  Explore: "Explore options, explain tradeoffs",
  Rate: "Rate the options based on a set of criteria you choose according to relevance",
};

export default function (pi: ExtensionAPI) {
  pi.registerShortcut("alt+r", {
    description: "Open picker of canned responses and send the selected one",
    handler: async (ctx: ExtensionContext) => {
      if (!ctx.isIdle()) {
        ctx.ui.notify(
          "Canned responses unavailable while streaming",
          "warning",
        );
        return;
      }

      const labels = Object.keys(RESPONSES);
      const picked = await ctx.ui.select("Canned response", labels);
      if (!picked) return;

      const message = RESPONSES[picked];
      if (!message) {
        ctx.ui.notify(`No message mapped for label "${picked}"`, "error");
        return;
      }
      await pi.sendUserMessage(message);
    },
  });
}
