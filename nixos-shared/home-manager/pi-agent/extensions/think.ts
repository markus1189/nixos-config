/**
 * Think Tool - Gives the model a dedicated space to reason during complex tasks.
 *
 * Based on Anthropic's "think" tool concept. The tool doesn't fetch data or
 * change state — it just acknowledges the thought, giving the model a scratchpad
 * for structured reasoning between tool calls.
 *
 * Most useful for:
 * - Analyzing tool outputs before deciding next steps
 * - Policy-heavy environments with complex rules
 * - Sequential decision-making in long tool call chains
 *
 * Usage: Copy to ~/.pi/agent/extensions/ or .pi/extensions/
 */

import { Type } from "@mariozechner/pi-ai";
import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";
import { keyHint } from "@mariozechner/pi-coding-agent";
import { Text } from "@mariozechner/pi-tui";

export default function (pi: ExtensionAPI) {
  pi.registerTool({
    name: "think",
    label: "Think",
    description:
      "Use this tool to think about something. It will not obtain new information or make any changes, but just log the thought. Use it when complex reasoning or brainstorming is needed. For example, if you explore a repo and discover the source of a bug, call this tool to brainstorm several unique ways of fixing it and assess which approach is simplest and most effective. Or if you receive test results, use it to reason about how to fix failing tests. Also useful for planning multi-step actions, verifying compliance with requirements, or processing tool outputs before acting.",
    parameters: Type.Object({
      thought: Type.String({ description: "Your thought or reasoning." }),
    }),

    renderCall(args: { thought?: string }, theme) {
      const { thought } = args;
      if (!thought) {
        return new Text(theme.fg("toolTitle", theme.bold("Think")), 0, 0);
      }
      const words = thought.trim().split(/\s+/);
      const trail =
        words.length > 50
          ? "… " + words.slice(-50).join(" ")
          : thought;
      return new Text(
        theme.fg("toolTitle", theme.bold("Think")) + "\n" + theme.fg("dim", trail),
        0,
        0,
      );
    },

    async execute(_toolCallId, params, _signal, _onUpdate, _ctx) {
      const { thought } = params as { thought: string };
      const wordCount = thought.trim().split(/\s+/).length;
      return {
        content: [{ type: "text", text: "Thought noted." }],
        details: { thought, wordCount },
      };
    },

    renderResult(result, { expanded }, theme) {
      const { thought, wordCount } = (result as any).details ?? {};
      if (!thought) {
        return new Text(theme.fg("dim", "Empty thought"), 0, 0);
      }

      if (expanded) {
        return new Text(thought, 0, 0);
      }

      return new Text(
        theme.fg("dim", `${wordCount} words`) +
          ` (${keyHint("expandTools", "to expand")})`,
        0,
        0,
      );
    },
  });
}
