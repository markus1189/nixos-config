/**
 * Desktop Notification Extension
 *
 * Sends informative desktop notifications when agent finishes.
 * Uses dunstify for Dunst notification daemon.
 */

import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { spawn } from "child_process";

function notify(
  title: string,
  body: string,
  urgency: "low" | "normal" | "critical" = "normal",
): void {
  process.stdout.write("\u0007");
  spawn("dunstify", ["-a", "Pi", "-u", urgency, title, body], {
    detached: true,
    stdio: "ignore",
  }).unref();
}

export default function (pi: ExtensionAPI) {
  let startTime = 0;

  pi.on("agent_start", async () => {
    startTime = Date.now();
  });

  pi.on("agent_end", async (event) => {
    const messages = event.messages;

    // Count tool calls and errors
    const toolResults = messages.filter((m): m is Extract<typeof m, { role: "toolResult" }> =>
      m.role === "toolResult",
    );
    const toolCount = toolResults.length;
    const errorCount = toolResults.filter((m) => m.isError).length;

    // Sum usage across assistant messages in this run
    const usage = messages.reduce(
      (acc, m) => {
        if (m.role !== "assistant") return acc;
        const u = (m as { usage?: { input?: number; output?: number; cacheRead?: number; cacheWrite?: number; cost?: { total?: number } } }).usage;
        if (!u) return acc;
        acc.input += u.input ?? 0;
        acc.output += u.output ?? 0;
        acc.cacheRead += u.cacheRead ?? 0;
        acc.cacheWrite += u.cacheWrite ?? 0;
        acc.cost += u.cost?.total ?? 0;
        return acc;
      },
      { input: 0, output: 0, cacheRead: 0, cacheWrite: 0, cost: 0 },
    );

    // Calculate duration
    const duration = Math.round((Date.now() - startTime) / 1000);

    // Build summary
    const parts: string[] = ["Done"];
    if (toolCount > 0) {
      parts.push(`${toolCount} tool${toolCount !== 1 ? "s" : ""}`);
    }
    if (errorCount > 0) {
      parts.push(`${errorCount} error${errorCount !== 1 ? "s" : ""}`);
    }
    const tokens = usage.input + usage.output;
    if (tokens > 0) {
      parts.push(`${tokens.toLocaleString()} tok`);
    }
    if (usage.cost > 0) {
      parts.push(`$${usage.cost.toFixed(4)}`);
    }
    if (duration > 0) {
      parts.push(`${duration}s`);
    }

    // Add TMUX_PANE if set
    const tmuxPane = process.env.TMUX_PANE;
    if (tmuxPane) {
      parts.push(`pane:${tmuxPane}`);
    }

    const body = parts.join(" · ");

    // Set urgency based on outcome and complexity
    let urgency: "low" | "normal";
    if (toolCount < 3 || duration < 5) {
      urgency = "low"; // Quick/simple tasks
    } else {
      urgency = "normal"; // Regular successful completion
    }

    notify("Pi", body, urgency);
  });
}
