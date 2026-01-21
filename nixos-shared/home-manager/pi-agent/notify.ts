/**
 * Desktop Notification Extension
 *
 * Sends informative desktop notifications when agent finishes.
 * Uses dunstify for Dunst notification daemon.
 */

import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";
import { spawn } from "child_process";

function notify(title: string, body: string, urgency: "low" | "normal" | "critical" = "normal"): void {
        process.stdout.write('\u0007');
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
                const toolResults = messages.filter((m) => m.role === "toolResult");
                const toolCount = toolResults.length;
                const errorCount = toolResults.filter((m) => m.isError).length;

                // Calculate duration
                const duration = Math.round((Date.now() - startTime) / 1000);

                // Determine completion status
                const status = errorCount > 0 ? "Failed" : "Done";

                // Build summary
                const parts: string[] = [status];
                if (toolCount > 0) {
                        parts.push(`${toolCount} tool${toolCount !== 1 ? "s" : ""}`);
                }
                if (errorCount > 0) {
                        parts.push(`${errorCount} error${errorCount !== 1 ? "s" : ""}`);
                }
                if (duration > 0) {
                        parts.push(`${duration}s`);
                }

                const body = parts.join(" · ");

                // Set urgency based on outcome and complexity
                let urgency: "low" | "normal" ;
                if (toolCount < 3 || duration < 5) {
                        urgency = "low"; // Quick/simple tasks
                } else {
                        urgency = "normal"; // Regular successful completion
                }

                notify("Pi", body, urgency);
        });
}
