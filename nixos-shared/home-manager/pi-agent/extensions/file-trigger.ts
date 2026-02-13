/**
 * File Trigger Extension
 *
 * Watches a randomly-named trigger file and injects its contents into the conversation.
 * The file path is printed on startup so external tools can discover it.
 *
 * Usage:
 *   echo "Run the tests" > /tmp/agent-trigger-<random>.txt
 */

import * as fs from "node:fs";
import * as path from "node:path";
import * as crypto from "node:crypto";
import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";

export default function (pi: ExtensionAPI) {
  const triggerFile = path.join(
    "/tmp",
    `agent-trigger-${crypto.randomBytes(4).toString("hex")}.txt`,
  );

  pi.on("session_start", async (_event, ctx) => {
    // Create the file so fs.watch works
    fs.writeFileSync(triggerFile, "");

    fs.watch(triggerFile, () => {
      try {
        const content = fs.readFileSync(triggerFile, "utf-8").trim();
        if (content) {
          pi.sendMessage(
            {
              customType: "file-trigger",
              content: `External trigger: ${content}`,
              display: true,
            },
            { triggerTurn: true },
          );
          fs.writeFileSync(triggerFile, "");
        }
      } catch {
        // File might have been deleted
      }
    });

    if (ctx.hasUI) {
      ctx.ui.setStatus("file-trigger", ctx.ui.theme.fg("dim", `Trigger: ${triggerFile}`));
    }
  });

  pi.on("session_shutdown", async () => {
    try {
      fs.unlinkSync(triggerFile);
    } catch {
      // Already gone
    }
  });
}
