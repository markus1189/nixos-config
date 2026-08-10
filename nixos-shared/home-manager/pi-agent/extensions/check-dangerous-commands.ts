/**
 * Dangerous command guard for pi — reuse of the Claude Code PreToolUse hook.
 *
 * Forwards the bash command to the shared check-dangerous-commands.sh backend,
 * which delegates detection to ast-grep + tree-sitter-bash. Because the backend
 * parses the command as bash, flags belonging to other commands, contents inside
 * strings, and shell comments do not trigger false positives.
 *
 * Matches the Claude hook's strict hard-block behaviour: no confirmation prompt,
 * the tool call is blocked in place. It does NOT terminate the turn — the
 * rule-specific reason is fed back to the model as a tool result so it can
 * see the hint and correct course on the next turn.
 */

import { spawnSync } from "child_process";
import { isToolCallEventType } from "@earendil-works/pi-coding-agent";
import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";

// Store path to the packaged check-dangerous-commands.sh (injected via pkgs.mutate).
const CHECK_SCRIPT = "@checkScript@";

// exit code 2 == the backend found a dangerous command and printed an explanation.
const BACKEND_BLOCKED = 2;

function runBackend(command: string): {
  status: number;
  message: string;
} {
  const input = JSON.stringify({
    tool_name: "Bash",
    tool_input: { command },
  });
  const res = spawnSync(CHECK_SCRIPT, [], {
    input,
    encoding: "utf8",
    timeout: 5000,
  });
  // The backend writes a rule-specific explanation to stderr. Forward it so the
  // model sees an accurate hint, not a generic one.
  const message = (res.stderr ?? "").trim();
  return { status: res.status ?? -1, message };
}

export default function (pi: ExtensionAPI) {
  pi.on("tool_call", async (event, ctx) => {
    if (event.toolName !== "bash") return;
    if (!isToolCallEventType("bash", event)) return;

    const { status, message } = runBackend(event.input.command);
    if (status === BACKEND_BLOCKED) {
      // No terminate: let the tool result (with the hint) feed back to the model
      // so it can correct course and continue on the next turn.
      const reason =
        message ||
        "Blocked: dangerous command. Prefer 'rm -r' for recursion, or " +
          "restrict 'find' to a concrete subpath.";
      return {
        block: true,
        reason,
      };
    }
  });
}