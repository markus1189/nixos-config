/**
 * Dangerous command guard for pi — reuse of the Claude Code PreToolUse hook.
 *
 * Forwards the bash command to the shared check-dangerous-commands.sh backend,
 * which delegates detection to ast-grep + tree-sitter-bash. Because the backend
 * parses the command as bash, flags belonging to other commands, contents inside
 * strings, and shell comments do not trigger false positives.
 *
 * Matches the Claude hook's strict hard-block behaviour: no confirmation prompt,
 * the tool call is blocked and terminates the turn.
 */

import { spawnSync } from "child_process";
import { isToolCallEventType } from "@earendil-works/pi-coding-agent";
import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";

// Store path to the packaged check-dangerous-commands.sh (injected via pkgs.mutate).
const CHECK_SCRIPT = "@checkScript@";

// exit code 2 == the backend found a dangerous rm and printed an error.
const BACKEND_BLOCKED = 2;

function runBackend(command: string): number {
  const input = JSON.stringify({
    tool_name: "Bash",
    tool_input: { command },
  });
  const res = spawnSync(CHECK_SCRIPT, [], {
    input,
    encoding: "utf8",
    timeout: 5000,
  });
  return res.status ?? -1;
}

export default function (pi: ExtensionAPI) {
  pi.on("tool_call", async (event, ctx) => {
    if (event.toolName !== "bash") return;
    if (!isToolCallEventType("bash", event)) return;

    const status = runBackend(event.input.command);
    if (status === BACKEND_BLOCKED) {
      return {
        block: true,
        reason:
          "Blocked: 'rm' with recursive+force flags ('-rf') and no interactive confirmation. " +
          "This is forbidden. Prefer 'rm -r' for recursion, or ask the user to run a forced delete.",
        terminate: true,
      };
    }
  });
}