import type { Plugin } from "@opencode-ai/plugin"
import { spawn } from "child_process"

const SOUNDS_DIR = "@sounds@"

// Detached with stdio ignored, so playback never blocks the agent and never
// writes into the TUI. Bun's `$` is unusable here: it rejects background `&`
// outright ("Background commands \"&\" are not supported yet") and accepts
// only one redirect per command, so `>/dev/null 2>&1 &` fails to parse.
//
// The timeout matches claude-code's playSound and is load-bearing for the
// same reason: if the audio stack wedges, aplay blocks forever on the
// PipeWire socket and every tool call leaks an immortal process.
function playSound(name: string) {
  spawn("@coreutils@/bin/timeout", ["5", "@aplay@/bin/aplay", `${SOUNDS_DIR}/${name}`], {
    detached: true,
    stdio: "ignore",
  }).unref()
}

// Tool ids are lowercase and come from `GET /experimental/tool/ids`:
// invalid, question, bash, read, glob, grep, edit, write, task, webfetch,
// todowrite, websearch, skill, apply_patch. There is no `list` tool, and
// `patch` is spelled `apply_patch`.
const RESEARCH_TOOLS = new Set(["task", "websearch"])
const READONLY_TOOLS = new Set(["read", "glob", "grep", "webfetch"])
const MUTATING_TOOLS = new Set(["bash", "write", "edit", "apply_patch", "todowrite"])

export const Sounds: Plugin = async () => {
  return {
    event: async ({ event }) => {
      switch (event.type) {
        case "session.created":
          playSound("involved-notification.wav")
          break
        case "session.compacted":
          // hollow-582 is compaction across the fleet; pull-out-551 means
          // "cleared / switched" in claude-code and pi-agent, so don't reuse it.
          playSound("hollow-582.wav")
          break
        case "session.idle":
        case "session.deleted":
          playSound("for-sure-576.wav")
          break
      }
    },

    // tool.execute.* are top-level hooks, not bus events -- they never arrive
    // through `event`, and the tool id is `input.tool`, a bare string.
    "tool.execute.before": async (input) => {
      if (input.tool === "skill") playSound("graceful-285.wav")
      else if (RESEARCH_TOOLS.has(input.tool)) playSound("happy-to-help-notification-sound.wav")
      else if (READONLY_TOOLS.has(input.tool)) playSound("come-here-notification.wav")
      else if (MUTATING_TOOLS.has(input.tool)) playSound("intuition-561.wav")
    },

    // Subagent completion only. Neither sibling chimes after every tool:
    // claude-code registers no PostToolUse hook, and pi-agent's tool_result
    // handler fires only when the call errored.
    "tool.execute.after": async (input) => {
      if (input.tool === "task") playSound("time-is-now-585.wav")
    },
  }
}
