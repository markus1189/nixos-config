import { spawn } from "child_process"

const SOUNDS_DIR = "@sounds@"

// Detached with stdio ignored, so playback never blocks the agent and never
// writes into the TUI.
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

// Tool ids as OpenCode 2.0.24 lists them through `ctx.tool.list()`, minus
// the opencode_* session/MCP helpers: patch, edit, write, glob, grep, read,
// shell, skill, subagent, webfetch, websearch, question.
const RESEARCH_TOOLS = new Set(["subagent", "websearch"])
const READONLY_TOOLS = new Set(["read", "glob", "grep", "webfetch"])
const MUTATING_TOOLS = new Set(["shell", "write", "edit", "patch"])

// No default import of @opencode/plugin: Plugin.define is a runtime value,
// and nothing installs that package next to this Nix-managed file. OpenCode
// reads only `id` and `setup` from the default export.
export default {
  id: "sounds",
  async setup(ctx: any) {
    // Subagents run in child sessions; they get the subagent chime below, not
    // the session-level sounds.
    const children = new Set<string>()

    const controller = new AbortController()
    void (async () => {
      for await (const event of ctx.event.subscribe({ signal: controller.signal })) {
        const id = event.data?.sessionID
        switch (event.type) {
          case "session.created":
            if (event.data.parentID) children.add(id)
            else playSound("involved-notification.wav")
            break
          case "session.compaction.ended":
            // hollow-582 is compaction across the fleet; pull-out-551 means
            // "cleared / switched" in claude-code and pi-agent, so don't reuse it.
            if (!children.has(id)) playSound("hollow-582.wav")
            break
          case "permission.asked":
            // Blocked on the user, like claude-code's permission_prompt.
            playSound("your-turn-491.wav")
            break
          // V2's idle: one execution of queued input has finished.
          case "session.execution.succeeded":
          case "session.deleted":
            if (!children.has(id)) playSound("for-sure-576.wav")
            break
        }
      }
    })()

    await ctx.tool.hook("execute.before", (event: any) => {
      if (event.tool === "skill") playSound("graceful-285.wav")
      else if (RESEARCH_TOOLS.has(event.tool)) playSound("happy-to-help-notification-sound.wav")
      else if (READONLY_TOOLS.has(event.tool)) playSound("come-here-notification.wav")
      else if (MUTATING_TOOLS.has(event.tool)) playSound("intuition-561.wav")
    })

    // Subagent completion only. Neither sibling chimes after every tool:
    // claude-code registers no PostToolUse hook, and pi-agent's tool_result
    // handler fires only when the call errored.
    await ctx.tool.hook("execute.after", (event: any) => {
      if (event.tool === "subagent") playSound("time-is-now-585.wav")
    })

    return () => controller.abort()
  },
}
