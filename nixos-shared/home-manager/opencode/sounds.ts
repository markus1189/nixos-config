import type { Plugin } from "@opencode-ai/plugin"

export const Sounds: Plugin = async ({ $ }) => {
  // Helper: Play sound file in background
  const playSound = async (soundPath: string) => {
    await $`@aplay@/bin/aplay ${soundPath} >/dev/null 2>&1 &`.nothrow()
  }

  // Helper: Match tool name against pattern (regex)
  const matchesTool = (toolName: string, pattern: string): boolean => {
    const regex = new RegExp(pattern)
    return regex.test(toolName)
  }

  return {
    event: async ({ event }) => {
      // Session events
      if (event.type === "session.created") {
        // Startup/resume sound
        await playSound("@involvedNotificationSound@")
      } else if (event.type === "session.compacted") {
        // Session cleared sound
        await playSound("@pullOutSound@")
      } else if (event.type === "session.idle" || event.type === "session.deleted") {
        // Session stopped/completed sound
        await playSound("@forSureSound@")
      }

      // Tool execution events - before
      else if (event.type === "tool.execute.before") {
        const toolName = event.data?.tool?.name || ""

        if (matchesTool(toolName, "Task|WebSearch")) {
          // Research/task tools
          await playSound("@happyToHelpSound@")
        } else if (matchesTool(toolName, "Read|List|Glob|Grep|WebFetch")) {
          // Read-only tools
          await playSound("@comeHereSound@")
        } else if (matchesTool(toolName, "Bash|Write|Edit|MultiEdit|TodoWrite")) {
          // State-changing tools
          await playSound("@intuitionSound@")
        }
      }

      // Tool execution events - after
      else if (event.type === "tool.execute.after") {
        const toolName = event.data?.tool?.name || ""

        if (toolName === "Task") {
          // Subagent completion
          await playSound("@timeIsNowSound@")
        } else {
          // General notification for other tool completions
          await playSound("@justMaybeSound@")
        }
      }
    }
  }
}
