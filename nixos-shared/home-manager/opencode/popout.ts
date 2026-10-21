// TUI plugin, modelled on marginal's. The route prompt shape comes from the
// opencode2 2.0.32 bundle; there is no published type for it.
//
// The home route with a prompt is the opencode equivalent of Claude Code's
// /clear + $.prompt.fill: a blank session that only exists once the prompt is
// submitted, so a popout you abandon leaves nothing behind.
import type { Context } from "@opencode/plugin/tui"

type Message =
  | { type: "assistant"; content: readonly { type: string; text?: string }[] }
  | { type: string }

export default {
  id: "popout",
  setup(context: Context) {
    const toast = (message: string, variant: "info" | "warning" | "error") =>
      context.ui.toast.show({ message, variant })

    async function popout(input?: string) {
      const route = context.ui.router.current()
      if (route.type !== "session") return toast("Open a session first.", "warning")
      const { sessionID } = route

      await context.data.session.message.sync(sessionID)
      const messages = context.data.session.message.list(sessionID) as readonly Message[]
      const text = [...messages].reverse().map(textOf).find((t) => t !== "")
      if (!text) return toast("No assistant reply with text to carry over.", "warning")

      // The model reads the fill as typed by the user; the framing says whose words they are.
      const args = input?.trim()
      context.ui.router.navigate({
        type: "home",
        prompt: {
          text:
            `[popout] Your own last reply from the previous session, carried over as the only context.\n` +
            `Previous session: ${sessionID}\n\n${text}\n\n---\n${args ? args + "\n" : ""}`,
          files: [],
          agents: [],
          skills: [],
          // Unlike the other three, read without a `?? []`: leaving it out crashes the TUI.
          pasted: [],
        },
      })
    }

    // "global", or the layer is inactive while the prompt has focus (see marginal).
    context.keymap.layer(() => ({
      mode: "global",
      commands: [
        {
          id: "popout.run",
          title: "Pop out",
          description: "New session, prompt prefilled with the last reply: /popout [text]",
          group: "Session",
          bind: "<leader>p",
          palette: true,
          slash: { name: "popout", arguments: true },
          run: popout,
        },
      ],
    }))
  },
}

function textOf(message: Message): string {
  if (message.type !== "assistant" || !("content" in message)) return ""
  return message.content
    .filter((part) => part.type === "text" && typeof part.text === "string")
    .map((part) => part.text)
    .join("\n\n")
    .trim()
}
