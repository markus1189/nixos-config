// Desktop notifications come from cli.json's `attention.notifications`; this
// only adds the BEL, which tmux turns into a window alert. The bell means
// "this window needs you", as in claude-code and pi: a finished root turn,
// a question, a permission prompt.
export default {
  id: "terminal-bell",
  setup(context: any) {
    const bell = () => process.stdout.write("\x07")
    const offs = [
      // Subagents run in child sessions; ring only when the root one finishes.
      context.data.on("session.execution.succeeded", (event: any) => {
        const id = event.data.sessionID
        if (context.data.session.root(id) === id) bell()
      }),
      context.data.on("form.created", bell),
      context.data.on("permission.asked", bell),
    ]
    return () => offs.forEach((off: any) => off?.())
  },
}
