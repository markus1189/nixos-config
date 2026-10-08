// Desktop notifications come from cli.json's `attention.notifications`; this
// only adds the BEL, which tmux turns into a window alert.
export default {
  id: "terminal-bell",
  setup(context: any) {
    // Subagents run in child sessions; ring only when the root one finishes.
    return context.data.on("session.execution.succeeded", (event: any) => {
      const id = event.data.sessionID
      if (context.data.session.root(id) === id) process.stdout.write("\x07")
    })
  },
}
