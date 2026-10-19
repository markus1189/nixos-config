import type { Register } from 'claude-code'

export const register: Register = on => {
  on('session.start', async ($, e, next) => {
    await $.command.register({
      name: 'popout',
      description: 'New session (/clear), prompt prefilled with the last reply',
    })
    return next(e)
  })

  on('command.run', { command: 'popout' }, async ($, e) => {
    const last = (await $.session.messages())
      .findLast(m => m.role === 'assistant' && m.text.trim())
    if (!last) return { text: 'No assistant reply with text to carry over.' }

    // The model reads the fill as typed by the user; the framing says whose words they are.
    const args = e.args.trim()
    const id = await $.session.id()
    // Transcript dirs are the session root with every non-alphanumeric turned into '-'.
    const dir = (await $.session.root()).replace(/[^a-zA-Z0-9]/g, '-')
    const text =
      `[popout] Your own last reply from the previous session, carried over as the only context.\n` +
      `Previous session: ${id} (transcript ~/.claude/projects/${dir}/${id}.jsonl, resume: claude --resume ${id})\n\n` +
      `${last.text.trim()}\n\n---\n${args ? args + '\n' : ''}`

    // $.command.run is refused inside a command.run hook (it would wait on this one).
    // Fill only after the clear resolves, or the reset prompt box swallows it.
    $.clock.after(0, () =>
      $.command.run({ command: 'clear' })
        .then(() => $.prompt.fill({ text }))
        .catch(err => $.ui.toast(`popout failed: ${err}`)),
    )
    return { text: 'Popping out…' }
  })
}
