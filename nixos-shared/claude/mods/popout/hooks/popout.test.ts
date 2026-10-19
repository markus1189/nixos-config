import { expect, mock, test } from 'claude-code/testing'

test('popout clears and fills the last reply', async ($, on) => {
  const clock = mock.clock(on)
  const ran: string[] = []
  const filled: string[] = []
  on('session.messages', () => ({ value: [
    { role: 'assistant', text: 'old', toolUses: [] },
    { role: 'user', text: 'q', toolUses: [] },
    { role: 'assistant', text: 'the answer', toolUses: [] },
  ] }))
  on('session.id', () => ({ value: 'abc-123' }))
  on('session.root', () => ({ value: '/home/u/repo.x' }))
  on('command.run', ($, e) => { ran.push(e.command); return {} })
  on('prompt.fill', ($, e) => { filled.push(e.text); return { isFilled: true } })

  await $.command.run({ command: 'popout', args: 'now do X' })
  await clock.settle()

  expect(ran).toEqual(['clear'])
  expect(filled.length).toBe(1)
  expect(filled[0]).toContain('the answer')
  expect(filled[0]).not.toContain('old')
  expect(filled[0]).toContain('now do X')
  expect(filled[0]).toContain('Previous session: abc-123 (transcript ~/.claude/projects/-home-u-repo-x/abc-123.jsonl')
})
