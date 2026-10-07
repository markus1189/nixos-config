import { expect, test } from 'claude-code/testing'

import { summarize } from './register'

const usage = { model: 'claude-test', input_tokens: 10, output_tokens: 200, cache_read_input_tokens: 0, cache_creation_input_tokens: 0 }

test('records a sample per model request and passes the stream through', async ($, on) => {
  on('turn.step', async function* (_$, e) {
    yield { kind: 'text', index: 0, text: 'hello ' }
    yield { kind: 'text', index: 0, text: 'world' }
    yield { kind: 'stop', stopReason: 'end_turn', usage }
    return { turnId: e.turnId, index: e.index, answer: 'hello world', toolUses: [], stopReason: 'end_turn', usage }
  })

  const stream = $.turn.step({ turnId: 't1', index: 0, model: 'claude-test', messageCount: 1 })
  const texts: string[] = []
  let step = await stream.next()
  while (!step.done) {
    if (step.value.kind === 'text') texts.push(step.value.text)
    step = await stream.next()
  }
  const result = step.value

  expect(texts.join('')).toBe('hello world')
  expect(result.usage?.output_tokens).toBe(200)

  for (const surface of ['terminal', 'desktop'] as const) {
    const ui = await $.ui.mount({ plugin: 'tps-meter', surface, component: 'AbovePrompt', props: { hasSurvey: false, isWorking: false, maxRows: 10, bodyColumns: 120, scroll: { offset: 0, bodyRows: 10 }, view: {} } })
    expect((await ui.find({ type: 'Text', text: /tok\/s/ }))).toBeTruthy()
  }
})

test('summary weights by tokens and ignores tiny tool-call responses', () => {
  const s = summarize([
    { model: 'm', isSubagent: false, outputTokens: 100, ttftMs: 500, genMs: 1000, tps: 100 },
    { model: 'm', isSubagent: false, outputTokens: 300, ttftMs: 700, genMs: 1000, tps: 300 },
    { model: 'm', isSubagent: false, outputTokens: 5, ttftMs: 900, genMs: 1, tps: 5000 },
  ])
  expect(s.weightedTps).toBe(200)
  expect(s.counted).toBe(2)
  expect(s.requests).toBe(3)
})

test('/tps reset clears the samples', async ($, on) => {
  // Stands in for the engine's own band, drawn when the plugin passes.
  on('ui.render', ($, e) => {
    const { Text } = $.ui.resolve(e)
    return <Text>engine band</Text>
  })
  on('turn.step', async function* (_$, e) {
    yield { kind: 'text', index: 0, text: 'x' }
    yield { kind: 'stop', stopReason: 'end_turn', usage }
    return { turnId: e.turnId, index: e.index, answer: 'x', toolUses: [], stopReason: 'end_turn', usage }
  })
  const stream = $.turn.step({ turnId: 't1', index: 0, model: 'claude-test', messageCount: 1 })
  while (!(await stream.next()).done) {}

  const props = { hasSurvey: false, isWorking: false, maxRows: 10, bodyColumns: 120, scroll: { offset: 0, bodyRows: 10 }, view: {} }
  const before = await $.ui.mount({ plugin: 'tps-meter', surface: 'terminal', component: 'AbovePrompt', props })
  expect(await before.find({ type: 'Text', text: /tok\/s/ })).toBeTruthy()

  const ran = await $.command.run({ command: 'tps', args: 'reset', origin: 'user' } as never)
  expect(ran.text).toMatch(/cleared/)
  const after = await $.ui.mount({ plugin: 'tps-meter', surface: 'terminal', component: 'AbovePrompt', props })
  expect(await after.find({ type: 'Text', text: /tok\/s/ })).toBeFalsy()
})
