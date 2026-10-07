import { atom, read, update } from 'claude-code'
import type { Register } from 'claude-code'

import type { Sample } from '../types'

const PANE = 'tps-meter'
// Tool-call-only responses emit a handful of tokens in one burst; their rate is noise.
const MIN_TOKENS = 20

const samples = atom({ plugin: 'tps-meter', key: 'samples' } as const, [])

export function percentile(sorted: number[], p: number): number {
  if (sorted.length === 0) return 0
  return sorted[Math.min(sorted.length - 1, Math.floor((p / 100) * sorted.length))] ?? 0
}

export function summarize(list: Sample[]) {
  const counted = list.filter(s => s.outputTokens >= MIN_TOKENS && s.genMs > 0)
  const tokens = counted.reduce((n, s) => n + s.outputTokens, 0)
  const genMs = counted.reduce((n, s) => n + s.genMs, 0)
  const rates = counted.map(s => s.tps).sort((a, b) => a - b)
  const ttfts = list.map(s => s.ttftMs).sort((a, b) => a - b)
  return {
    requests: list.length,
    counted: counted.length,
    totalOut: list.reduce((n, s) => n + s.outputTokens, 0),
    // Token-weighted: long responses dominate, as they do the waiting.
    weightedTps: genMs > 0 ? tokens / (genMs / 1000) : 0,
    p50: percentile(rates, 50),
    p90: percentile(rates, 90),
    ttftP50: percentile(ttfts, 50),
  }
}

const SPARK = '▁▂▃▄▅▆▇█'
// Scaled min..max: from zero, one outlier flattens the rest to the bottom bar.
function sparkline(values: number[]): string {
  const min = Math.min(...values)
  const span = Math.max(...values) - min || 1
  return values.map(v => SPARK[Math.round(((v - min) / span) * 7)]).join('')
}

const fmt = (n: number) => n.toFixed(0)
const secs = (ms: number) => `${(ms / 1000).toFixed(1)}s`

export const register: Register = on => {
  on('session.start', async ($, e, next) => {
    await $.command.register({
      name: 'tps',
      description: 'Show tokens-per-second metrics for this session; /tps reset clears them',
    })
    // An earlier version pinned a status notice; it outlives reloads until cleared.
    $.ui.status(undefined)
    return next(e)
  })

  on('command.run', { command: 'tps' }, async ($, e) => {
    if (e.args.trim() === 'reset') {
      await update($, samples, () => [])
      return { text: 'samples cleared.' }
    }
    await $.ui.open({ id: PANE, title: 'Tokens / s' })
    const s = summarize(await read($, samples))
    return {
      text: `${s.requests} requests, ${s.totalOut} output tokens; ${fmt(s.weightedTps)} tok/s weighted, p50 ${fmt(s.p50)}, p90 ${fmt(s.p90)}, TTFT p50 ${secs(s.ttftP50)}`,
    }
  })

  on('turn.step', async function* ($, e, next) {
    const sentAt = performance.now()
    // The response envelope arrives before any thinking; hidden thinking emits no
    // chunks, so timing from the first visible chunk would count its tokens in no time.
    let startAt: number | undefined
    let firstAt: number | undefined
    let stopAt: number | undefined

    const stream = next(e)
    for await (const chunk of stream) {
      startAt ??= performance.now()
      if (chunk.kind !== 'engine') {
        firstAt ??= performance.now()
        if (chunk.kind === 'stop') stopAt = performance.now()
      }
      yield chunk
    }
    stopAt ??= performance.now()

    const result = await stream.result
    const usage = result.usage
    if (!usage || startAt === undefined || firstAt === undefined) return result

    const genMs = stopAt - startAt
    const sample: Sample = {
      model: usage.model,
      isSubagent: e.agentId !== undefined,
      outputTokens: usage.output_tokens,
      ttftMs: firstAt - sentAt,
      genMs,
      tps: genMs > 0 ? usage.output_tokens / (genMs / 1000) : 0,
    }
    await update($, samples, list => [...list, sample].slice(-500))
    return result
  })

  // Not $.ui.status: pinned notices are drawn with a fixed ⚠ prefix in the warning colour.
  on('ui.render', { component: 'AbovePrompt' }, async ($, e, next) => {
    const list = await read($, samples)
    const last = list.at(-1)
    if (e.props.hasSurvey || last === undefined) return next(e)

    const { Box, Text } = $.ui.resolve(e)
    const s = summarize(list)
    const lastRate = last.outputTokens >= MIN_TOKENS ? fmt(last.tps) : '–'
    const stats = `⚡ ${fmt(s.weightedTps)} tok/s · last ${lastRate} · p50 ${fmt(s.p50)} p90 ${fmt(s.p90)} · TTFT ${secs(last.ttftMs)}`
    const room = (e.props.bodyColumns ?? 80) - stats.length - 2
    const rates = list.filter(x => x.outputTokens >= MIN_TOKENS).map(x => x.tps)

    return (
      <Box>
        <Text dimColor>{stats}</Text>
        {room >= 4 && rates.length > 1 && <Text color="cyan">  {sparkline(rates.slice(-Math.min(room, 40)))}</Text>}
      </Box>
    )
  })

  on('ui.render', { component: 'Pane', requestId: PANE }, async ($, e) => {
    const { Box, Text } = $.ui.resolve(e)
    const list = await read($, samples)
    const s = summarize(list)
    const counted = list.filter(x => x.outputTokens >= MIN_TOKENS)
    const width = Math.max(10, (e.props.bodyColumns ?? 60) - 2)

    const byModel = new Map<string, Sample[]>()
    for (const x of list) byModel.set(x.model, [...(byModel.get(x.model) ?? []), x])

    return (
      <Box flexDirection="column">
        {list.length === 0 && <Text dimColor>No model requests measured yet.</Text>}
        {list.length > 0 && (
          <Box flexDirection="column">
            <Text bold>{fmt(s.weightedTps)} tok/s (token-weighted)</Text>
            <Text>p50 {fmt(s.p50)} · p90 {fmt(s.p90)} · TTFT p50 {secs(s.ttftP50)}</Text>
            <Text dimColor>
              {s.requests} requests ({s.counted} ≥{MIN_TOKENS} tok) · {s.totalOut} output tokens
            </Text>
            <Text color="cyan">{sparkline(counted.slice(-width).map(x => x.tps))}</Text>
            <Text> </Text>
            {[...byModel].map(([model, xs]) => {
              const m = summarize(xs)
              return (
                <Text>
                  {model}: {fmt(m.weightedTps)} tok/s · {m.requests} req
                  {xs.some(x => x.isSubagent) ? ` (${xs.filter(x => x.isSubagent).length} subagent)` : ''}
                </Text>
              )
            })}
          </Box>
        )}
      </Box>
    )
  })
}
