import { atom, read, update } from 'claude-code'
import type { Register } from 'claude-code'

import type { Sample } from '../types'

const PANE = 'tps-meter'
// Tool-call-only responses emit a handful of tokens in one burst; their rate is noise.
const MIN_TOKENS = 20
// Rough English/code average; the visible rate is an estimate, the API counts no subtotal.
const CHARS_PER_TOKEN = 4
// Text that lands in one burst has no rate worth showing.
const MIN_STREAM_MS = 250

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
    // Throughput's bad tail is the slow end, latency's the long end.
    p10: percentile(rates, 10),
    ttftP50: percentile(ttfts, 50),
    ttftP90: percentile(ttfts, 90),
  }
}

const SPARK = '▁▂▃▄▅▆▇█'
const DEFAULT_BG = 0x01000000

// Scaled min..max: from zero, one outlier flattens the rest to the bottom bar.
function scaled(values: number[]): number[] {
  const min = Math.min(...values)
  const span = Math.max(...values) - min || 1
  return values.map(v => (v - min) / span)
}

const bar = (t: number) => SPARK[Math.round(t * 7)] ?? '▁'

function sparkline(values: number[]): string {
  return scaled(values).map(bar).join('')
}

// Slow blue, middle grey, fast orange within the window shown: no traffic-light
// verdict (relative is not bad) and readable with red-green colour blindness.
const STOPS = [0x4c78dd, 0xb0b0b0, 0xf0883e]

export function speedColor(t: number): number {
  const at = Math.min(Math.max(t, 0), 1) * (STOPS.length - 1)
  const i = Math.min(Math.floor(at), STOPS.length - 2)
  const from = STOPS[i] ?? 0
  const to = STOPS[i + 1] ?? 0
  const channel = (shift: number) => {
    const a = (from >> shift) & 0xff
    return Math.round(a + (((to >> shift) & 0xff) - a) * (at - i)) << shift
  }
  return channel(16) | channel(8) | channel(0)
}

export function sparkCells(values: number[]): string {
  const words = new Uint32Array(values.length * 3)
  scaled(values).forEach((t, i) => {
    words.set([bar(t).codePointAt(0) ?? 0x2581, speedColor(t), DEFAULT_BG], i * 3)
  })
  return new Uint8Array(words.buffer).toBase64()
}

const fmt = (n: number) => n.toFixed(0)
const plural = (n: number, word: string) => `${n} ${word}${n === 1 ? '' : 's'}`
const secs = (ms: number) => `${(ms / 1000).toFixed(1)}s`

export const register: Register = on => {
  on('session.start', async ($, e, next) => {
    await $.command.register({
      name: 'tps',
      description: 'Toggle the tokens-per-second pane; /tps reset clears the samples',
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
    if ((await $.ui.panes()).some(pane => pane.id === PANE)) {
      await $.ui.close({ id: PANE })
      return { text: 'pane closed.' }
    }
    await $.ui.open({ id: PANE, title: 'Tokens / s' })
    const s = summarize(await read($, samples))
    return {
      text: `${plural(s.requests, 'request')}, ${s.totalOut} output tokens; ${fmt(s.weightedTps)} tok/s weighted, p10 ${fmt(s.p10)} tok/s, TTFT p50 ${secs(s.ttftP50)} p90 ${secs(s.ttftP90)}`,
    }
  })

  on('turn.step', async function* ($, e, next) {
    const sentAt = performance.now()
    let firstAt: number | undefined
    let stopAt: number | undefined
    let textFirstAt: number | undefined
    let textLastAt: number | undefined
    let visibleChars = 0

    const stream = next(e)
    for await (const chunk of stream) {
      if (chunk.kind !== 'engine') {
        firstAt ??= performance.now()
        // Not tool arguments: measured 2026-10-07, they arrive in bursts (593 tok/s) and
        // undercount at chars/4.
        if (chunk.kind === 'text' || chunk.kind === 'thinking') {
          textFirstAt ??= performance.now()
          textLastAt = performance.now()
          visibleChars += chunk.text.length
        }
        if (chunk.kind === 'stop') stopAt = performance.now()
      }
      yield chunk
    }
    stopAt ??= performance.now()

    const result = await stream.result
    const usage = result.usage
    if (!usage || firstAt === undefined) return result

    // Measured 2026-10-07: hidden thinking reaches the hook as no chunk at all, envelope
    // included, so any window opening at a chunk counts its tokens in near-zero time
    // (9778 tok/s). From the send, the rate includes prefill and can only understate.
    const genMs = stopAt - sentAt
    const streamMs = textFirstAt !== undefined && textLastAt !== undefined ? textLastAt - textFirstAt : 0
    const visibleTokens = visibleChars / CHARS_PER_TOKEN
    const sample: Sample = {
      model: usage.model,
      isSubagent: e.agentId !== undefined,
      outputTokens: usage.output_tokens,
      ttftMs: firstAt - sentAt,
      genMs,
      tps: genMs > 0 ? usage.output_tokens / (genMs / 1000) : 0,
      visibleTps: visibleTokens >= MIN_TOKENS && streamMs >= MIN_STREAM_MS ? visibleTokens / (streamMs / 1000) : null,
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
    const visible = last.visibleTps === null ? '' : ` · streamed ~${fmt(last.visibleTps)}`
    const stats = `⚡ ${fmt(s.weightedTps)} tok/s · last ${lastRate}${visible} · p10 ${fmt(s.p10)} · TTFT ${secs(last.ttftMs)} (p50 ${secs(s.ttftP50)} p90 ${secs(s.ttftP90)})`
    const room = (e.props.bodyColumns ?? 80) - stats.length - 2
    const rates = list.filter(x => x.outputTokens >= MIN_TOKENS).map(x => x.tps)
    const shown = room >= 4 && rates.length > 1 ? rates.slice(-Math.min(room, 40)) : []

    // Raster, the per-cell colour, is the terminal's alone.
    if (e.surface === 'terminal' && shown.length > 0) {
      const { Raster } = $.ui.resolve(e)
      return (
        <Box>
          <Text dimColor>{stats}  </Text>
          <Raster key="band-spark" columns={shown.length} rows={1} cells={sparkCells(shown)} />
        </Box>
      )
    }
    return (
      <Box>
        <Text dimColor>{stats}</Text>
        {shown.length > 0 && <Text color="cyan">  {sparkline(shown)}</Text>}
      </Box>
    )
  })

  on('ui.render', { component: 'Pane', requestId: PANE }, async ($, e) => {
    const { Box, Text } = $.ui.resolve(e)
    const list = await read($, samples)
    const s = summarize(list)
    const counted = list.filter(x => x.outputTokens >= MIN_TOKENS)
    const width = Math.max(10, (e.props.bodyColumns ?? 60) - 2)
    const spark = (values: number[]) => {
      if (e.surface !== 'terminal') return <Text color="cyan">{sparkline(values)}</Text>
      const { Raster } = $.ui.resolve(e)
      return <Raster key="pane-spark" columns={values.length} rows={1} cells={sparkCells(values)} />
    }

    const byModel = new Map<string, Sample[]>()
    for (const x of list) byModel.set(x.model, [...(byModel.get(x.model) ?? []), x])

    return (
      <Box flexDirection="column">
        {list.length === 0 && <Text dimColor>No model requests measured yet.</Text>}
        {list.length > 0 && (
          <Box flexDirection="column">
            <Text bold>{fmt(s.weightedTps)} tok/s (token-weighted)</Text>
            <Text>p10 {fmt(s.p10)} tok/s · TTFT p50 {secs(s.ttftP50)} p90 {secs(s.ttftP90)}</Text>
            <Text dimColor>
              {plural(s.requests, 'request')} ({s.counted} ≥{MIN_TOKENS} tok) · {s.totalOut} output tokens
            </Text>
            {counted.length > 1 && spark(counted.slice(-width).map(x => x.tps))}
            {[...byModel].map(([model, xs]) => {
              const m = summarize(xs)
              return (
                <Text>
                  {model}: {fmt(m.weightedTps)} tok/s · {plural(m.requests, 'req')}
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
