export type Sample = {
  model: string
  isSubagent: boolean
  outputTokens: number
  ttftMs: number
  genMs: number
  tps: number
  visibleTps: number | null
}

declare module 'claude-code' {
  interface PluginState {
    'tps-meter': { samples: Sample[] }
  }
}
