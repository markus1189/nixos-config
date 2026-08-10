/**
 * tokens-per-second — pi extension
 *
 * Live output-token throughput in pi's built-in footer via ctx.ui.setStatus(),
 * plus session-wide stats: token-weighted average and percentiles over all
 * completed assistant messages.
 *
 * Footer while streaming:  "41.7 tok/s · avg 38.2 · p90 46.1"
 * Footer between messages: "last 39.4 · avg 38.2 · p90 46.1"
 * Command /tps: full stats (n, min, p50, p90, p99, max, avg, totals)
 *
 * Install: copy to ~/.pi/agent/extensions/tokens-per-second.ts
 *          (or .pi/extensions/ for project-local), then /reload.
 */

import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";

const STATUS_KEY = "tok/s";
const WINDOW_MS = 3000; // sliding window for the live rate
const RENDER_INTERVAL_MS = 250; // throttle footer updates
const CHARS_PER_TOKEN = 4; // fallback estimate when usage isn't streamed

interface Sample {
	t: number;
	tokens: number;
}

interface MessageStat {
	rate: number; // tok/s for this message
	tokens: number; // output tokens
	seconds: number; // generation time (first delta → end, excludes TTFT)
}

export default function (pi: ExtensionAPI) {
	// Per-message streaming state
	let startTime = 0;
	let firstDeltaTime = 0;
	let estChars = 0;
	let usageSeen = false;
	let samples: Sample[] = [];
	let lastRender = 0;
	let lastRate: number | null = null; // rate of the most recent completed message (idle footer)

	// Session-wide stats (since extension load / session start)
	let stats: MessageStat[] = [];

	const slidingRate = (now: number): number => {
		samples = samples.filter((s) => now - s.t <= WINDOW_MS);
		if (samples.length < 2) return 0;
		const first = samples[0];
		const last = samples[samples.length - 1];
		const dt = (last.t - first.t) / 1000;
		return dt > 0 ? (last.tokens - first.tokens) / dt : 0;
	};

	/** Token-weighted session average: total tokens / total time. */
	const sessionAvg = (): number => {
		const tokens = stats.reduce((a, s) => a + s.tokens, 0);
		const seconds = stats.reduce((a, s) => a + s.seconds, 0);
		return seconds > 0 ? tokens / seconds : 0;
	};

	/** Nearest-rank percentile over per-message rates. p in [0, 100]. */
	const percentile = (p: number): number => {
		if (stats.length === 0) return 0;
		const sorted = stats.map((s) => s.rate).sort((a, b) => a - b);
		const rank = Math.ceil((p / 100) * sorted.length);
		return sorted[Math.min(sorted.length - 1, Math.max(0, rank - 1))];
	};

	const fmt = (n: number): string => n.toFixed(1);

	// Fixed-width cell so the decimal point stays in a stable column and the
	// following text never shifts left/right as digits change width.
	const CELL = 5; // e.g. " 41.7"
	const cell = (n: number): string => fmt(n).padStart(CELL, " ");
	const dash = " ".repeat(CELL - 1) + "—"; // "    —" placeholder, same width as a number

	// avg/p90 columns are ALWAYS present; dashes stand in until real data
	// arrives. This keeps the layout constant — columns never pop in/out.
	const sessionSuffix = (): string =>
		` · avg ${stats.length ? cell(sessionAvg()) : dash}` +
		` · p90 ${stats.length ? cell(percentile(90)) : dash}`;

	// Indicators are always exactly IND_W chars, so swapping the "live"/"last"
	// variants in place never shifts the avg/p90 columns either.
	const IND_W = 11;
	const liveInd = (rate: number): string => `${cell(rate)} tok/s`; // 5 + 6 = 11
	const liveIndDash = `${dash} tok/s`; // placeholder while streaming, no rate yet
	const lastInd = (rate: number): string => `last ${cell(rate)}`.padEnd(IND_W);
	const lastIndDash = `last ${dash}`.padEnd(IND_W);

	pi.on("session_start", async (_event, ctx) => {
		stats = [];
		startTime = 0;
		firstDeltaTime = 0;
		estChars = 0;
		usageSeen = false;
		samples = [];
		lastRender = 0;
		lastRate = null;
		if (ctx.hasUI) ctx.ui.setStatus(STATUS_KEY, undefined);
	});

	pi.on("message_start", async (event, ctx) => {
		if (event.message.role !== "assistant") return;
		startTime = Date.now();
		firstDeltaTime = 0;
		estChars = 0;
		usageSeen = false;
		samples = [];
		lastRender = 0;
		if (ctx.hasUI)
			ctx.ui.setStatus(STATUS_KEY, ctx.ui.theme.fg("muted", `· ${liveIndDash}${sessionSuffix()}`));
	});

	pi.on("message_update", async (event, ctx) => {
		if (event.message.role !== "assistant") return;

		const now = Date.now();
		if (firstDeltaTime === 0) firstDeltaTime = now;

		if (!ctx.hasUI) return;

		const ev = event.assistantMessageEvent;
		if (
			ev.type === "text_delta" ||
			ev.type === "thinking_delta" ||
			ev.type === "toolcall_delta"
		) {
			estChars += ev.delta.length;
		}

		// Estimate and real usage are different units — never mix them in one
		// window. Latch on first real usage and discard the estimate samples.
		const out = event.message.usage.output;
		if (out > 0 && !usageSeen) {
			usageSeen = true;
			samples = [];
		}
		samples.push({
			t: now,
			tokens: usageSeen ? out : Math.round(estChars / CHARS_PER_TOKEN),
		});

		if (now - lastRender < RENDER_INTERVAL_MS) return;
		lastRender = now;

		const rate = slidingRate(now);
		const ind = rate > 0 ? liveInd(rate) : liveIndDash;
		ctx.ui.setStatus(STATUS_KEY, ctx.ui.theme.fg("muted", `· ${ind}${sessionSuffix()}`));
	});

	pi.on("message_end", async (event, ctx) => {
		if (event.message.role !== "assistant") return;

		// Rate = pure generation time when we saw a delta; startTime is the
		// fallback. Zero means no matching message_start (e.g. loaded
		// mid-stream) — record nothing rather than a garbage stat.
		const genStart = firstDeltaTime || startTime;
		startTime = 0;
		firstDeltaTime = 0;

		let stat: MessageStat | undefined;
		const seconds = genStart > 0 ? (Date.now() - genStart) / 1000 : 0;
		const tokens = event.message.usage.output;
		const { stopReason } = event.message;
		if (
			seconds > 0 &&
			tokens > 0 &&
			stopReason !== "aborted" &&
			stopReason !== "error"
		) {
			stat = { rate: tokens / seconds, tokens, seconds };
			stats.push(stat);
		}

		if (!ctx.hasUI) return;
		// Keep the line present in idle: show the most recent completed rate
		// (or a placeholder) instead of clearing and shifting everything left.
		if (stat) lastRate = stat.rate;
		const ind = lastRate !== null ? lastInd(lastRate) : lastIndDash;
		ctx.ui.setStatus(STATUS_KEY, ctx.ui.theme.fg("muted", `· ${ind}${sessionSuffix()}`));
	});

	pi.registerCommand("tps", {
		description: "Show session token-throughput stats",
		handler: async (_args, ctx) => {
			if (!ctx.hasUI) return;
			if (stats.length === 0) {
				ctx.ui.notify("No completed assistant messages yet", "info");
				return;
			}
			const tokens = stats.reduce((a, s) => a + s.tokens, 0);
			const seconds = stats.reduce((a, s) => a + s.seconds, 0);
			const rates = stats.map((s) => s.rate);
			const lines = [
				`messages: ${stats.length}   tokens: ${tokens}   time: ${fmt(seconds)}s`,
				`avg (weighted): ${fmt(sessionAvg())} tok/s`,
				`min ${fmt(Math.min(...rates))} · p50 ${fmt(percentile(50))} · p90 ${fmt(
					percentile(90),
				)} · p95 ${fmt(percentile(95))} · max ${fmt(Math.max(...rates))}`,
			];
			ctx.ui.notify(lines.join("\n"), "info");
		},
	});

	pi.registerCommand("tps-clear", {
		description: "Reset tok/s session stats and clear the footer status",
		handler: async (_args, ctx) => {
			stats = [];
			lastRate = null;
			if (ctx.hasUI) ctx.ui.setStatus(STATUS_KEY, undefined);
		},
	});
}
