/**
 * AGENTS.md Subdirectory Resolver Extension
 *
 * When the read tool reads a file, walks up from that file's directory toward
 * the project root (exclusive), looking for AGENTS.md or CLAUDE.md files in
 * each intermediate directory. Discovered instruction files are appended to
 * the read tool's output as <system-reminder> blocks.
 *
 * Key behaviors:
 * - Only triggers on the "read" tool
 * - Walks from the read target's directory up to (but excluding) the project root
 * - Checks AGENTS.md before CLAUDE.md per directory (first match wins)
 * - Deduplicates per branch: each file is injected at most once
 * - Self-exclusion: reading an AGENTS.md directly won't re-inject it
 * - Root exclusion: files at the project root are skipped (belong in system prompt)
 * - For files outside ctx.cwd, finds the file's repo root via .git
 * - Branch-aware state: uses pi.appendEntry() so forks/tree navigation
 *   correctly re-discover instruction files on new branches
 *
 * Debug logging: set AGENTS_MD_DEBUG=1 to log to ~/.pi/agent/agents-md-resolver.log
 */

import * as fs from "node:fs";
import * as path from "node:path";
import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";

const INSTRUCTION_FILES = ["AGENTS.md", "CLAUDE.md"];
const ENTRY_TYPE = "agents-md-resolver";

const DEBUG = !!process.env.AGENTS_MD_DEBUG;
const LOG_FILE = DEBUG
	? path.join(process.env.HOME ?? "/tmp", ".pi", "agent", "agents-md-resolver.log")
	: "";

function log(message: string) {
	if (!DEBUG) return;
	try {
		fs.appendFileSync(LOG_FILE, `[${new Date().toISOString()}] ${message}\n`);
	} catch {
		// Ignore logging errors
	}
}

/**
 * Find the project root for a given file path.
 * Uses ctx.cwd if the file is within it, otherwise walks up looking for .git.
 */
function findRoot(filepath: string, cwd: string): string | undefined {
	if (filepath.startsWith(cwd + "/") || filepath === cwd) {
		return cwd;
	}

	let dir = path.dirname(filepath);
	while (true) {
		try {
			if (fs.existsSync(path.join(dir, ".git"))) return dir;
		} catch {
			// Ignore errors
		}
		const parent = path.dirname(dir);
		if (parent === dir) break;
		dir = parent;
	}

	return undefined;
}

export default function agentsMdResolver(pi: ExtensionAPI) {
	const loaded = new Set<string>();
	let projectRoot = "";

	/** Rebuild `loaded` from persisted entries on the current branch. */
	function rebuildState(ctx: {
		sessionManager: {
			getBranch(): Array<{ type: string; customType?: string; data?: unknown }>;
		};
	}) {
		loaded.clear();
		for (const entry of ctx.sessionManager.getBranch()) {
			if (entry.type === "custom" && entry.customType === ENTRY_TYPE) {
				const paths = (entry.data as { paths?: string[] })?.paths;
				if (Array.isArray(paths)) {
					for (const p of paths) loaded.add(p);
				}
			}
		}
		log(`rebuildState: ${loaded.size} paths [${[...loaded].join(", ")}]`);
	}

	pi.on("session_start", async (_event, ctx) => {
		projectRoot = path.resolve(ctx.cwd);
		rebuildState(ctx);
		log(`session_start: projectRoot=${projectRoot}`);
	});

	pi.on("session_switch", async (_event, ctx) => {
		projectRoot = path.resolve(ctx.cwd);
		rebuildState(ctx);
		log(`session_switch: projectRoot=${projectRoot}`);
	});

	pi.on("session_fork", async (_event, ctx) => {
		rebuildState(ctx);
		log(`session_fork: rebuilt state`);
	});

	pi.on("tool_result", async (event) => {
		if (event.toolName !== "read") return;
		if (event.isError) return;

		const inputPath = (event.input as { path?: string }).path;
		if (!inputPath) return;

		const filepath = path.resolve(projectRoot, inputPath);
		const root = findRoot(filepath, projectRoot);
		if (!root) {
			log(`read: ${filepath} — no project root found, skipping`);
			return;
		}

		log(`read: ${filepath} (root=${root})`);

		let current = path.dirname(filepath);
		const discovered: string[] = [];
		const newPaths: string[] = [];

		while (current.startsWith(root) && current !== root) {
			for (const file of INSTRUCTION_FILES) {
				const candidate = path.join(current, file);
				if (candidate === filepath) continue;
				if (loaded.has(candidate)) continue;

				try {
					if (fs.existsSync(candidate)) {
						loaded.add(candidate);
						newPaths.push(candidate);
						const content = fs.readFileSync(candidate, "utf-8");
						discovered.push(`Instructions from: ${candidate}\n${content}`);
						log(`  INJECTED: ${candidate} (${content.length} bytes)`);
						break;
					}
				} catch {
					// Ignore read errors
				}
			}
			current = path.dirname(current);
		}

		if (discovered.length > 0) {
			log(`  discovered ${discovered.length} instruction file(s)`);
			pi.appendEntry(ENTRY_TYPE, { paths: newPaths });

			const reminder = `\n\n<system-reminder>\n${discovered.join("\n\n")}\n</system-reminder>`;
			const modified = event.content.map((c, i) =>
				i === event.content.length - 1 && c.type === "text" ? { ...c, text: c.text + reminder } : c
			);

			return { content: modified, details: event.details, isError: event.isError };
		}
	});
}
