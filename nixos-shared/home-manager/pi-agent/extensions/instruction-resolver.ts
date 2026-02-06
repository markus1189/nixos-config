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
 * - Checks AGENTS.md before CLAUDE.md per directory (first match wins)
 * - Deduplicates per branch: each file is injected at most once
 * - Self-exclusion: reading an AGENTS.md directly won't re-inject it
 * - Branch-aware state: uses pi.appendEntry() so forks/tree navigation
 *   correctly re-discover instruction files on new branches
 *
 * Walk boundaries:
 * - Files inside ctx.cwd: walks up to ctx.cwd (exclusive)
 * - Files outside ctx.cwd: walks up to $HOME (inclusive)
 * - Files pi already loaded into the system prompt (cwd ancestors +
 *   ~/.pi/agent/) are pre-excluded via discoverPreloaded()
 *
 * Debug logging: set INSTRUCTION_RESOLVER_DEBUG=1 to log to ~/.pi/agent/instruction-resolver.log
 */

import * as fs from "node:fs";
import * as path from "node:path";
import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";

const INSTRUCTION_FILES = ["AGENTS.md", "CLAUDE.md"];
const ENTRY_TYPE = "instruction-resolver";

const DEBUG = !!process.env.INSTRUCTION_RESOLVER_DEBUG;
const LOG_FILE = DEBUG
  ? path.join(
      process.env.HOME ?? "/tmp",
      ".pi",
      "agent",
      "instruction-resolver.log",
    )
  : "";

function log(message: string) {
  if (!DEBUG) return;
  try {
    fs.appendFileSync(LOG_FILE, `[${new Date().toISOString()}] ${message}\n`);
  } catch {
    // Ignore logging errors
  }
}

const HOME = process.env.HOME ?? "/";
const AGENT_DIR = path.join(HOME, ".pi", "agent");

/**
 * Discover which AGENTS.md/CLAUDE.md files pi already loaded into the system
 * prompt at startup. Replicates pi's loadProjectContextFiles algorithm:
 * - ~/.pi/agent/AGENTS.md (global)
 * - Walk from cwd up to / checking each directory
 */
function discoverPreloaded(cwd: string): Set<string> {
  const preloaded = new Set<string>();

  // Global agent dir
  for (const file of INSTRUCTION_FILES) {
    const candidate = path.join(AGENT_DIR, file);
    try {
      if (fs.existsSync(candidate)) {
        preloaded.add(candidate);
        break;
      }
    } catch {
      // Ignore
    }
  }

  // Walk from cwd up to filesystem root (same as pi's loadProjectContextFiles)
  let dir = cwd;
  while (true) {
    for (const file of INSTRUCTION_FILES) {
      const candidate = path.join(dir, file);
      try {
        if (fs.existsSync(candidate) && !preloaded.has(candidate)) {
          preloaded.add(candidate);
          break;
        }
      } catch {
        // Ignore
      }
    }
    const parent = path.dirname(dir);
    if (parent === dir) break;
    dir = parent;
  }

  return preloaded;
}

/**
 * Determine the walk boundary for a file path.
 * - Inside ctx.cwd: use cwd as exclusive boundary
 * - Outside ctx.cwd: use $HOME as boundary
 *
 * Returns { root, exclusive } where `exclusive` controls whether the root
 * directory itself is excluded from scanning.
 */
function walkBoundary(
  filepath: string,
  cwd: string,
): { root: string; exclusive: boolean } {
  if (filepath.startsWith(cwd + "/") || filepath === cwd) {
    return { root: cwd, exclusive: true };
  }
  return { root: HOME, exclusive: false };
}

export default function instructionResolver(pi: ExtensionAPI) {
  const loaded = new Set<string>();
  let projectRoot = "";

  /**
   * Rebuild `loaded` from:
   * 1. Files pi already loaded into the system prompt (preloaded)
   * 2. Persisted entries on the current branch (previously injected by us)
   */
  function rebuildState(ctx: {
    sessionManager: {
      getBranch(): Array<{ type: string; customType?: string; data?: unknown }>;
    };
  }) {
    loaded.clear();

    // Seed with files pi already has in the system prompt
    const preloaded = discoverPreloaded(projectRoot);
    for (const p of preloaded) loaded.add(p);
    log(`preloaded: ${preloaded.size} paths [${[...preloaded].join(", ")}]`);

    // Add files previously injected on this branch
    for (const entry of ctx.sessionManager.getBranch()) {
      if (entry.type === "custom" && entry.customType === ENTRY_TYPE) {
        const paths = (entry.data as { paths?: string[] })?.paths;
        if (Array.isArray(paths)) {
          for (const p of paths) loaded.add(p);
        }
      }
    }
    log(`rebuildState: ${loaded.size} total paths [${[...loaded].join(", ")}]`);
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
    const boundary = walkBoundary(filepath, projectRoot);

    log(
      `read: ${filepath} (boundary=${boundary.root}, exclusive=${boundary.exclusive})`,
    );

    let current = path.dirname(filepath);
    const discovered: string[] = [];
    const newPaths: string[] = [];

    while (
      current.startsWith(boundary.root) &&
      (boundary.exclusive ? current !== boundary.root : true)
    ) {
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
        i === event.content.length - 1 && c.type === "text"
          ? { ...c, text: c.text + reminder }
          : c,
      );

      return {
        content: modified,
        details: event.details,
        isError: event.isError,
      };
    }
  });
}
