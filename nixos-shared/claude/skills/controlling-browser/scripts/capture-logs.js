#!/usr/bin/env nix
/*
#! nix shell nixpkgs#nodejs_22 --command node
*/

/**
 * Capture console logs, warnings, errors, and exceptions from the browser.
 *
 * Usage:
 *   ./capture-logs.js                          # Stream to stdout until Ctrl+C
 *   ./capture-logs.js --duration 5000          # Capture for 5 seconds
 *   ./capture-logs.js --output logs.txt        # Save to file
 *   ./capture-logs.js --format json            # JSON output
 *   ./capture-logs.js --level error            # Only errors
 */

import { writeFileSync } from "node:fs";
import { connect } from "./cdp.js";

const DEBUG = process.env.DEBUG === "1";
const log = DEBUG ? (...args) => console.error("[debug]", ...args) : () => {};

// Parse arguments
const args = process.argv.slice(2);
const getArg = (name) => {
  const idx = args.indexOf(name);
  return idx >= 0 ? args[idx + 1] : null;
};

const outputFile = getArg("--output");
const format = getArg("--format") || "text";
const duration = getArg("--duration") ? parseInt(getArg("--duration")) : null;
const levelFilter = getArg("--level");

const logs = [];
let cdp = null;

// Global timeout (30 minutes max)
const globalTimeout = setTimeout(() => {
  console.error("✗ Global timeout exceeded (30min)");
  cleanup();
  process.exit(1);
}, 30 * 60 * 1000);

function formatLogEntry(entry) {
  const time = entry.timestamp.toISOString();
  const loc = entry.location ? ` (${entry.location})` : "";
  return `${time} [${entry.level.toUpperCase()}] ${entry.message}${loc}`;
}

function saveLogs() {
  if (!outputFile || logs.length === 0) return;

  if (format === "json") {
    writeFileSync(outputFile, JSON.stringify(logs, null, 2));
  } else {
    const text = logs.map(formatLogEntry).join("\n");
    writeFileSync(outputFile, text);
  }
  console.error(`Saved ${logs.length} log entries to ${outputFile}`);
}

function cleanup() {
  clearTimeout(globalTimeout);
  saveLogs();
  if (cdp) cdp.close();
}

function shouldInclude(level) {
  if (!levelFilter) return true;
  // Map variations to canonical levels
  const levelMap = {
    log: "log",
    debug: "debug",
    info: "info",
    warn: "warning",
    warning: "warning",
    error: "error",
    exception: "error",
  };
  return levelMap[level] === levelFilter || level === levelFilter;
}

function extractMessage(msgArgs) {
  return msgArgs
    .map((arg) => {
      if (arg.value !== undefined) return String(arg.value);
      if (arg.description) return arg.description;
      if (arg.unserializableValue) return arg.unserializableValue;
      return `[${arg.type}]`;
    })
    .join(" ");
}

async function main() {
  log("connecting...");
  cdp = await connect(5000);

  log("getting pages...");
  const pages = await cdp.getPages();
  const page = pages.at(-1);

  if (!page) {
    console.error("✗ No active tab found");
    process.exit(1);
  }

  log("attaching to page...");
  const sessionId = await cdp.attachToPage(page.targetId);

  // Enable runtime events
  log("enabling runtime events...");
  await cdp.send("Runtime.enable", {}, sessionId);

  // Listen for console API calls (console.log, etc.)
  cdp.on("Runtime.consoleAPICalled", (event, evtSessionId) => {
    if (evtSessionId !== sessionId) return;

    const { type, args: msgArgs, timestamp } = event;

    if (!shouldInclude(type)) return;

    const message = extractMessage(msgArgs);
    const entry = {
      timestamp: new Date(timestamp),
      level: type,
      source: "console",
      message,
    };

    logs.push(entry);

    if (!outputFile) {
      console.log(`[${type.toUpperCase()}] ${message}`);
    }
  });

  // Listen for exceptions
  cdp.on("Runtime.exceptionThrown", (event, evtSessionId) => {
    if (evtSessionId !== sessionId) return;

    const { exceptionDetails } = event;
    const { text, exception, url, lineNumber, timestamp } = exceptionDetails;

    if (!shouldInclude("exception")) return;

    const message = exception?.description || text;
    const location = url ? `${url}:${lineNumber}` : "";

    const entry = {
      timestamp: new Date(timestamp),
      level: "exception",
      source: "runtime",
      message,
      location,
    };

    logs.push(entry);

    if (!outputFile) {
      const loc = location ? ` (${location})` : "";
      console.log(`[EXCEPTION] ${message}${loc}`);
    }
  });

  // Enable log domain for browser-level logs
  log("enabling log domain...");
  await cdp.send("Log.enable", {}, sessionId);

  cdp.on("Log.entryAdded", (event, evtSessionId) => {
    if (evtSessionId !== sessionId) return;

    const { entry } = event;
    const { level, text, url, lineNumber, timestamp } = entry;

    if (!shouldInclude(level)) return;

    const location = url ? `${url}:${lineNumber}` : "";
    const logEntry = {
      timestamp: new Date(timestamp),
      level,
      source: "browser",
      message: text,
      location,
    };

    logs.push(logEntry);

    if (!outputFile) {
      const loc = location ? ` (${location})` : "";
      console.log(`[${level.toUpperCase()}] ${text}${loc}`);
    }
  });

  // Wait for duration or indefinitely
  if (duration) {
    console.error(`Capturing logs for ${duration}ms...`);
    await new Promise((resolve) => setTimeout(resolve, duration));
    cleanup();
  } else {
    console.error("Capturing logs... Press Ctrl+C to stop");
    await new Promise(() => {}); // Wait forever
  }
}

process.on("SIGINT", () => {
  console.error("\nInterrupted");
  cleanup();
  process.exit(0);
});

process.on("SIGTERM", () => {
  cleanup();
  process.exit(0);
});

main().catch((e) => {
  console.error("✗", e.message);
  cleanup();
  process.exit(1);
});
