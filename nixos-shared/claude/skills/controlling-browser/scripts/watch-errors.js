#!/usr/bin/env nix
/*
#! nix shell nixpkgs#nodejs_22 --command node
*/

/**
 * Watch for JavaScript errors and exceptions in real-time.
 * Simplified version of capture-logs.js focused on error monitoring.
 *
 * Usage:
 *   ./watch-errors.js                   # Stream errors to stdout
 *   ./watch-errors.js --output errs.txt # Also save to file
 */

import { writeFileSync } from "node:fs";
import { connect } from "./cdp.js";

const DEBUG = process.env.DEBUG === "1";
const log = DEBUG ? (...args) => console.error("[debug]", ...args) : () => {};

// Parse arguments
const args = process.argv.slice(2);
const outputIdx = args.indexOf("--output");
const outputFile = outputIdx >= 0 ? args[outputIdx + 1] : null;

const errors = [];
let cdp = null;

// Global timeout (30 minutes max)
const globalTimeout = setTimeout(() => {
  console.error("✗ Global timeout exceeded (30min)");
  cleanup();
  process.exit(1);
}, 30 * 60 * 1000);

function cleanup() {
  clearTimeout(globalTimeout);
  if (outputFile && errors.length > 0) {
    const text = errors
      .map((e) => {
        const time = e.timestamp.toISOString();
        const loc = e.location ? ` (${e.location})` : "";
        return `${time} [${e.level.toUpperCase()}] ${e.message}${loc}`;
      })
      .join("\n");
    writeFileSync(outputFile, text);
    console.error(`\nSaved ${errors.length} errors to ${outputFile}`);
  }
  if (cdp) cdp.close();
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

  // Listen for console.error calls
  cdp.on("Runtime.consoleAPICalled", (event, evtSessionId) => {
    if (evtSessionId !== sessionId) return;

    const { type, args: msgArgs, timestamp } = event;

    // Only capture errors
    if (type !== "error") return;

    const message = msgArgs
      .map((arg) => {
        if (arg.value !== undefined) return String(arg.value);
        if (arg.description) return arg.description;
        return `[${arg.type}]`;
      })
      .join(" ");

    const entry = {
      timestamp: new Date(timestamp),
      level: "error",
      source: "console",
      message,
    };

    errors.push(entry);
    console.log(`[ERROR] ${message}`);
  });

  // Listen for uncaught exceptions
  cdp.on("Runtime.exceptionThrown", (event, evtSessionId) => {
    if (evtSessionId !== sessionId) return;

    const { exceptionDetails } = event;
    const { text, exception, url, lineNumber, timestamp } = exceptionDetails;

    const message = exception?.description || text;
    const location = url ? `${url}:${lineNumber}` : "";

    const entry = {
      timestamp: new Date(timestamp),
      level: "exception",
      source: "runtime",
      message,
      location,
    };

    errors.push(entry);
    const loc = location ? ` (${location})` : "";
    console.log(`[EXCEPTION] ${message}${loc}`);
  });

  // Enable log domain for browser-level errors
  log("enabling log domain...");
  await cdp.send("Log.enable", {}, sessionId);

  cdp.on("Log.entryAdded", (event, evtSessionId) => {
    if (evtSessionId !== sessionId) return;

    const { entry } = event;
    const { level, text, url, lineNumber, timestamp } = entry;

    // Only capture errors
    if (level !== "error") return;

    const location = url ? `${url}:${lineNumber}` : "";
    const logEntry = {
      timestamp: new Date(timestamp),
      level: "error",
      source: "browser",
      message: text,
      location,
    };

    errors.push(logEntry);
    const loc = location ? ` (${location})` : "";
    console.log(`[ERROR] ${text}${loc}`);
  });

  console.error("Watching for errors... Press Ctrl+C to stop");
  await new Promise(() => {}); // Wait forever
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
