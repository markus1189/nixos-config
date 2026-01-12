#!/usr/bin/env nix
/*
#! nix shell nixpkgs#nodejs_22 --command node
*/

// Block requests matching URL patterns.
//
// Usage:
//   ./block-requests.js "*\/ads\/*"
//   ./block-requests.js "*google-analytics*" "*facebook*"
//
// Patterns use glob-style matching (* = any characters).
// Runs until Ctrl+C.

import { connect } from "./cdp.js";

const DEBUG = process.env.DEBUG === "1";
const log = DEBUG ? (...args) => console.error("[debug]", ...args) : () => {};

// Parse arguments - all positional args are patterns
const patterns = process.argv.slice(2).filter((arg) => !arg.startsWith("-"));

if (patterns.length === 0) {
  console.error("Usage: block-requests.js <pattern> [pattern...]");
  console.error("");
  console.error("Examples:");
  console.error('  ./block-requests.js "*/ads/*"');
  console.error('  ./block-requests.js "*google-analytics*" "*facebook*"');
  console.error("");
  console.error("Patterns use glob-style (* matches any characters)");
  process.exit(1);
}

// Convert glob pattern to regex
function patternToRegex(pattern) {
  const escaped = pattern.replace(/[.+^${}()|[\]\\]/g, "\\$&");
  const regexStr = escaped.replace(/\*/g, ".*");
  return new RegExp(regexStr, "i");
}

const regexPatterns = patterns.map(patternToRegex);

function matchesAnyPattern(url) {
  return regexPatterns.some((re) => re.test(url));
}

let cdp = null;
let blockedCount = 0;
let allowedCount = 0;

// Global timeout (1 hour max)
const globalTimeout = setTimeout(() => {
  console.error("✗ Global timeout exceeded (1h)");
  cleanup();
  process.exit(1);
}, 60 * 60 * 1000);

function cleanup() {
  clearTimeout(globalTimeout);
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

  // Enable Fetch domain with wildcard pattern
  log("enabling fetch interception...");
  await cdp.send(
    "Fetch.enable",
    {
      patterns: [{ urlPattern: "*", requestStage: "Request" }],
    },
    sessionId
  );

  // Handle paused requests
  cdp.on("Fetch.requestPaused", async (event, evtSessionId) => {
    if (evtSessionId !== sessionId) return;

    const { requestId, request } = event;

    try {
      if (matchesAnyPattern(request.url)) {
        blockedCount++;
        log(`[BLOCKED] ${request.url}`);
        await cdp.send(
          "Fetch.failRequest",
          { requestId, errorReason: "BlockedByClient" },
          evtSessionId
        );
      } else {
        allowedCount++;
        log(`[ALLOWED] ${request.url}`);
        await cdp.send("Fetch.continueRequest", { requestId }, evtSessionId);
      }
    } catch (e) {
      // Request may have been cancelled
      log(`Failed to handle request: ${e.message}`);
    }
  });

  console.error("Blocking requests matching:");
  patterns.forEach((p) => console.error(`  ${p}`));
  console.error("\nPress Ctrl+C to stop");

  await new Promise(() => {}); // Wait forever
}

process.on("SIGINT", () => {
  console.error(`\nBlocked: ${blockedCount}, Allowed: ${allowedCount}`);
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
