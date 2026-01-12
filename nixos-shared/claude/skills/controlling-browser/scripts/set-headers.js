#!/usr/bin/env nix
/*
#! nix shell nixpkgs#nodejs_22 --command node
*/

/**
 * Globally set HTTP headers for all browser requests.
 *
 * Usage:
 *   ./set-headers.js "Authorization: Bearer token123"
 *   ./set-headers.js "User-Agent: MyBot" "X-Custom: value"
 *   ./set-headers.js --remove Cookie --remove Accept-Language
 *
 * Runs until Ctrl+C. All requests will have headers modified.
 */

import { connect } from "./cdp.js";

const DEBUG = process.env.DEBUG === "1";
const log = DEBUG ? (...args) => console.error("[debug]", ...args) : () => {};

// Parse arguments
const args = process.argv.slice(2);
const headersToSet = []; // [{name, value}]
const headersToRemove = new Set(); // lowercase header names

let i = 0;
while (i < args.length) {
  if (args[i] === "--remove" && args[i + 1]) {
    headersToRemove.add(args[i + 1].toLowerCase());
    i += 2;
  } else if (args[i].includes(":")) {
    const colonIdx = args[i].indexOf(":");
    const name = args[i].slice(0, colonIdx).trim();
    const value = args[i].slice(colonIdx + 1).trim();
    if (name) {
      headersToSet.push({ name, value });
    }
    i++;
  } else {
    console.error(`Ignoring invalid argument: ${args[i]}`);
    i++;
  }
}

if (headersToSet.length === 0 && headersToRemove.size === 0) {
  console.error("Usage: set-headers.js \"Header-Name: value\" [--remove Header-Name]");
  console.error("");
  console.error("Examples:");
  console.error('  ./set-headers.js "Authorization: Bearer token123"');
  console.error('  ./set-headers.js "User-Agent: CustomBot" "X-Custom: value"');
  console.error('  ./set-headers.js --remove Cookie --remove Accept-Language');
  process.exit(1);
}

let cdp = null;
let requestCount = 0;

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
    requestCount++;

    log(`[${requestCount}] ${request.method} ${request.url}`);

    try {
      // Build new headers array from original
      const headers = Object.entries(request.headers)
        .filter(([name]) => !headersToRemove.has(name.toLowerCase()))
        .map(([name, value]) => ({ name, value }));

      // Add/override headers
      for (const h of headersToSet) {
        const idx = headers.findIndex(
          (x) => x.name.toLowerCase() === h.name.toLowerCase()
        );
        if (idx >= 0) {
          headers[idx] = h;
        } else {
          headers.push(h);
        }
      }

      await cdp.send(
        "Fetch.continueRequest",
        { requestId, headers },
        evtSessionId
      );
    } catch (e) {
      // Request may have been cancelled
      log(`Failed to continue request: ${e.message}`);
    }
  });

  // Show what we're doing
  const setMsg = headersToSet.map((h) => `  + ${h.name}: ${h.value}`).join("\n");
  const removeMsg = [...headersToRemove].map((h) => `  - ${h}`).join("\n");

  console.error("Intercepting requests...");
  if (setMsg) console.error("Setting headers:\n" + setMsg);
  if (removeMsg) console.error("Removing headers:\n" + removeMsg);
  console.error("\nPress Ctrl+C to stop");

  await new Promise(() => {}); // Wait forever
}

process.on("SIGINT", () => {
  console.error(`\nIntercepted ${requestCount} requests`);
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
