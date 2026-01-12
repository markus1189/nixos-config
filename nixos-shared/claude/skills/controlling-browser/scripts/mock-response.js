#!/usr/bin/env nix
/*
#! nix shell nixpkgs#nodejs_22 --command node
*/

// Return mock responses for requests matching a URL pattern.
//
// Usage:
//   ./mock-response.js --url "*api/user*" --body '{"name":"test"}'
//   ./mock-response.js --url "*api*" --file mock.json
//   ./mock-response.js --url "*api*" --body '{}' --status 404
//
// Runs until Ctrl+C.

import { readFileSync } from "node:fs";
import { connect } from "./cdp.js";

const DEBUG = process.env.DEBUG === "1";
const log = DEBUG ? (...args) => console.error("[debug]", ...args) : () => {};

// Parse arguments
const args = process.argv.slice(2);
const getArg = (name) => {
  const idx = args.indexOf(name);
  return idx >= 0 ? args[idx + 1] : null;
};

const urlPattern = getArg("--url");
const bodyArg = getArg("--body");
const fileArg = getArg("--file");
const statusCode = parseInt(getArg("--status") || "200");
const contentType = getArg("--content-type");

if (!urlPattern || (!bodyArg && !fileArg)) {
  console.error("Usage: mock-response.js --url <pattern> --body <json> [options]");
  console.error("       mock-response.js --url <pattern> --file <path> [options]");
  console.error("");
  console.error("Options:");
  console.error("  --url <pattern>        URL pattern to match (glob-style)");
  console.error("  --body <content>       Response body content");
  console.error("  --file <path>          Read response body from file");
  console.error("  --status <code>        HTTP status code (default: 200)");
  console.error("  --content-type <type>  Content-Type header");
  console.error("");
  console.error("Examples:");
  console.error('  ./mock-response.js --url "*/api/user" --body \'{"name":"test"}\'');
  console.error('  ./mock-response.js --url "*/api/*" --file mock.json --status 201');
  process.exit(1);
}

// Load response body
let responseBody;
try {
  if (fileArg) {
    responseBody = readFileSync(fileArg, "utf-8");
  } else {
    responseBody = bodyArg;
  }
} catch (e) {
  console.error(`✗ Failed to read file: ${e.message}`);
  process.exit(1);
}

// Auto-detect content type if not specified
let finalContentType = contentType;
if (!finalContentType) {
  try {
    JSON.parse(responseBody);
    finalContentType = "application/json";
  } catch {
    finalContentType = "text/plain";
  }
}

// Convert glob pattern to regex
function patternToRegex(pattern) {
  const escaped = pattern.replace(/[.+^${}()|[\]\\]/g, "\\$&");
  const regexStr = escaped.replace(/\*/g, ".*");
  return new RegExp(regexStr, "i");
}

const urlRegex = patternToRegex(urlPattern);

let cdp = null;
let mockedCount = 0;
let passedCount = 0;

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
      if (urlRegex.test(request.url)) {
        mockedCount++;
        log(`[MOCKED] ${request.url}`);

        // Build response headers
        const responseHeaders = [
          { name: "Content-Type", value: finalContentType },
          { name: "Access-Control-Allow-Origin", value: "*" },
        ];

        await cdp.send(
          "Fetch.fulfillRequest",
          {
            requestId,
            responseCode: statusCode,
            responseHeaders,
            body: Buffer.from(responseBody).toString("base64"),
          },
          evtSessionId
        );
      } else {
        passedCount++;
        log(`[PASSED] ${request.url}`);
        await cdp.send("Fetch.continueRequest", { requestId }, evtSessionId);
      }
    } catch (e) {
      // Request may have been cancelled
      log(`Failed to handle request: ${e.message}`);
    }
  });

  console.error(`Mocking requests matching: ${urlPattern}`);
  console.error(`Response: ${statusCode} ${finalContentType}`);
  console.error(`Body: ${responseBody.slice(0, 100)}${responseBody.length > 100 ? "..." : ""}`);
  console.error("\nPress Ctrl+C to stop");

  await new Promise(() => {}); // Wait forever
}

process.on("SIGINT", () => {
  console.error(`\nMocked: ${mockedCount}, Passed: ${passedCount}`);
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
