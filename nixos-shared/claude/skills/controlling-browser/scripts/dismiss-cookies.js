#!/usr/bin/env nix
/*
#! nix shell nixpkgs#nodejs_22 --command node
*/

/**
 * Cookie Consent Dismissal Helper
 *
 * Automatically accepts cookie consent dialogs on EU websites.
 * Supports common CMPs: OneTrust, Cookiebot, Didomi, Quantcast, Google, BBC, Amazon, etc.
 *
 * Usage:
 *   ./dismiss-cookies.js          # Accept cookies
 *   ./dismiss-cookies.js --reject # Reject cookies (where possible)
 */

import { connect } from "./cdp.js";

const DEBUG = process.env.DEBUG === "1";
const log = DEBUG ? (...args) => console.error("[debug]", ...args) : () => {};

const reject = process.argv.includes("--reject");
const mode = reject ? "reject" : "accept";

// Import the large cookie dismissal script from the original file
import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { dirname, join } from "node:path";

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

// Read the implementation file
const implContent = readFileSync(join(__dirname, "dismiss-cookies-impl.js"), "utf-8");
const scriptMatch = implContent.match(/const COOKIE_DISMISS_SCRIPT = `(.+?)`;/s);
const COOKIE_DISMISS_SCRIPT = scriptMatch ? scriptMatch[1] : null;

if (!COOKIE_DISMISS_SCRIPT) {
  console.error("✗ Failed to load cookie dismissal script");
  process.exit(1);
}

// Global timeout
const globalTimeout = setTimeout(() => {
  console.error("✗ Global timeout exceeded (30s)");
  process.exit(1);
}, 30000);

try {
  log("connecting...");
  const cdp = await connect(5000);

  log("getting pages and frames...");
  const pages = await cdp.getPages();
  const page = pages.at(-1);

  if (!page) {
    console.error("✗ No active tab found");
    process.exit(1);
  }

  const sessionId = await cdp.attachToPage(page.targetId);
  const frameTree = await cdp.getFrameTree(sessionId);

  const frames = [frameTree];
  const stack = [frameTree];
  while (stack.length > 0) {
    const frame = stack.pop();
    if (frame.childFrames) {
      frames.push(...frame.childFrames);
      stack.push(...frame.childFrames);
    }
  }

  log(`trying ${mode} cookies in ${frames.length} frames...`);

  let clicked = [];
  for (const frame of frames) {
    try {
      const expression = `(${COOKIE_DISMISS_SCRIPT})(${!reject})`;
      const result = await cdp.evaluateInFrame(sessionId, frame.frame.id, expression, 5000);
      if (result && result.length > 0) {
        clicked.push(...result);
      }
    } catch (e) {
      log(`frame ${frame.frame.id} failed: ${e.message}`);
    }
  }

  if (clicked.length > 0) {
    console.log(`✓ Clicked ${clicked.length} consent button(s): ${clicked.join(", ")}`);
  } else {
    console.log("✓ No consent dialogs found (or already dismissed)");
  }

  log("closing...");
  cdp.close();
  log("done");
} catch (e) {
  console.error("✗", e.message);
  process.exit(1);
} finally {
  clearTimeout(globalTimeout);
  setTimeout(() => process.exit(0), 100);
}
