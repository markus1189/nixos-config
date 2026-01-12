#!/usr/bin/env nix
/*
#! nix shell nixpkgs#nodejs_22 --command node
*/

import { connect } from "./cdp.js";

const DEBUG = process.env.DEBUG === "1";
const log = DEBUG ? (...args) => console.error("[debug]", ...args) : () => {};

function usage() {
  console.log("Usage: click.js <selector> [--wait <ms>] [--double]");
  console.log("\nClick an element by CSS selector.");
  console.log("\nOptions:");
  console.log("  --wait <ms>   Wait for element to appear (default: 0)");
  console.log("  --double      Double-click instead of single click");
  console.log("\nExamples:");
  console.log('  click.js "button[type=submit]"');
  console.log('  click.js "#login-button"');
  console.log('  click.js ".cookie-accept" --wait 2000');
  console.log('  click.js "tr.row" --double');
  process.exit(1);
}

// Parse arguments
const args = process.argv.slice(2);
if (args.length === 0) usage();

let selector = null;
let waitTime = 0;
let doubleClick = false;

for (let i = 0; i < args.length; i++) {
  if (args[i] === "--wait") {
    waitTime = parseInt(args[++i], 10);
    if (isNaN(waitTime)) {
      console.error("✗ Invalid wait time");
      process.exit(1);
    }
  } else if (args[i] === "--double") {
    doubleClick = true;
  } else if (!selector) {
    selector = args[i];
  }
}

if (!selector) {
  console.error("✗ No selector provided");
  usage();
}

// Global timeout
const globalTimeout = setTimeout(() => {
  console.error("✗ Global timeout exceeded (45s)");
  process.exit(1);
}, 45000);

try {
  log("connecting...");
  const cdp = await connect(5000);

  log("getting pages...");
  const pages = await cdp.getPages();
  const page = pages.at(-1);

  if (!page) {
    console.error("✗ No active tab found");
    process.exit(1);
  }

  log("attaching to page...");
  const sessionId = await cdp.attachToPage(page.targetId);

  // Wait for element if requested
  if (waitTime > 0) {
    log(`waiting for element (${waitTime}ms max)...`);
    const waitStart = Date.now();
    while (Date.now() - waitStart < waitTime) {
      const exists = await cdp.evaluate(
        sessionId,
        `!!document.querySelector(${JSON.stringify(selector)})`
      );
      if (exists) break;
      await new Promise((r) => setTimeout(r, 100));
    }
  }

  // Get element coordinates
  log("getting element coordinates...");
  const coords = await cdp.evaluate(
    sessionId,
    `(() => {
      const el = document.querySelector(${JSON.stringify(selector)});
      if (!el) return null;
      const rect = el.getBoundingClientRect();
      return { x: rect.left + rect.width / 2, y: rect.top + rect.height / 2 };
    })()`
  );

  if (!coords) {
    console.error("✗ Element not found:", selector);
    process.exit(1);
  }

  log(`clicking at (${coords.x}, ${coords.y})...`);
  const clickCount = doubleClick ? 2 : 1;

  // Mouse down
  await cdp.send(
    "Input.dispatchMouseEvent",
    {
      type: "mousePressed",
      x: coords.x,
      y: coords.y,
      button: "left",
      clickCount,
    },
    sessionId
  );

  // Mouse up
  await cdp.send(
    "Input.dispatchMouseEvent",
    {
      type: "mouseReleased",
      x: coords.x,
      y: coords.y,
      button: "left",
      clickCount,
    },
    sessionId
  );

  console.log(`✓ Clicked ${selector}${doubleClick ? " (double)" : ""}`);

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
