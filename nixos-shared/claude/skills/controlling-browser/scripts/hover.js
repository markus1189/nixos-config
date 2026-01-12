#!/usr/bin/env nix
/*
#! nix shell nixpkgs#nodejs_22 --command node
*/

import { connect } from "./cdp.js";

const DEBUG = process.env.DEBUG === "1";
const log = DEBUG ? (...args) => console.error("[debug]", ...args) : () => {};

function usage() {
  console.log("Usage: hover.js <selector> [--duration <ms>]");
  console.log("\nHover over an element to trigger hover states.");
  console.log("\nOptions:");
  console.log("  --duration <ms>  How long to hold hover (default: 0, returns immediately)");
  console.log("\nExamples:");
  console.log('  hover.js ".dropdown-trigger"');
  console.log('  hover.js "#menu-item" --duration 2000');
  console.log('  hover.js "nav a:first-child"');
  process.exit(1);
}

// Parse arguments
const args = process.argv.slice(2);
if (args.length === 0) usage();

let selector = null;
let duration = 0;

for (let i = 0; i < args.length; i++) {
  if (args[i] === "--duration") {
    duration = parseInt(args[++i], 10);
    if (isNaN(duration) || duration < 0) {
      console.error("✗ Invalid duration");
      process.exit(1);
    }
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

  log(`hovering at (${coords.x}, ${coords.y})...`);

  // Move mouse to element
  await cdp.send(
    "Input.dispatchMouseEvent",
    {
      type: "mouseMoved",
      x: coords.x,
      y: coords.y,
    },
    sessionId
  );

  // Trigger mouseenter/mouseover via JS for better compatibility
  await cdp.evaluate(
    sessionId,
    `(() => {
      const el = document.querySelector(${JSON.stringify(selector)});
      if (el) {
        el.dispatchEvent(new MouseEvent('mouseenter', { bubbles: true, cancelable: true }));
        el.dispatchEvent(new MouseEvent('mouseover', { bubbles: true, cancelable: true }));
      }
    })()`
  );

  if (duration > 0) {
    console.log(`✓ Hovering over ${selector} for ${duration}ms...`);
    await new Promise((r) => setTimeout(r, duration));
    console.log("✓ Hover complete");
  } else {
    console.log(`✓ Hovered over ${selector}`);
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
