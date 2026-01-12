#!/usr/bin/env nix
/*
#! nix shell nixpkgs#nodejs_22 --command node
*/

import { connect } from "./cdp.js";

const DEBUG = process.env.DEBUG === "1";
const log = DEBUG ? (...args) => console.error("[debug]", ...args) : () => {};

function usage() {
  console.log("Usage: wait-for.js [options]");
  console.log("\nWait for element to appear, disappear, or become visible.");
  console.log("\nOptions:");
  console.log("  --selector <css>   CSS selector to wait for");
  console.log("  --text <string>    Wait for text to appear on page");
  console.log("  --disappear        Wait for element to be removed");
  console.log("  --visible          Wait for element to be visible (not hidden)");
  console.log("  --timeout <ms>     Max wait time (default: 30000)");
  console.log("  --interval <ms>    Polling interval (default: 100)");
  console.log("\nExamples:");
  console.log('  wait-for.js --selector "#dynamic-content"');
  console.log('  wait-for.js --selector ".loading" --disappear');
  console.log('  wait-for.js --selector "#button" --visible --timeout 10000');
  console.log('  wait-for.js --text "Loading complete"');
  process.exit(1);
}

// Parse arguments
const args = process.argv.slice(2);
if (args.length === 0) usage();

let selector = null;
let text = null;
let disappear = false;
let visible = false;
let timeout = 30000;
let interval = 100;

for (let i = 0; i < args.length; i++) {
  if (args[i] === "--selector") {
    selector = args[++i];
  } else if (args[i] === "--text") {
    text = args[++i];
  } else if (args[i] === "--disappear") {
    disappear = true;
  } else if (args[i] === "--visible") {
    visible = true;
  } else if (args[i] === "--timeout") {
    timeout = parseInt(args[++i], 10);
    if (isNaN(timeout) || timeout < 0) {
      console.error("✗ Invalid timeout");
      process.exit(1);
    }
  } else if (args[i] === "--interval") {
    interval = parseInt(args[++i], 10);
    if (isNaN(interval) || interval < 10) {
      console.error("✗ Invalid interval (min: 10ms)");
      process.exit(1);
    }
  }
}

if (!selector && !text) {
  console.error("✗ Must specify --selector or --text");
  usage();
}

if (text && (disappear || visible)) {
  console.error("✗ --disappear and --visible only work with --selector");
  process.exit(1);
}

// Build the condition expression
function buildCondition() {
  if (text) {
    return `document.body.textContent.includes(${JSON.stringify(text)})`;
  }

  if (disappear) {
    return `document.querySelector(${JSON.stringify(selector)}) === null`;
  }

  if (visible) {
    return `(() => {
      const el = document.querySelector(${JSON.stringify(selector)});
      if (!el) return false;
      const style = window.getComputedStyle(el);
      return style.display !== 'none' &&
             style.visibility !== 'hidden' &&
             style.opacity !== '0' &&
             el.offsetParent !== null;
    })()`;
  }

  // Default: wait for element to exist
  return `document.querySelector(${JSON.stringify(selector)}) !== null`;
}

// Global timeout
const globalTimeout = setTimeout(() => {
  console.error("✗ Global timeout exceeded");
  process.exit(1);
}, timeout + 5000);

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

  const condition = buildCondition();
  const modeDesc = text
    ? `text "${text.length > 30 ? text.slice(0, 27) + "..." : text}"`
    : disappear
      ? `${selector} to disappear`
      : visible
        ? `${selector} to be visible`
        : `${selector} to exist`;

  console.error(`Waiting for ${modeDesc} (timeout: ${timeout}ms)...`);
  log("condition:", condition);

  const startTime = Date.now();

  while (Date.now() - startTime < timeout) {
    const result = await cdp.evaluate(sessionId, condition);

    if (result === true) {
      const elapsed = Date.now() - startTime;
      console.log(`✓ Condition met after ${elapsed}ms`);
      cdp.close();
      clearTimeout(globalTimeout);
      setTimeout(() => process.exit(0), 100);
      break;
    }

    await new Promise((r) => setTimeout(r, interval));
  }

  if (Date.now() - startTime >= timeout) {
    console.error(`✗ Timeout after ${timeout}ms`);
    cdp.close();
    process.exit(1);
  }
} catch (e) {
  console.error("✗", e.message);
  process.exit(1);
} finally {
  clearTimeout(globalTimeout);
}
