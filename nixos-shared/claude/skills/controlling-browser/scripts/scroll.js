#!/usr/bin/env nix
/*
#! nix shell nixpkgs#nodejs_22 --command node
*/

import { connect } from "./cdp.js";

const DEBUG = process.env.DEBUG === "1";
const log = DEBUG ? (...args) => console.error("[debug]", ...args) : () => {};

function usage() {
  console.log("Usage: scroll.js [options]");
  console.log("\nScroll the page or to a specific element.");
  console.log("\nOptions:");
  console.log("  --to-bottom           Scroll to bottom of page");
  console.log("  --to-top              Scroll to top of page");
  console.log("  --pixels <n>          Scroll down by n pixels (negative for up)");
  console.log("  --to-selector <sel>   Scroll element into view");
  console.log("  --smooth              Use smooth scrolling animation");
  console.log("  --infinite [--delay <ms>]  Scroll incrementally (for infinite scroll pages)");
  console.log("\nExamples:");
  console.log("  scroll.js --to-bottom");
  console.log("  scroll.js --to-top");
  console.log("  scroll.js --pixels 500");
  console.log("  scroll.js --pixels -300");
  console.log('  scroll.js --to-selector "#footer"');
  console.log('  scroll.js --to-selector ".comment-section" --smooth');
  console.log("  scroll.js --infinite --delay 1000");
  process.exit(1);
}

// Parse arguments
const args = process.argv.slice(2);
if (args.length === 0) usage();

let mode = null; // 'bottom', 'top', 'pixels', 'selector', 'infinite'
let pixelAmount = 0;
let selector = null;
let smooth = false;
let infiniteDelay = 500;

for (let i = 0; i < args.length; i++) {
  if (args[i] === "--to-bottom") {
    mode = "bottom";
  } else if (args[i] === "--to-top") {
    mode = "top";
  } else if (args[i] === "--pixels") {
    mode = "pixels";
    pixelAmount = parseInt(args[++i], 10);
    if (isNaN(pixelAmount)) {
      console.error("✗ Invalid pixel amount");
      process.exit(1);
    }
  } else if (args[i] === "--to-selector") {
    mode = "selector";
    selector = args[++i];
  } else if (args[i] === "--smooth") {
    smooth = true;
  } else if (args[i] === "--infinite") {
    mode = "infinite";
  } else if (args[i] === "--delay") {
    infiniteDelay = parseInt(args[++i], 10);
    if (isNaN(infiniteDelay) || infiniteDelay < 0) {
      console.error("✗ Invalid delay");
      process.exit(1);
    }
  }
}

if (!mode) {
  console.error("✗ No scroll mode specified");
  usage();
}

// Global timeout
const globalTimeout = setTimeout(() => {
  console.error("✗ Global timeout exceeded (60s)");
  process.exit(1);
}, 60000);

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

  const behavior = smooth ? "smooth" : "instant";

  if (mode === "bottom") {
    log("scrolling to bottom...");
    await cdp.evaluate(
      sessionId,
      `window.scrollTo({ top: document.body.scrollHeight, behavior: "${behavior}" })`
    );
    console.log("✓ Scrolled to bottom");
  } else if (mode === "top") {
    log("scrolling to top...");
    await cdp.evaluate(
      sessionId,
      `window.scrollTo({ top: 0, behavior: "${behavior}" })`
    );
    console.log("✓ Scrolled to top");
  } else if (mode === "pixels") {
    log(`scrolling ${pixelAmount} pixels...`);
    await cdp.evaluate(
      sessionId,
      `window.scrollBy({ top: ${pixelAmount}, behavior: "${behavior}" })`
    );
    const direction = pixelAmount >= 0 ? "down" : "up";
    console.log(`✓ Scrolled ${direction} ${Math.abs(pixelAmount)}px`);
  } else if (mode === "selector") {
    log(`scrolling to ${selector}...`);
    const found = await cdp.evaluate(
      sessionId,
      `(() => {
        const el = document.querySelector(${JSON.stringify(selector)});
        if (!el) return false;
        el.scrollIntoView({ behavior: "${behavior}", block: "center" });
        return true;
      })()`
    );
    if (found) {
      console.log(`✓ Scrolled to ${selector}`);
    } else {
      console.error(`✗ Element not found: ${selector}`);
      process.exit(1);
    }
  } else if (mode === "infinite") {
    log("starting infinite scroll...");
    console.log("Scrolling incrementally (Ctrl+C to stop)...");

    let lastHeight = 0;
    let sameHeightCount = 0;
    const maxSameHeight = 5;
    const scrollStep = 800;

    while (sameHeightCount < maxSameHeight) {
      // Get current scroll position and height
      const info = await cdp.evaluate(
        sessionId,
        `({ scrollY: window.scrollY, scrollHeight: document.body.scrollHeight })`
      );

      if (info.scrollHeight === lastHeight) {
        sameHeightCount++;
        log(`same height (${sameHeightCount}/${maxSameHeight})...`);
      } else {
        sameHeightCount = 0;
        lastHeight = info.scrollHeight;
      }

      // Scroll down
      await cdp.evaluate(
        sessionId,
        `window.scrollBy({ top: ${scrollStep}, behavior: "instant" })`
      );

      // Check if we've reached the bottom
      const atBottom = await cdp.evaluate(
        sessionId,
        `window.scrollY + window.innerHeight >= document.body.scrollHeight - 10`
      );

      if (atBottom && sameHeightCount >= maxSameHeight - 1) {
        log("reached bottom, stopping");
        break;
      }

      await new Promise((r) => setTimeout(r, infiniteDelay));
    }

    console.log("✓ Finished infinite scroll");
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
