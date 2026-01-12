#!/usr/bin/env nix
/*
#! nix shell nixpkgs#nodejs_22 --command node
*/

import { connect } from "./cdp.js";

const DEBUG = process.env.DEBUG === "1";
const log = DEBUG ? (...args) => console.error("[debug]", ...args) : () => {};

function usage() {
  console.log("Usage: type-text.js <text> [--selector <sel>] [--delay <ms>] [--clear]");
  console.log("\nType text into focused element or specified selector.");
  console.log("\nOptions:");
  console.log("  --selector <sel>  Click selector before typing");
  console.log("  --delay <ms>      Delay between keystrokes (human-like typing)");
  console.log("  --clear           Clear existing value before typing");
  console.log("\nExamples:");
  console.log('  type-text.js "Hello world"');
  console.log('  type-text.js "Search query" --selector "input[name=q]"');
  console.log('  type-text.js "Password123" --selector "#password" --clear');
  console.log('  type-text.js "Slow typing" --delay 100');
  process.exit(1);
}

// Parse arguments
const args = process.argv.slice(2);
if (args.length === 0) usage();

let text = null;
let selector = null;
let delay = 0;
let clear = false;

for (let i = 0; i < args.length; i++) {
  if (args[i] === "--selector") {
    selector = args[++i];
  } else if (args[i] === "--delay") {
    delay = parseInt(args[++i], 10);
    if (isNaN(delay)) {
      console.error("✗ Invalid delay");
      process.exit(1);
    }
  } else if (args[i] === "--clear") {
    clear = true;
  } else if (!text) {
    text = args[i];
  }
}

if (!text) {
  console.error("✗ No text provided");
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

  // Click selector to focus if provided
  if (selector) {
    log("clicking selector to focus...");
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

    await cdp.send(
      "Input.dispatchMouseEvent",
      { type: "mousePressed", x: coords.x, y: coords.y, button: "left", clickCount: 1 },
      sessionId
    );
    await cdp.send(
      "Input.dispatchMouseEvent",
      { type: "mouseReleased", x: coords.x, y: coords.y, button: "left", clickCount: 1 },
      sessionId
    );

    // Small delay for focus to take effect
    await new Promise((r) => setTimeout(r, 50));
  }

  // Clear existing value if requested
  if (clear) {
    log("clearing existing value...");
    // Select all (Ctrl+A) then delete
    await cdp.send(
      "Input.dispatchKeyEvent",
      { type: "keyDown", key: "a", code: "KeyA", modifiers: 2 }, // 2 = Ctrl
      sessionId
    );
    await cdp.send(
      "Input.dispatchKeyEvent",
      { type: "keyUp", key: "a", code: "KeyA", modifiers: 2 },
      sessionId
    );
    await cdp.send(
      "Input.dispatchKeyEvent",
      { type: "keyDown", key: "Backspace", code: "Backspace" },
      sessionId
    );
    await cdp.send(
      "Input.dispatchKeyEvent",
      { type: "keyUp", key: "Backspace", code: "Backspace" },
      sessionId
    );
    await new Promise((r) => setTimeout(r, 50));
  }

  // Type the text
  if (delay > 0) {
    // Human-like typing with delays
    log(`typing with ${delay}ms delay...`);
    for (const char of text) {
      await cdp.send("Input.insertText", { text: char }, sessionId);
      await new Promise((r) => setTimeout(r, delay));
    }
  } else {
    // Fast insertion
    log("inserting text...");
    await cdp.send("Input.insertText", { text }, sessionId);
  }

  // Trigger input event for frameworks that need it
  if (selector) {
    await cdp.evaluate(
      sessionId,
      `(() => {
        const el = document.querySelector(${JSON.stringify(selector)});
        if (el) {
          el.dispatchEvent(new Event('input', { bubbles: true }));
          el.dispatchEvent(new Event('change', { bubbles: true }));
        }
      })()`
    );
  }

  const display = text.length > 30 ? text.slice(0, 27) + "..." : text;
  console.log(`✓ Typed "${display}"${selector ? ` into ${selector}` : ""}`);

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
