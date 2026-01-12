#!/usr/bin/env nix
/*
#! nix shell nixpkgs#nodejs_22 --command node
*/

import { connect } from "./cdp.js";
import { readFileSync } from "fs";

const DEBUG = process.env.DEBUG === "1";
const log = DEBUG ? (...args) => console.error("[debug]", ...args) : () => {};

function usage() {
  console.log("Usage: fill-form.js [--field <sel> --value <val>]... [--json <file>] [--submit <sel>]");
  console.log("\nFill multiple form fields at once.");
  console.log("\nOptions:");
  console.log("  --field <sel> --value <val>  Selector and value pair (repeatable)");
  console.log("  --json <file>                JSON file with {selector: value} pairs");
  console.log("  --submit <sel>               Click submit button after filling");
  console.log("\nExamples:");
  console.log('  fill-form.js --field "#email" --value "user@example.com" --field "#password" --value "secret"');
  console.log('  fill-form.js --json form-data.json');
  console.log('  fill-form.js --field "#username" --value "test" --submit "button[type=submit]"');
  process.exit(1);
}

// Parse arguments
const args = process.argv.slice(2);
if (args.length === 0) usage();

const fields = []; // Array of {selector, value}
let jsonFile = null;
let submitSelector = null;

for (let i = 0; i < args.length; i++) {
  if (args[i] === "--field") {
    const selector = args[++i];
    if (args[i + 1] !== "--value") {
      console.error("✗ --field must be followed by --value");
      process.exit(1);
    }
    i++; // skip --value
    const value = args[++i];
    fields.push({ selector, value });
  } else if (args[i] === "--value") {
    console.error("✗ --value must follow --field");
    process.exit(1);
  } else if (args[i] === "--json") {
    jsonFile = args[++i];
  } else if (args[i] === "--submit") {
    submitSelector = args[++i];
  }
}

// Load JSON file if provided
if (jsonFile) {
  try {
    const data = JSON.parse(readFileSync(jsonFile, "utf8"));
    for (const [selector, value] of Object.entries(data)) {
      fields.push({ selector, value: String(value) });
    }
  } catch (e) {
    console.error("✗ Failed to read JSON file:", e.message);
    process.exit(1);
  }
}

if (fields.length === 0) {
  console.error("✗ No fields provided");
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

  // Helper to click and type into a field
  async function fillField(selector, value) {
    // Get element coordinates
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
      return { success: false, error: "Element not found" };
    }

    // Click to focus
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

    await new Promise((r) => setTimeout(r, 50));

    // Select all and clear
    await cdp.send(
      "Input.dispatchKeyEvent",
      { type: "keyDown", key: "a", code: "KeyA", modifiers: 2 },
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

    // Insert text
    await cdp.send("Input.insertText", { text: value }, sessionId);

    // Trigger events
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

    return { success: true };
  }

  // Fill all fields
  let successCount = 0;
  for (const { selector, value } of fields) {
    log(`filling ${selector}...`);
    const result = await fillField(selector, value);
    if (result.success) {
      successCount++;
      console.log(`✓ Filled ${selector}`);
    } else {
      console.error(`✗ Failed to fill ${selector}: ${result.error}`);
    }
    await new Promise((r) => setTimeout(r, 100)); // Small delay between fields
  }

  // Submit if requested
  if (submitSelector) {
    log("clicking submit...");
    const coords = await cdp.evaluate(
      sessionId,
      `(() => {
        const el = document.querySelector(${JSON.stringify(submitSelector)});
        if (!el) return null;
        const rect = el.getBoundingClientRect();
        return { x: rect.left + rect.width / 2, y: rect.top + rect.height / 2 };
      })()`
    );

    if (coords) {
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
      console.log(`✓ Clicked ${submitSelector}`);
    } else {
      console.error(`✗ Submit button not found: ${submitSelector}`);
    }
  }

  console.log(`\n✓ Filled ${successCount}/${fields.length} fields`);

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
