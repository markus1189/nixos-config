#!/usr/bin/env nix
/*
#! nix shell nixpkgs#nodejs_22 --command node
*/

import { connect } from "./cdp.js";

const DEBUG = process.env.DEBUG === "1";
const log = DEBUG ? (...args) => console.error("[debug]", ...args) : () => {};

// Key mapping for common keys
const KEY_MAP = {
  enter: { key: "Enter", code: "Enter" },
  escape: { key: "Escape", code: "Escape" },
  esc: { key: "Escape", code: "Escape" },
  tab: { key: "Tab", code: "Tab" },
  backspace: { key: "Backspace", code: "Backspace" },
  delete: { key: "Delete", code: "Delete" },
  space: { key: " ", code: "Space" },
  arrowup: { key: "ArrowUp", code: "ArrowUp" },
  arrowdown: { key: "ArrowDown", code: "ArrowDown" },
  arrowleft: { key: "ArrowLeft", code: "ArrowLeft" },
  arrowright: { key: "ArrowRight", code: "ArrowRight" },
  up: { key: "ArrowUp", code: "ArrowUp" },
  down: { key: "ArrowDown", code: "ArrowDown" },
  left: { key: "ArrowLeft", code: "ArrowLeft" },
  right: { key: "ArrowRight", code: "ArrowRight" },
  home: { key: "Home", code: "Home" },
  end: { key: "End", code: "End" },
  pageup: { key: "PageUp", code: "PageUp" },
  pagedown: { key: "PageDown", code: "PageDown" },
  f1: { key: "F1", code: "F1" },
  f2: { key: "F2", code: "F2" },
  f3: { key: "F3", code: "F3" },
  f4: { key: "F4", code: "F4" },
  f5: { key: "F5", code: "F5" },
  f6: { key: "F6", code: "F6" },
  f7: { key: "F7", code: "F7" },
  f8: { key: "F8", code: "F8" },
  f9: { key: "F9", code: "F9" },
  f10: { key: "F10", code: "F10" },
  f11: { key: "F11", code: "F11" },
  f12: { key: "F12", code: "F12" },
};

// Modifier key mapping
const MODIFIER_MAP = {
  control: 2,
  ctrl: 2,
  alt: 1,
  shift: 8,
  meta: 4,
  cmd: 4,
  command: 4,
};

function usage() {
  console.log("Usage: press-key.js <key> [--times <n>]");
  console.log("\nSimulate keyboard key presses.");
  console.log("\nKey format:");
  console.log("  Single key: Enter, Escape, Tab, Space, a, 1, etc.");
  console.log("  With modifiers: Control+C, Shift+Enter, Alt+Tab, Meta+A");
  console.log("  Multiple modifiers: Control+Shift+S");
  console.log("\nOptions:");
  console.log("  --times <n>   Press key n times (default: 1)");
  console.log("\nSupported keys:");
  console.log("  Enter, Escape/Esc, Tab, Backspace, Delete, Space");
  console.log("  ArrowUp/Up, ArrowDown/Down, ArrowLeft/Left, ArrowRight/Right");
  console.log("  Home, End, PageUp, PageDown, F1-F12");
  console.log("  Any single character: a-z, 0-9, etc.");
  console.log("\nModifiers:");
  console.log("  Control/Ctrl, Alt, Shift, Meta/Cmd/Command");
  console.log("\nExamples:");
  console.log("  press-key.js Enter");
  console.log("  press-key.js Escape");
  console.log('  press-key.js "Control+C"');
  console.log('  press-key.js "Control+Shift+S"');
  console.log("  press-key.js Tab --times 3");
  process.exit(1);
}

// Parse arguments
const args = process.argv.slice(2);
if (args.length === 0) usage();

let keySpec = null;
let times = 1;

for (let i = 0; i < args.length; i++) {
  if (args[i] === "--times") {
    times = parseInt(args[++i], 10);
    if (isNaN(times) || times < 1) {
      console.error("✗ Invalid times value");
      process.exit(1);
    }
  } else if (!keySpec) {
    keySpec = args[i];
  }
}

if (!keySpec) {
  console.error("✗ No key specified");
  usage();
}

// Parse key specification (e.g., "Control+Shift+S")
function parseKeySpec(spec) {
  const parts = spec.split("+");
  let modifiers = 0;
  let keyName = null;

  for (const part of parts) {
    const lowerPart = part.toLowerCase();
    if (MODIFIER_MAP[lowerPart] !== undefined) {
      modifiers |= MODIFIER_MAP[lowerPart];
    } else {
      keyName = part;
    }
  }

  if (!keyName) {
    return { error: "No key specified in key combination" };
  }

  // Look up key in map or use as-is
  const lowerKey = keyName.toLowerCase();
  if (KEY_MAP[lowerKey]) {
    return { ...KEY_MAP[lowerKey], modifiers };
  }

  // Single character key
  if (keyName.length === 1) {
    const char = keyName;
    const code = char.match(/[a-zA-Z]/)
      ? `Key${char.toUpperCase()}`
      : char.match(/[0-9]/)
        ? `Digit${char}`
        : null;
    return { key: char, code: code || char, modifiers };
  }

  return { error: `Unknown key: ${keyName}` };
}

const keyInfo = parseKeySpec(keySpec);
if (keyInfo.error) {
  console.error("✗", keyInfo.error);
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

  log("getting pages...");
  const pages = await cdp.getPages();
  const page = pages.at(-1);

  if (!page) {
    console.error("✗ No active tab found");
    process.exit(1);
  }

  log("attaching to page...");
  const sessionId = await cdp.attachToPage(page.targetId);

  // Press key(s)
  for (let i = 0; i < times; i++) {
    log(`pressing ${keySpec} (${i + 1}/${times})...`);

    // Key down
    await cdp.send(
      "Input.dispatchKeyEvent",
      {
        type: "keyDown",
        key: keyInfo.key,
        code: keyInfo.code,
        modifiers: keyInfo.modifiers,
      },
      sessionId
    );

    // Key up
    await cdp.send(
      "Input.dispatchKeyEvent",
      {
        type: "keyUp",
        key: keyInfo.key,
        code: keyInfo.code,
        modifiers: keyInfo.modifiers,
      },
      sessionId
    );

    if (times > 1 && i < times - 1) {
      await new Promise((r) => setTimeout(r, 50));
    }
  }

  const timesStr = times > 1 ? ` (${times}x)` : "";
  console.log(`✓ Pressed ${keySpec}${timesStr}`);

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
