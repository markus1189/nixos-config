#!/usr/bin/env nix
/*
#! nix shell nixpkgs#nodejs_22 --command node
*/

import { connect } from "./cdp.js";

const DEBUG = process.env.DEBUG === "1";
const log = DEBUG ? (...args) => console.error("[debug]", ...args) : () => {};

function usage() {
  console.log("Usage: click.js <selector|text> [options]");
  console.log("\nClick an element by CSS selector or text content.");
  console.log("\nOptions:");
  console.log("  --text              Search by text content (exact match)");
  console.log("  --text-contains     Search by partial text match");
  console.log("  --wait <ms>         Wait for element to appear (default: 0)");
  console.log("  --double            Double-click instead of single click");
  console.log("  --wait-after <strategy>  Wait after clicking (navigation)");
  console.log("\nWait Strategies:");
  console.log("  navigation          Wait for page navigation to complete");
  console.log("\nExamples:");
  console.log('  click.js "button[type=submit]"              # CSS selector');
  console.log('  click.js "#login-button"                    # CSS selector');
  console.log('  click.js "Submit" --text                    # Exact text match');
  console.log('  click.js "Warenkorb" --text-contains        # Partial text match');
  console.log('  click.js ".cookie-accept" --wait 2000       # Wait for element');
  console.log('  click.js "tr.row" --double                  # Double-click');
  console.log('  click.js "#submit" --wait-after navigation  # Wait for page load');
  process.exit(1);
}

// Parse arguments
const args = process.argv.slice(2);
if (args.length === 0) usage();

let selector = null;
let searchText = null;
let textMode = null; // 'exact' or 'contains'
let waitTime = 0;
let doubleClick = false;
let waitAfter = null;

for (let i = 0; i < args.length; i++) {
  if (args[i] === "--wait") {
    waitTime = parseInt(args[++i], 10);
    if (isNaN(waitTime)) {
      console.error("✗ Invalid wait time");
      process.exit(1);
    }
  } else if (args[i] === "--double") {
    doubleClick = true;
  } else if (args[i] === "--text") {
    textMode = "exact";
  } else if (args[i] === "--text-contains") {
    textMode = "contains";
  } else if (args[i] === "--wait-after") {
    waitAfter = args[++i];
    if (waitAfter !== "navigation") {
      console.error("✗ Invalid wait-after strategy. Supported: navigation");
      process.exit(1);
    }
  } else if (!selector && !searchText) {
    if (textMode) {
      searchText = args[i];
    } else {
      selector = args[i];
    }
  }
}

// If textMode is set but no text was provided, treat the first arg as text
if (textMode && !searchText && selector) {
  searchText = selector;
  selector = null;
}

if (!selector && !searchText) {
  console.error("✗ No selector or text provided");
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

  // Build element finding expression
  let findExpression;
  let description;

  if (searchText) {
    // Text-based search
    description = `text "${searchText}" (${textMode} match)`;
    const escapedText = searchText.replace(/\\/g, '\\\\').replace(/"/g, '\\"');

    if (textMode === "exact") {
      findExpression = `(() => {
        const elements = Array.from(document.querySelectorAll('*'));
        return elements.find(el => {
          const visible = el.offsetParent !== null;
          const text = el.textContent.trim();
          // Check if this element's direct text matches (not including children)
          const childText = Array.from(el.childNodes)
            .filter(n => n.nodeType === Node.TEXT_NODE)
            .map(n => n.textContent.trim())
            .join(' ');
          const matches = text === "${escapedText}" || childText === "${escapedText}";
          return visible && matches;
        });
      })()`;
    } else {
      findExpression = `(() => {
        const elements = Array.from(document.querySelectorAll('*'));
        return elements.find(el => {
          const visible = el.offsetParent !== null;
          const textMatch = el.textContent.includes("${escapedText}");
          return visible && textMatch;
        });
      })()`;
    }
  } else {
    // CSS selector search
    description = `selector "${selector}"`;
    findExpression = `document.querySelector(${JSON.stringify(selector)})`;
  }

  // Wait for element if requested
  if (waitTime > 0) {
    log(`waiting for element (${waitTime}ms max)...`);
    const waitStart = Date.now();
    while (Date.now() - waitStart < waitTime) {
      const exists = await cdp.evaluate(sessionId, `!!${findExpression}`);
      if (exists) break;
      await new Promise((r) => setTimeout(r, 100));
    }
  }

  // Get element coordinates
  log("getting element coordinates...");
  const coords = await cdp.evaluate(
    sessionId,
    `(() => {
      const el = ${findExpression};
      if (!el) return null;
      const rect = el.getBoundingClientRect();
      return { x: rect.left + rect.width / 2, y: rect.top + rect.height / 2 };
    })()`
  );

  if (!coords) {
    console.error(`✗ Element not found: ${description}`);
    process.exit(1);
  }

  log(`clicking at (${coords.x}, ${coords.y})...`);
  const clickCount = doubleClick ? 2 : 1;

  // Setup navigation listener if needed
  let navigationPromise = null;
  if (waitAfter === "navigation") {
    await cdp.send("Page.enable", {}, sessionId);

    navigationPromise = new Promise((resolve) => {
      let navigationStarted = false;
      let loadFired = false;

      const checkComplete = () => {
        if (navigationStarted && loadFired) {
          resolve();
        }
      };

      // Listen for navigation start
      cdp.on("Page.frameStartedLoading", (params, sid) => {
        if (sid === sessionId) {
          navigationStarted = true;
          log("navigation started");
          checkComplete();
        }
      });

      // Listen for page load
      cdp.on("Page.loadEventFired", (params, sid) => {
        if (sid === sessionId) {
          loadFired = true;
          log("page loaded");
          checkComplete();
        }
      });

      // Fallback: if no navigation happens within 1s, assume it's not a navigation click
      setTimeout(() => {
        if (!navigationStarted) {
          log("no navigation detected, assuming non-navigation click");
          resolve();
        }
      }, 1000);

      // Max wait of 30s for navigation
      setTimeout(() => {
        log("navigation timeout");
        resolve();
      }, 30000);
    });
  }

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

  console.log(`✓ Clicked ${description}${doubleClick ? " (double)" : ""}`);

  // Wait for navigation if requested
  if (navigationPromise) {
    log("waiting for navigation...");
    await navigationPromise;
    console.log("✓ Navigation completed");
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
