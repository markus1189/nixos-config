#!/usr/bin/env nix
/*
#! nix shell nixpkgs#nodejs_22 --command node
*/

import { connect } from "./cdp.js";

const DEBUG = process.env.DEBUG === "1";
const log = DEBUG ? (...args) => console.error("[debug]", ...args) : () => {};

function usage() {
  console.log("Usage: find-by-label.js <name> [options]");
  console.log("\nFind elements by their accessible name (more reliable than text search).");
  console.log("\nOptions:");
  console.log("  --type <role>      Filter by ARIA role (button, textbox, link, etc.)");
  console.log("  --exact            Require exact match (default: contains)");
  console.log("  --first            Return only first match");
  console.log("  --selector         Output CSS selector if possible");
  console.log("  --click            Click the element (requires --first or single match)");
  console.log("  --wait <ms>        Wait for element before clicking");
  console.log("\nExamples:");
  console.log('  find-by-label.js "Submit"');
  console.log('  find-by-label.js "Email" --type textbox');
  console.log('  find-by-label.js "Sign in" --type button --first');
  console.log('  find-by-label.js "Search" --selector');
  console.log('  find-by-label.js "Submit" --click --first       # Click the element');
  console.log('  find-by-label.js "Login" --type button --click  # Click if unique');
  process.exit(1);
}

// Parse arguments
const args = process.argv.slice(2);
if (args.length === 0) usage();

let searchName = null;
let roleFilter = null;
let exactMatch = false;
let firstOnly = false;
let outputSelector = false;
let clickElement = false;
let waitTime = 0;

for (let i = 0; i < args.length; i++) {
  if (args[i] === "--type") {
    roleFilter = args[++i];
  } else if (args[i] === "--exact") {
    exactMatch = true;
  } else if (args[i] === "--first") {
    firstOnly = true;
  } else if (args[i] === "--selector") {
    outputSelector = true;
  } else if (args[i] === "--click") {
    clickElement = true;
  } else if (args[i] === "--wait") {
    waitTime = parseInt(args[++i], 10);
    if (isNaN(waitTime)) {
      console.error("✗ Invalid wait time");
      process.exit(1);
    }
  } else if (args[i] === "--help" || args[i] === "-h") {
    usage();
  } else if (!searchName) {
    searchName = args[i];
  }
}

if (!searchName) {
  console.error("✗ No search name provided");
  usage();
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

  // Enable required domains
  await cdp.send("DOM.enable", {}, sessionId);
  await cdp.send("Accessibility.enable", {}, sessionId);

  // Wait if requested
  if (waitTime > 0) {
    log(`waiting for element (${waitTime}ms max)...`);
    const waitStart = Date.now();
    let found = false;

    while (Date.now() - waitStart < waitTime && !found) {
      const { nodes } = await cdp.send("Accessibility.getFullAXTree", {}, sessionId);
      for (const node of nodes) {
        if (node.ignored) continue;
        const nodeName = node.name?.value || "";
        const nodeRole = node.role?.value;

        let nameMatches = exactMatch
          ? nodeName === searchName
          : nodeName.toLowerCase().includes(searchName.toLowerCase());

        if (nameMatches && (!roleFilter || nodeRole === roleFilter)) {
          found = true;
          break;
        }
      }
      if (!found) {
        await new Promise((r) => setTimeout(r, 100));
      }
    }

    if (!found) {
      console.error(`✗ Timeout waiting for element with accessible name "${searchName}"${roleFilter ? ` and role ${roleFilter}` : ""}`);
      cdp.close();
      clearTimeout(globalTimeout);
      process.exit(1);
    }
  }

  log("getting full accessibility tree...");
  const { nodes } = await cdp.send("Accessibility.getFullAXTree", {}, sessionId);

  // Search for matching nodes
  const matches = [];

  for (const node of nodes) {
    if (node.ignored) continue;

    const nodeName = node.name?.value || "";
    const nodeRole = node.role?.value;

    // Check name match
    let nameMatches = false;
    if (exactMatch) {
      nameMatches = nodeName === searchName;
    } else {
      nameMatches = nodeName.toLowerCase().includes(searchName.toLowerCase());
    }

    if (!nameMatches) continue;

    // Check role filter
    if (roleFilter && nodeRole !== roleFilter) continue;

    matches.push({
      name: nodeName,
      role: nodeRole,
      nodeId: node.nodeId,
      backendDOMNodeId: node.backendDOMNodeId,
      properties: node.properties,
    });

    if (firstOnly) break;
  }

  if (matches.length === 0) {
    console.error(`✗ No elements found with accessible name "${searchName}"${roleFilter ? ` and role ${roleFilter}` : ""}`);
    cdp.close();
    clearTimeout(globalTimeout);
    process.exit(1);
  }

  // Handle clicking
  if (clickElement) {
    if (matches.length > 1 && !firstOnly) {
      console.error(`✗ Multiple matches found (${matches.length}). Use --first to click the first match.`);
      cdp.close();
      clearTimeout(globalTimeout);
      process.exit(1);
    }

    const targetMatch = matches[0];
    log(`clicking element with backendDOMNodeId: ${targetMatch.backendDOMNodeId}`);

    try {
      // Convert backendDOMNodeId to regular nodeId
      const { nodeIds } = await cdp.send(
        "DOM.pushNodesByBackendIdsToFrontend",
        { backendNodeIds: [targetMatch.backendDOMNodeId] },
        sessionId
      );

      if (!nodeIds || !nodeIds[0]) {
        console.error("✗ Could not resolve element to DOM node");
        process.exit(1);
      }

      const nodeId = nodeIds[0];

      // Get bounding box for the element
      const { model } = await cdp.send("DOM.getBoxModel", { nodeId }, sessionId);

      if (!model || !model.content) {
        console.error("✗ Could not get element bounding box");
        process.exit(1);
      }

      // Calculate center point from content quad [x1, y1, x2, y2, x3, y3, x4, y4]
      const [x1, y1, x2, y2, x3, y3, x4, y4] = model.content;
      const centerX = (x1 + x2 + x3 + x4) / 4;
      const centerY = (y1 + y2 + y3 + y4) / 4;

      log(`clicking at (${centerX}, ${centerY})...`);

      // Mouse down
      await cdp.send(
        "Input.dispatchMouseEvent",
        {
          type: "mousePressed",
          x: centerX,
          y: centerY,
          button: "left",
          clickCount: 1,
        },
        sessionId
      );

      // Mouse up
      await cdp.send(
        "Input.dispatchMouseEvent",
        {
          type: "mouseReleased",
          x: centerX,
          y: centerY,
          button: "left",
          clickCount: 1,
        },
        sessionId
      );

      console.log(`✓ Clicked ${targetMatch.role}: "${targetMatch.name}"`);

      await cdp.send("Accessibility.disable", {}, sessionId);
      await cdp.send("DOM.disable", {}, sessionId);
      cdp.close();
      clearTimeout(globalTimeout);
      setTimeout(() => process.exit(0), 100);
      return;
    } catch (e) {
      console.error("✗ Failed to click element:", e.message);
      cdp.close();
      clearTimeout(globalTimeout);
      process.exit(1);
    }
  }

  // Get CSS selectors if requested
  if (outputSelector) {
    for (const match of matches) {
      if (match.backendDOMNodeId) {
        try {
          // Resolve to DOM node
          const { nodeIds } = await cdp.send(
            "DOM.pushNodesByBackendIdsToFrontend",
            { backendNodeIds: [match.backendDOMNodeId] },
            sessionId
          );

          if (nodeIds && nodeIds[0]) {
            // Try to get a unique selector
            const selector = await cdp.evaluate(
              sessionId,
              `(() => {
                const node = document.evaluate(
                  '//*[@data-a11y-node-id="${match.backendDOMNodeId}"]',
                  document,
                  null,
                  XPathResult.FIRST_ORDERED_NODE_TYPE,
                  null
                ).singleNodeValue;

                // Try various selector strategies
                function getSelector(el) {
                  if (!el || el === document.body) return null;

                  // If it has an ID, use it
                  if (el.id) return '#' + CSS.escape(el.id);

                  // Try unique class combination
                  if (el.classList.length > 0) {
                    const selector = el.tagName.toLowerCase() + '.' + Array.from(el.classList).map(c => CSS.escape(c)).join('.');
                    if (document.querySelectorAll(selector).length === 1) {
                      return selector;
                    }
                  }

                  // Build selector with parent context
                  const parent = getSelector(el.parentElement);
                  if (parent) {
                    const siblings = Array.from(el.parentElement.children).filter(c => c.tagName === el.tagName);
                    if (siblings.length === 1) {
                      return parent + ' > ' + el.tagName.toLowerCase();
                    } else {
                      const index = siblings.indexOf(el) + 1;
                      return parent + ' > ' + el.tagName.toLowerCase() + ':nth-of-type(' + index + ')';
                    }
                  }

                  return el.tagName.toLowerCase();
                }

                return getSelector(document.body);
              })()`
            );

            if (selector) {
              match.selector = selector;
            }
          }
        } catch (e) {
          log(`Could not get selector for node ${match.nodeId}: ${e.message}`);
        }
      }
    }
  }

  // Disable domains
  await cdp.send("Accessibility.disable", {}, sessionId);
  await cdp.send("DOM.disable", {}, sessionId);

  // Output results
  if (firstOnly && matches.length === 1) {
    const match = matches[0];
    if (outputSelector && match.selector) {
      console.log(match.selector);
    } else {
      console.log(`${match.role}: "${match.name}"`);
    }
  } else {
    console.log(`Found ${matches.length} match(es):\n`);
    for (const match of matches) {
      let line = `  ${match.role}: "${match.name}"`;
      if (outputSelector && match.selector) {
        line += ` → ${match.selector}`;
      }
      console.log(line);
    }
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
