#!/usr/bin/env nix
/*
#! nix shell nixpkgs#nodejs_22 --command node
*/

import { connect } from "./cdp.js";
import { writeFileSync } from "fs";

const DEBUG = process.env.DEBUG === "1";
const log = DEBUG ? (...args) => console.error("[debug]", ...args) : () => {};

function usage() {
  console.log("Usage: get-accessibility.js [options]");
  console.log("\nQuery accessibility tree to inspect ARIA roles, labels, and properties.");
  console.log("\nOptions:");
  console.log("  --selector <css>   Get accessibility info for specific element");
  console.log("  --full-tree        Get complete accessibility tree");
  console.log("  --role <role>      Query by ARIA role (button, heading, link, etc.)");
  console.log("  --name <name>      Query by accessible name");
  console.log("  --output <file>    Save as JSON");
  console.log("  --depth <n>        Max tree depth (default: unlimited)");
  console.log("\nExamples:");
  console.log('  get-accessibility.js --selector "#my-button"');
  console.log("  get-accessibility.js --full-tree");
  console.log("  get-accessibility.js --role button");
  console.log('  get-accessibility.js --name "Submit"');
  console.log("  get-accessibility.js --full-tree --output a11y-tree.json");
  process.exit(1);
}

// Parse arguments
const args = process.argv.slice(2);

let selector = null;
let fullTree = false;
let role = null;
let name = null;
let outputFile = null;
let maxDepth = null;

for (let i = 0; i < args.length; i++) {
  if (args[i] === "--selector") {
    selector = args[++i];
  } else if (args[i] === "--full-tree") {
    fullTree = true;
  } else if (args[i] === "--role") {
    role = args[++i];
  } else if (args[i] === "--name") {
    name = args[++i];
  } else if (args[i] === "--output") {
    outputFile = args[++i];
  } else if (args[i] === "--depth") {
    maxDepth = parseInt(args[++i], 10);
    if (isNaN(maxDepth) || maxDepth < 1) {
      console.error("✗ Invalid depth");
      process.exit(1);
    }
  } else if (args[i] === "--help" || args[i] === "-h") {
    usage();
  }
}

// Default to shallow tree if nothing specified
if (!selector && !fullTree && !role && !name) {
  fullTree = true;
  maxDepth = maxDepth || 3;
}

function formatNode(node, depth = 0) {
  const indent = "  ".repeat(depth);
  const roleStr = node.role?.value || "unknown";
  const nameStr = node.name?.value || "";
  const ignoredStr = node.ignored ? " [IGNORED]" : "";

  let output = `${indent}${roleStr}`;
  if (nameStr) output += `: "${nameStr}"`;
  output += ignoredStr;

  // Show important properties
  if (node.properties && node.properties.length > 0) {
    const props = node.properties
      .filter((p) =>
        [
          "focused",
          "disabled",
          "required",
          "invalid",
          "checked",
          "pressed",
          "expanded",
          "level",
        ].includes(p.name)
      )
      .map((p) => {
        const val = p.value?.value ?? p.value;
        return `${p.name}=${val}`;
      })
      .join(", ");
    if (props) output += ` (${props})`;
  }

  return output;
}

function buildNodeMap(nodes) {
  const map = new Map();
  for (const node of nodes) {
    map.set(node.nodeId, node);
  }
  return map;
}

function printTree(node, nodeMap, depth = 0, maxD = null, showIgnored = false) {
  if (maxD !== null && depth > maxD) return;

  // Only print non-ignored nodes (or all if showIgnored)
  const shouldPrint = !node.ignored || showIgnored;
  const effectiveDepth = shouldPrint ? depth : depth - 1;

  if (shouldPrint) {
    console.log(formatNode(node, depth));
  }

  if (node.childIds) {
    for (const childId of node.childIds) {
      const childNode = nodeMap.get(childId);
      if (childNode) {
        // Always traverse children, but only increment depth for printed nodes
        printTree(childNode, nodeMap, shouldPrint ? depth + 1 : depth, maxD, showIgnored);
      }
    }
  }
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

  let axNodes = [];

  if (selector) {
    // Get accessibility info for specific element
    log(`getting accessibility for selector: ${selector}`);

    // Find the element
    const { root } = await cdp.send("DOM.getDocument", { depth: 0 }, sessionId);
    const { nodeId } = await cdp.send(
      "DOM.querySelector",
      { nodeId: root.nodeId, selector },
      sessionId
    );

    if (!nodeId) {
      console.error(`✗ Element not found: ${selector}`);
      process.exit(1);
    }

    // Get the backend node ID
    const { node } = await cdp.send(
      "DOM.describeNode",
      { nodeId, depth: 0 },
      sessionId
    );

    // Get partial accessibility tree for this node
    const { nodes } = await cdp.send(
      "Accessibility.getPartialAXTree",
      { backendNodeId: node.backendNodeId, fetchRelatives: true },
      sessionId
    );

    axNodes = nodes;
  } else if (role || name) {
    // Query by role or name
    log(`querying by role=${role} name=${name}`);

    // Need to get document for queryAXTree
    const { root } = await cdp.send("DOM.getDocument", { depth: 0 }, sessionId);
    const { node } = await cdp.send(
      "DOM.describeNode",
      { nodeId: root.nodeId },
      sessionId
    );

    const queryParams = { backendNodeId: node.backendNodeId };
    if (role) queryParams.accessibleName = undefined;
    if (name) queryParams.accessibleName = name;
    if (role) queryParams.role = role;

    const { nodes } = await cdp.send("Accessibility.queryAXTree", queryParams, sessionId);
    axNodes = nodes;
  } else {
    // Get full accessibility tree
    log("getting full accessibility tree...");
    const params = {};
    if (maxDepth) params.depth = maxDepth;

    const { nodes } = await cdp.send("Accessibility.getFullAXTree", params, sessionId);
    axNodes = nodes;
  }

  // Disable domains
  await cdp.send("Accessibility.disable", {}, sessionId);
  await cdp.send("DOM.disable", {}, sessionId);

  if (axNodes.length === 0) {
    console.log("No accessible nodes found");
  } else if (outputFile) {
    // Save as JSON
    writeFileSync(outputFile, JSON.stringify({ nodes: axNodes }, null, 2));
    console.log(`✓ Saved ${axNodes.length} nodes to ${outputFile}`);
  } else {
    // Print tree
    const nodeMap = buildNodeMap(axNodes);

    // For query results, print each match
    if (role || name) {
      console.log(`Found ${axNodes.length} matching node(s):\n`);
      for (const node of axNodes) {
        console.log(formatNode(node, 0));
      }
    } else {
      // For tree results, print hierarchically starting from root
      const rootNode = axNodes[0];
      if (rootNode) {
        printTree(rootNode, nodeMap, 0, maxDepth);
      }
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
