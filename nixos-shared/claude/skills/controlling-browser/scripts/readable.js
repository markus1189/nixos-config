#!/usr/bin/env nix
/*
#! nix shell nixpkgs#nodejs_22 --command node
*/

import { connect } from "./cdp.js";
import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { dirname, join } from "node:path";

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

const DEBUG = process.env.DEBUG === "1";
const log = DEBUG ? (...args) => console.error("[debug]", ...args) : () => {};

// Parse arguments
const args = process.argv.slice(2);
const format = args[0] || "json";

if (format === "--help" || format === "-h") {
  console.log("Usage: readable.js [format]");
  console.log("\nFormats:");
  console.log("  (none)      Full article object as JSON (default)");
  console.log("  --text      Plain text content only");
  console.log("  --html      Clean HTML content only");
  console.log("  --title     Article title only");
  console.log("  --summary   Article excerpt/summary");
  console.log("\nExamples:");
  console.log("  readable.js              # Get full article as JSON");
  console.log("  readable.js --text       # Get plain text");
  console.log("  readable.js --html       # Get clean HTML");
  process.exit(0);
}

// Validate format
const validFormats = ["json", "--text", "--html", "--title", "--summary"];
if (!validFormats.includes(format)) {
  console.error(`✗ Invalid format: ${format}`);
  console.error(`Valid formats: ${validFormats.join(", ")}`);
  process.exit(1);
}

// Global timeout
const globalTimeout = setTimeout(() => {
  console.error("✗ Global timeout exceeded (45s)");
  process.exit(1);
}, 45000);

try {
  log("loading Readability library...");
  const readabilityCode = readFileSync(
    join(__dirname, "vendor", "Readability.js"),
    "utf8"
  );

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

  log("extracting readable content...");
  const expression = `
    (async () => {
      ${readabilityCode}

      // Parse the document
      const documentClone = document.cloneNode(true);
      const article = new Readability(documentClone, {
        keepClasses: false,
        charThreshold: 500
      }).parse();

      if (!article) {
        throw new Error("Failed to extract readable content - page may not contain an article");
      }

      return article;
    })()
  `;

  const result = await cdp.evaluate(sessionId, expression, 30000);

  log("formatting output...");

  // Output based on format
  switch (format) {
    case "--text":
      console.log(result.textContent);
      break;

    case "--html":
      console.log(result.content);
      break;

    case "--title":
      console.log(result.title);
      break;

    case "--summary":
      console.log(result.excerpt || "No summary available");
      break;

    case "json":
    default:
      console.log(JSON.stringify({
        title: result.title,
        byline: result.byline,
        excerpt: result.excerpt,
        siteName: result.siteName,
        length: result.length,
        textContent: result.textContent,
        content: result.content
      }, null, 2));
      break;
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
