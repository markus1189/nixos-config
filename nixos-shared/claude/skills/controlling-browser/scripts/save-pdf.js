#!/usr/bin/env nix
/*
#! nix shell nixpkgs#nodejs_22 --command node
*/

import { writeFileSync } from "node:fs";
import { connect } from "./cdp.js";

const DEBUG = process.env.DEBUG === "1";
const log = DEBUG ? (...args) => console.error("[debug]", ...args) : () => {};

const args = process.argv.slice(2);
const outputFile = args.find((a) => !a.startsWith("--"));

if (!outputFile) {
  console.log("Usage: save-pdf.js <output.pdf> [options]");
  console.log("\nOptions:");
  console.log("  --landscape        Landscape orientation");
  console.log("  --no-background    Skip background graphics (saves ink)");
  console.log("  --paper <size>     Paper size: letter, legal, a4, a3");
  console.log("  --margins <inches> All margins in inches");
  console.log("  --scale <factor>   Scale factor (0.1-2.0)");
  console.log("  --pages <ranges>   Page ranges: \"1-3,5\"");
  console.log("  --header <html>    Custom header template");
  console.log("  --footer <html>    Custom footer template");
  console.log("\nExamples:");
  console.log("  save-pdf.js output.pdf");
  console.log("  save-pdf.js output.pdf --landscape --no-background");
  console.log("  save-pdf.js output.pdf --paper a4 --margins 0.5");
  process.exit(1);
}

// Parse options
const options = {
  landscape: args.includes("--landscape"),
  printBackground: !args.includes("--no-background"),
  displayHeaderFooter: false,
  scale: 1.0,
  paperWidth: 8.5,
  paperHeight: 11,
  marginTop: 0.4,
  marginBottom: 0.4,
  marginLeft: 0.4,
  marginRight: 0.4,
};

// Custom margins
const marginIdx = args.indexOf("--margins");
if (marginIdx >= 0) {
  const margin = parseFloat(args[marginIdx + 1]);
  options.marginTop = margin;
  options.marginBottom = margin;
  options.marginLeft = margin;
  options.marginRight = margin;
}

// Paper sizes
const paperIdx = args.indexOf("--paper");
if (paperIdx >= 0) {
  const paper = args[paperIdx + 1].toLowerCase();
  const sizes = {
    letter: { width: 8.5, height: 11 },
    legal: { width: 8.5, height: 14 },
    a4: { width: 8.27, height: 11.7 },
    a3: { width: 11.7, height: 16.5 },
  };
  if (sizes[paper]) {
    options.paperWidth = sizes[paper].width;
    options.paperHeight = sizes[paper].height;
  }
}

// Scale
const scaleIdx = args.indexOf("--scale");
if (scaleIdx >= 0) {
  options.scale = parseFloat(args[scaleIdx + 1]);
}

// Page ranges
const pagesIdx = args.indexOf("--pages");
if (pagesIdx >= 0) {
  options.pageRanges = args[pagesIdx + 1];
}

// Header/footer
const headerIdx = args.indexOf("--header");
if (headerIdx >= 0) {
  options.displayHeaderFooter = true;
  options.headerTemplate = args[headerIdx + 1];
  // Footer must be set if header is set, even if empty
  if (!options.footerTemplate) {
    options.footerTemplate = "<span></span>";
  }
}

const footerIdx = args.indexOf("--footer");
if (footerIdx >= 0) {
  options.displayHeaderFooter = true;
  options.footerTemplate = args[footerIdx + 1];
  // Header must be set if footer is set, even if empty
  if (!options.headerTemplate) {
    options.headerTemplate = "<span></span>";
  }
}

// Global timeout - PDF generation can take a while for large pages
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

  log("generating PDF with options:", options);
  const { data } = await cdp.send("Page.printToPDF", options, sessionId, 30000);

  log("writing to file...");
  const buffer = Buffer.from(data, "base64");
  writeFileSync(outputFile, buffer);

  console.log(outputFile);

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
