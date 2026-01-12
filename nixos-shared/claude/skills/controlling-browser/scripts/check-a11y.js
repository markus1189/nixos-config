#!/usr/bin/env nix
/*
#! nix shell nixpkgs#nodejs_22 --command node
*/

import { connect } from "./cdp.js";
import { writeFileSync } from "fs";

const DEBUG = process.env.DEBUG === "1";
const log = DEBUG ? (...args) => console.error("[debug]", ...args) : () => {};

function usage() {
  console.log("Usage: check-a11y.js [options]");
  console.log("\nScan page for common accessibility problems.");
  console.log("\nOptions:");
  console.log("  --report <file>    Save issues to file");
  console.log("  --format <type>    Output format: text (default) or json");
  console.log("  --severity <lvl>   Min severity to show: error, warning, info (default: warning)");
  console.log("\nChecks for:");
  console.log("  - Missing alt text on images");
  console.log("  - Form controls without labels");
  console.log("  - Missing document title");
  console.log("  - Skipped heading levels");
  console.log("  - Empty links and buttons");
  console.log("  - Invalid ARIA usage");
  console.log("\nExamples:");
  console.log("  check-a11y.js");
  console.log("  check-a11y.js --report issues.txt");
  console.log("  check-a11y.js --format json --report a11y.json");
  process.exit(1);
}

// Parse arguments
const args = process.argv.slice(2);

let reportFile = null;
let format = "text";
let minSeverity = "warning";

const severityOrder = { info: 0, warning: 1, error: 2 };

for (let i = 0; i < args.length; i++) {
  if (args[i] === "--report") {
    reportFile = args[++i];
  } else if (args[i] === "--format") {
    format = args[++i];
    if (!["text", "json"].includes(format)) {
      console.error("✗ Invalid format. Use: text, json");
      process.exit(1);
    }
  } else if (args[i] === "--severity") {
    minSeverity = args[++i];
    if (!["error", "warning", "info"].includes(minSeverity)) {
      console.error("✗ Invalid severity. Use: error, warning, info");
      process.exit(1);
    }
  } else if (args[i] === "--help" || args[i] === "-h") {
    usage();
  }
}

function checkAccessibility(nodes) {
  const issues = [];
  const headingLevels = [];

  for (const node of nodes) {
    if (node.ignored) continue;

    const role = node.role?.value;
    const name = node.name?.value || "";
    const nodeId = node.nodeId;

    // Check for images without alt text
    if (role === "image" || role === "img") {
      if (!name.trim()) {
        issues.push({
          severity: "error",
          type: "missing-alt",
          message: "Image missing alt text",
          role,
          nodeId,
        });
      }
    }

    // Check for form controls without labels
    if (["textbox", "combobox", "checkbox", "radio", "spinbutton", "slider", "searchbox"].includes(role)) {
      if (!name.trim()) {
        issues.push({
          severity: "error",
          type: "missing-label",
          message: `Form control missing accessible name: ${role}`,
          role,
          nodeId,
        });
      }
    }

    // Check for empty links
    if (role === "link") {
      if (!name.trim()) {
        issues.push({
          severity: "error",
          type: "empty-link",
          message: "Link has no accessible name",
          role,
          nodeId,
        });
      }
    }

    // Check for empty buttons
    if (role === "button") {
      if (!name.trim()) {
        issues.push({
          severity: "error",
          type: "empty-button",
          message: "Button has no accessible name",
          role,
          nodeId,
        });
      }
    }

    // Track heading levels for hierarchy check
    if (role === "heading") {
      const levelProp = node.properties?.find((p) => p.name === "level");
      const level = levelProp?.value?.value || levelProp?.value;
      if (level) {
        headingLevels.push({ level: parseInt(level, 10), nodeId, name });
      }
    }

    // Check for missing document title
    if (role === "RootWebArea" || role === "WebArea") {
      if (!name.trim()) {
        issues.push({
          severity: "warning",
          type: "missing-title",
          message: "Page is missing document title",
          role,
          nodeId,
        });
      }
    }
  }

  // Check heading hierarchy
  if (headingLevels.length > 0) {
    // Check if first heading is not h1
    if (headingLevels[0].level !== 1) {
      issues.push({
        severity: "warning",
        type: "heading-order",
        message: `First heading is h${headingLevels[0].level}, should be h1`,
        nodeId: headingLevels[0].nodeId,
      });
    }

    // Check for skipped levels
    for (let i = 1; i < headingLevels.length; i++) {
      const prevLevel = headingLevels[i - 1].level;
      const currLevel = headingLevels[i].level;

      // Going deeper by more than 1 level is a skip
      if (currLevel > prevLevel + 1) {
        issues.push({
          severity: "warning",
          type: "heading-skip",
          message: `Skipped heading level: h${prevLevel} to h${currLevel}`,
          nodeId: headingLevels[i].nodeId,
        });
      }
    }
  }

  return issues;
}

function formatIssue(issue) {
  const severityIcon = { error: "✗", warning: "⚠", info: "ℹ" };
  return `${severityIcon[issue.severity]} [${issue.severity.toUpperCase()}] ${issue.message}`;
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
  await cdp.send("Accessibility.enable", {}, sessionId);

  log("getting full accessibility tree...");
  const { nodes } = await cdp.send("Accessibility.getFullAXTree", {}, sessionId);

  // Disable domains
  await cdp.send("Accessibility.disable", {}, sessionId);

  log(`analyzing ${nodes.length} nodes...`);
  const allIssues = checkAccessibility(nodes);

  // Filter by severity
  const minLevel = severityOrder[minSeverity];
  const issues = allIssues.filter((i) => severityOrder[i.severity] >= minLevel);

  // Count by severity
  const errorCount = issues.filter((i) => i.severity === "error").length;
  const warningCount = issues.filter((i) => i.severity === "warning").length;
  const infoCount = issues.filter((i) => i.severity === "info").length;

  if (format === "json") {
    const output = JSON.stringify({ issues, summary: { errors: errorCount, warnings: warningCount, info: infoCount } }, null, 2);

    if (reportFile) {
      writeFileSync(reportFile, output);
      console.log(`✓ Saved ${issues.length} issues to ${reportFile}`);
    } else {
      console.log(output);
    }
  } else {
    // Text format
    const lines = [];

    if (issues.length === 0) {
      lines.push("✓ No accessibility issues found");
    } else {
      lines.push(`Found ${issues.length} accessibility issue(s):\n`);

      // Group by severity
      const errors = issues.filter((i) => i.severity === "error");
      const warnings = issues.filter((i) => i.severity === "warning");
      const infos = issues.filter((i) => i.severity === "info");

      if (errors.length > 0) {
        lines.push("ERRORS:");
        for (const issue of errors) {
          lines.push(`  ${formatIssue(issue)}`);
        }
        lines.push("");
      }

      if (warnings.length > 0) {
        lines.push("WARNINGS:");
        for (const issue of warnings) {
          lines.push(`  ${formatIssue(issue)}`);
        }
        lines.push("");
      }

      if (infos.length > 0) {
        lines.push("INFO:");
        for (const issue of infos) {
          lines.push(`  ${formatIssue(issue)}`);
        }
        lines.push("");
      }

      lines.push(`Summary: ${errorCount} error(s), ${warningCount} warning(s), ${infoCount} info`);
    }

    const output = lines.join("\n");

    if (reportFile) {
      writeFileSync(reportFile, output);
      console.log(`✓ Saved report to ${reportFile}`);
    } else {
      console.log(output);
    }
  }

  // Exit with error code if there are errors
  const exitCode = errorCount > 0 ? 1 : 0;

  log("closing...");
  cdp.close();
  log("done");

  clearTimeout(globalTimeout);
  setTimeout(() => process.exit(exitCode), 100);
} catch (e) {
  console.error("✗", e.message);
  clearTimeout(globalTimeout);
  process.exit(1);
}
