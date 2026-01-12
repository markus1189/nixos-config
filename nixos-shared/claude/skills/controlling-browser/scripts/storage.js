#!/usr/bin/env nix
/*
#! nix shell nixpkgs#nodejs_22 --command node
*/

import { readFileSync, writeFileSync } from "node:fs";
import { connect } from "./cdp.js";

const DEBUG = process.env.DEBUG === "1";
const log = DEBUG ? (...args) => console.error("[debug]", ...args) : () => {};

const args = process.argv.slice(2);
const command = args[0];

function usage() {
  console.log("Usage: storage.js <command> [options]");
  console.log("\nCommands:");
  console.log("  get <key>                Get localStorage value");
  console.log("  get <key> --session      Get sessionStorage value");
  console.log("  set <key> <value>        Set localStorage value");
  console.log("  set <key> <value> --session  Set sessionStorage value");
  console.log("  list                     List all localStorage items");
  console.log("  list --session           List all sessionStorage items");
  console.log("  export <file>            Export localStorage to JSON");
  console.log("  export <file> --session  Export sessionStorage to JSON");
  console.log("  import <file>            Import localStorage from JSON");
  console.log("  import <file> --session  Import sessionStorage from JSON");
  console.log("  clear                    Clear localStorage");
  console.log("  clear --session          Clear sessionStorage");
  console.log("  clear --all              Clear all storage (cache, storage, cookies)");
  console.log("\nExamples:");
  console.log("  storage.js get auth_token");
  console.log("  storage.js set theme dark");
  console.log("  storage.js list");
  console.log("  storage.js export backup.json");
  process.exit(1);
}

if (!command || command === "--help" || command === "-h") {
  usage();
}

const useSession = args.includes("--session");
const clearAll = args.includes("--all");
const storageType = useSession ? "sessionStorage" : "localStorage";

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

  switch (command) {
    case "get": {
      const key = args[1];
      if (!key || key.startsWith("--")) {
        console.error("✗ Get requires a key");
        process.exit(1);
      }
      const value = await cdp.evaluate(sessionId, `${storageType}.getItem(${JSON.stringify(key)})`);
      if (value === null) {
        console.error(`Key not found: ${key}`);
        process.exit(1);
      }
      console.log(value);
      break;
    }

    case "set": {
      const key = args[1];
      const valueIdx = args.indexOf(key) + 1;
      const value = args[valueIdx];
      if (!key || key.startsWith("--") || !value) {
        console.error("✗ Set requires key and value");
        process.exit(1);
      }
      await cdp.evaluate(sessionId, `${storageType}.setItem(${JSON.stringify(key)}, ${JSON.stringify(value)})`);
      console.log(`✓ Set ${storageType}.${key}`);
      break;
    }

    case "list": {
      const items = await cdp.evaluate(sessionId, `
        (() => {
          const items = {};
          for (let i = 0; i < ${storageType}.length; i++) {
            const key = ${storageType}.key(i);
            items[key] = ${storageType}.getItem(key);
          }
          return items;
        })()
      `);

      const keys = Object.keys(items);
      if (keys.length === 0) {
        console.log(`No items in ${storageType}`);
      } else {
        console.log(`${storageType} (${keys.length} items):`);
        for (const key of keys) {
          const value = items[key];
          const display = value.length > 50 ? value.slice(0, 47) + "..." : value;
          console.log(`  ${key}: ${display}`);
        }
      }
      break;
    }

    case "export": {
      const file = args[1];
      if (!file || file.startsWith("--")) {
        console.error("✗ Export requires output filename");
        process.exit(1);
      }
      const items = await cdp.evaluate(sessionId, `
        (() => {
          const items = {};
          for (let i = 0; i < ${storageType}.length; i++) {
            const key = ${storageType}.key(i);
            items[key] = ${storageType}.getItem(key);
          }
          return items;
        })()
      `);
      writeFileSync(file, JSON.stringify(items, null, 2));
      console.log(`✓ Exported ${Object.keys(items).length} item(s) to ${file}`);
      break;
    }

    case "import": {
      const file = args[1];
      if (!file || file.startsWith("--")) {
        console.error("✗ Import requires input filename");
        process.exit(1);
      }
      const items = JSON.parse(readFileSync(file, "utf-8"));
      const keys = Object.keys(items);

      for (const key of keys) {
        await cdp.evaluate(sessionId, `${storageType}.setItem(${JSON.stringify(key)}, ${JSON.stringify(items[key])})`);
      }
      console.log(`✓ Imported ${keys.length} item(s) to ${storageType}`);
      break;
    }

    case "clear": {
      if (clearAll) {
        // Get origin first
        const origin = await cdp.evaluate(sessionId, "window.origin");
        await cdp.send("Storage.clearDataForOrigin", {
          origin,
          storageTypes: "all",
        }, sessionId);
        console.log(`✓ Cleared all storage for ${origin}`);
      } else {
        await cdp.evaluate(sessionId, `${storageType}.clear()`);
        console.log(`✓ Cleared ${storageType}`);
      }
      break;
    }

    default:
      console.error(`✗ Unknown command: ${command}`);
      usage();
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
