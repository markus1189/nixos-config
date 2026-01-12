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
  console.log("Usage: cookies.js <command> [options]");
  console.log("\nCommands:");
  console.log("  list                     List all cookies");
  console.log("  list --url <url>         List cookies for specific URL");
  console.log("  export <file>            Export cookies to JSON file");
  console.log("  export <file> --url <url> Export cookies for specific URL");
  console.log("  import <file>            Import cookies from JSON file");
  console.log("  clear                    Clear all cookies");
  console.log("  clear --url <url>        Clear cookies for specific URL");
  console.log("\nExamples:");
  console.log("  cookies.js list");
  console.log("  cookies.js export session.json");
  console.log("  cookies.js import session.json");
  console.log("  cookies.js clear --url https://example.com");
  process.exit(1);
}

if (!command || command === "--help" || command === "-h") {
  usage();
}

function getArg(flag) {
  const idx = args.indexOf(flag);
  return idx >= 0 ? args[idx + 1] : null;
}

const url = getArg("--url");

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

  // Enable Network domain for cookie operations
  await cdp.send("Network.enable", {}, sessionId);

  switch (command) {
    case "list": {
      const params = url ? { urls: [url] } : {};
      const { cookies } = await cdp.send("Network.getCookies", params, sessionId);

      if (cookies.length === 0) {
        console.log("No cookies found");
      } else {
        // Table format
        console.log("Name".padEnd(25) + "Domain".padEnd(30) + "Secure  HttpOnly  Expires");
        console.log("-".repeat(85));
        for (const c of cookies) {
          const expires = c.expires === -1 ? "Session" : new Date(c.expires * 1000).toISOString().slice(0, 10);
          console.log(
            c.name.slice(0, 24).padEnd(25) +
            c.domain.slice(0, 29).padEnd(30) +
            (c.secure ? "Yes" : "No").padEnd(8) +
            (c.httpOnly ? "Yes" : "No").padEnd(10) +
            expires
          );
        }
        console.error(`\n${cookies.length} cookie(s)`);
      }
      break;
    }

    case "export": {
      const file = args[1];
      if (!file || file.startsWith("--")) {
        console.error("✗ Export requires output filename");
        process.exit(1);
      }
      const params = url ? { urls: [url] } : {};
      const { cookies } = await cdp.send("Network.getCookies", params, sessionId);
      writeFileSync(file, JSON.stringify(cookies, null, 2));
      console.log(`✓ Exported ${cookies.length} cookie(s) to ${file}`);
      break;
    }

    case "import": {
      const file = args[1];
      if (!file || file.startsWith("--")) {
        console.error("✗ Import requires input filename");
        process.exit(1);
      }
      const cookies = JSON.parse(readFileSync(file, "utf-8"));
      let imported = 0;
      let failed = 0;

      for (const cookie of cookies) {
        try {
          // Network.setCookie expects slightly different params
          const params = {
            name: cookie.name,
            value: cookie.value,
            domain: cookie.domain,
            path: cookie.path || "/",
            secure: cookie.secure || false,
            httpOnly: cookie.httpOnly || false,
            sameSite: cookie.sameSite || "Lax",
          };
          if (cookie.expires && cookie.expires !== -1) {
            params.expires = cookie.expires;
          }

          const { success } = await cdp.send("Network.setCookie", params, sessionId);
          if (success) {
            imported++;
          } else {
            failed++;
            log(`Failed to set cookie: ${cookie.name}`);
          }
        } catch (e) {
          failed++;
          log(`Error setting cookie ${cookie.name}: ${e.message}`);
        }
      }

      console.log(`✓ Imported ${imported} cookie(s)${failed ? `, ${failed} failed` : ""}`);
      break;
    }

    case "clear": {
      if (url) {
        // Get cookies for URL first, then delete each
        const { cookies } = await cdp.send("Network.getCookies", { urls: [url] }, sessionId);
        for (const cookie of cookies) {
          await cdp.send("Network.deleteCookies", {
            name: cookie.name,
            domain: cookie.domain,
            path: cookie.path,
          }, sessionId);
        }
        console.log(`✓ Cleared ${cookies.length} cookie(s) for ${url}`);
      } else {
        await cdp.send("Network.clearBrowserCookies", {}, sessionId);
        console.log("✓ Cleared all cookies");
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
