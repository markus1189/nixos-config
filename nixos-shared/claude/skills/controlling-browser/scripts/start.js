#!/usr/bin/env nix
/*
#! nix shell nixpkgs#nodejs_22 nixpkgs#rsync nixpkgs#chromium --impure --command node
*/

/**
 * NixOS-adapted Chrome/Chromium starter for CDP
 * Detects available browsers and uses appropriate paths
 */

import { spawn, execSync } from "node:child_process";
import { existsSync } from "node:fs";
import { homedir } from "node:os";

const useProfile = process.argv[2] === "--profile";

if (process.argv[2] && process.argv[2] !== "--profile") {
  console.log("Usage: start.js [--profile]");
  console.log("\nOptions:");
  console.log("  --profile  Copy your default Chrome/Chromium profile (cookies, logins)");
  console.log("\nExamples:");
  console.log("  start.js            # Start with fresh profile");
  console.log("  start.js --profile  # Start with your Chrome/Chromium profile");
  process.exit(1);
}

// Detect available browser and paths
function detectBrowser() {
  const browsers = [
    {
      name: "chromium",
      executable: "/run/current-system/sw/bin/chromium",
      profilePath: `${homedir()}/.config/chromium`,
      processName: "chromium"
    },
    {
      name: "google-chrome",
      executable: "/run/current-system/sw/bin/google-chrome-stable",
      profilePath: `${homedir()}/.config/google-chrome`,
      processName: "chrome"
    },
    {
      name: "chrome-fallback",
      executable: "chromium", // Try from PATH
      profilePath: `${homedir()}/.config/chromium`,
      processName: "chromium"
    }
  ];

  for (const browser of browsers) {
    if (existsSync(browser.executable) || browser.name === "chrome-fallback") {
      return browser;
    }
  }

  throw new Error("No Chrome/Chromium browser found. Install chromium or google-chrome.");
}

const browser = detectBrowser();
console.log(`Using ${browser.name} at ${browser.executable}`);

// Kill existing Chrome/Chromium instances (only skill-related ones)
try {
  // Kill by debug port
  execSync("pkill -f 'chromium.*remote-debugging-port=9222' || true", { stdio: "ignore" });
  execSync("pkill -f 'chrome.*remote-debugging-port=9222' || true", { stdio: "ignore" });
  // Kill by cache directory
  execSync("pkill -f 'chromium.*\\.cache/scraping' || true", { stdio: "ignore" });
  execSync("pkill -f 'chrome.*\\.cache/scraping' || true", { stdio: "ignore" });
} catch {}

// Wait a bit for processes to fully die
await new Promise((r) => setTimeout(r, 1000));

// Setup profile directory
const cacheDir = `${homedir()}/.cache/scraping`;
execSync(`mkdir -p "${cacheDir}"`, { stdio: "ignore" });

// Remove stale lock files that prevent startup
try {
  execSync(`rm -f "${cacheDir}/SingletonLock" "${cacheDir}/SingletonSocket" "${cacheDir}/SingletonCookie"`, { stdio: "ignore" });
} catch {}

if (useProfile) {
  // Sync profile with rsync (much faster on subsequent runs)
  if (existsSync(browser.profilePath)) {
    try {
      execSync(
        `rsync -a --delete "${browser.profilePath}/" "${cacheDir}/"`,
        { stdio: "pipe" }
      );
      console.log(`Copied profile from ${browser.profilePath}`);
    } catch (e) {
      console.error(`Warning: Failed to copy profile: ${e.message}`);
    }
  } else {
    console.warn(`Profile path ${browser.profilePath} not found, starting with fresh profile`);
  }
}

// Start Chrome/Chromium in background (detached so Node can exit)
const args = [
  "--remote-debugging-port=9222",
  `--user-data-dir=${cacheDir}`,
  "--profile-directory=Default",
  "--disable-search-engine-choice-screen",
  "--no-first-run",
  "--disable-features=ProfilePicker",
];

spawn(browser.executable, args, {
  detached: true,
  stdio: "ignore"
}).unref();

// Wait for Chrome to be ready by checking the debugging endpoint
let connected = false;
for (let i = 0; i < 30; i++) {
  try {
    const response = await fetch("http://localhost:9222/json/version");
    if (response.ok) {
      connected = true;
      break;
    }
  } catch {
    await new Promise((r) => setTimeout(r, 500));
  }
}

if (!connected) {
  console.error("✗ Failed to connect to Chrome/Chromium");
  process.exit(1);
}

console.log(
  `✓ ${browser.name} started on :9222${useProfile ? " with your profile" : ""}`
);
