# Technical Architecture

This document describes the technical design of the browser skill scripts.

## Nix Shebang Explanation

All scripts use `#!/usr/bin/env nix` shebangs to achieve zero-installation portability on NixOS systems. This eliminates npm, package.json, and node_modules entirely while ensuring deterministic, reproducible execution.

### Shebang Pattern

Every script follows this structure:

```javascript
#!/usr/bin/env nix
/*
#! nix shell nixpkgs#nodejs_22 --command node
*/
```

The first line invokes Nix, and the comment block (which Node.js ignores) contains the actual Nix command. This two-line pattern allows the script to be executable while passing the proper arguments to Nix.

For scripts that need browser binaries (like `start.js`):

```javascript
#!/usr/bin/env nix
/*
#! nix shell nixpkgs#nodejs_22 nixpkgs#rsync nixpkgs#chromium --impure --command node
*/
```

### How It Works

1. When you execute `./screenshot.js`, the kernel reads the shebang and runs `/usr/bin/env nix`
2. Nix parses the script, extracting the `#! nix shell` directives from comments
3. Nix fetches Node.js 22 from nixpkgs (cached after first run)
4. Nix creates an isolated environment with Node.js in PATH
5. Nix executes `node screenshot.js` in that environment
6. The script runs with deterministic dependencies

### Benefits

**Zero installation**: No `npm install`, no waiting for package resolution, no disk space for node_modules.

**No version conflicts**: Each script declares exactly which Node version it needs. Multiple scripts can use different versions without conflicts.

**Deterministic execution**: The same script always runs with the same Node.js version across all machines, eliminating "works on my machine" issues.

**Atomic updates**: Updating Node.js version is a one-line change in the shebang. All scripts continue working with their declared versions.

**System integration**: Scripts automatically use the system's Nix cache, sharing Node.js binaries with other tools.

### First-Run Behavior

The first time you run a script, Nix downloads Node.js (~50MB). Subsequent runs start instantly because Node.js is cached in `/nix/store/`. The download is automatic and requires no user intervention.

## Vendored Dependencies

### What's Vendored

The `scripts/vendor/` directory contains:

- **Readability.js** (90KB): Mozilla's article extraction library for readable content
- **ws** (WebSocket library): Minimal WebSocket client for Chrome DevTools Protocol

### Why Vendoring Instead of npm

**Nix shebang compatibility**: npm requires package.json, node_modules, and installation steps. Vendoring keeps scripts self-contained and executable without setup.

**Startup speed**: No need to traverse node_modules or resolve dependencies. Import paths are direct and fast.

**Simplicity**: Two files instead of hundreds. Easy to audit, update, and understand.

**Reliability**: Dependencies can't disappear from npm registry. Scripts continue working indefinitely.

### How to Update Vendored Dependencies

**Readability.js**:
```bash
cd nixos-shared/claude/skills/controlling-browser/scripts/vendor
curl -o Readability.js https://raw.githubusercontent.com/mozilla/readability/main/Readability.js
```

**ws library**:
```bash
cd /tmp
npm install ws
cp -r node_modules/ws nixos-shared/claude/skills/controlling-browser/scripts/vendor/
```

Verify the update by running tests and checking that all scripts still work correctly.

## Browser Auto-detection

The `start.js` script automatically detects available browsers on NixOS using a priority-based fallback system.

### Detection Logic

```javascript
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

  throw new Error("No Chrome/Chromium browser found.");
}
```

### NixOS-Specific Paths

NixOS uses `/run/current-system/sw/bin/` for system-level binaries. This directory contains symlinks to the current generation's packages, making it the canonical location for browser executables.

Profile paths use standard XDG locations: `~/.config/chromium` or `~/.config/google-chrome`.

### Fallback Logic

1. Check for Chromium at NixOS system path
2. Check for Google Chrome at NixOS system path
3. Fall back to searching PATH for `chromium` command
4. Error if no browser found

This ensures the script works on both standard NixOS systems and environments where browsers are installed via `home-manager` or `nix-shell`.

## Debug Mode

All scripts support a `DEBUG=1` environment variable for troubleshooting.

### Using Debug Mode

```bash
DEBUG=1 ./scripts/screenshot.js
DEBUG=1 ./scripts/nav.js https://example.com
```

### What Gets Logged

Debug mode adds detailed logging at each step:

```javascript
const DEBUG = process.env.DEBUG === "1";
const log = DEBUG ? (...args) => console.error("[debug]", ...args) : () => {};

log("connecting...");
const cdp = await connect(5000);

log("getting pages...");
const pages = await cdp.getPages();

log("attaching to page...");
const sessionId = await cdp.attachToPage(page.targetId);

log("taking screenshot...");
const data = await cdp.screenshot(sessionId);
```

Output goes to stderr to avoid polluting stdout (which contains the actual result).

### Debugging CDP Issues

Common scenarios where debug mode helps:

**Connection failures**: Check if Chrome is running with `--remote-debugging-port=9222`
```bash
DEBUG=1 ./scripts/screenshot.js
# Output: [debug] connecting...
# Error: Connection timeout - is Chrome running with --remote-debugging-port=9222?
```

**Timeout issues**: See which CDP operation is slow
```bash
DEBUG=1 ./scripts/nav.js https://slow-site.com
# Output: [debug] connecting...
# [debug] getting pages...
# [debug] attaching to page...
# [debug] navigating... (hangs here = slow site)
```

**Empty results**: Verify script reaches expected steps
```bash
DEBUG=1 ./scripts/readable.js
# Output: [debug] connecting...
# [debug] getting pages...
# [debug] attaching to page...
# [debug] extracting content...
# [debug] closing...
# (empty output = extraction failed, not connection)
```

**WebSocket errors**: Identify protocol-level failures
```bash
DEBUG=1 ./scripts/eval.js 'document.title'
# Shows exact point where WebSocket communication fails
```

### Performance Analysis

Debug output includes timing information through sequential logging. Compare timestamps (implicit in console output) to identify slow operations and optimize accordingly.
