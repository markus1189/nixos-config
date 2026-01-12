---
name: controlling-browser
description: "Remote controls Chrome/Chromium browsers using Chrome DevTools Protocol for web automation. Use when browsing the web, scraping content, taking screenshots, interacting with web forms, dismissing cookie dialogs, or when the user mentions web automation or browser control."
---

# Web Browser Control

Minimal CDP tools for collaborative site exploration. Auto-detects Chromium/Chrome on NixOS with zero setup—all scripts use Nix shebangs for dependency isolation, requiring no manual installation.

## Architecture Overview

**Zero-Setup Execution:** All scripts use Nix shebangs (`#!/usr/bin/env nix`) to automatically provision Node.js 22 and dependencies. No npm install, no version conflicts, no manual setup required.

**Vendored Dependencies:** External libraries (Readability.js for article extraction, WebSocket client) are vendored in `scripts/vendor/` for reliability and speed.

**Browser Auto-detection:** Scripts automatically detect Chrome or Chromium on NixOS, checking standard paths and falling back to PATH if needed.

See [references/architecture.md](references/architecture.md) for technical details on Nix shebangs, dependency management, and debugging with `DEBUG=1`.

## Quick Start

Most common operations with complete integration examples:

### 1. Start Browser
```bash
./scripts/start.js              # Fresh profile
./scripts/start.js --profile    # Copy your profile (cookies, logins)
```
Starts browser on `:9222` with remote debugging enabled.

### 2. Navigate
```bash
./scripts/nav.js https://example.com
./scripts/nav.js https://example.com --new  # Open in new tab
```
Navigate to URL. Returns when page begins loading.

### 3. Take Screenshot
```bash
SCREENSHOT=$(./scripts/screenshot.js)
# Use Read tool on $SCREENSHOT to view the image
# Then describe what you see to the user
```
Captures current page state. Essential for visual verification and debugging.

### 4. Extract Content
```bash
# Get clean article text
./scripts/readable.js --text

# Get structured article data
./scripts/readable.js > article.json
# Use Read tool on article.json to analyze
```
Uses Mozilla's Readability algorithm to extract main article content, removing ads and navigation.

### 5. Evaluate JavaScript
```bash
./scripts/eval.js 'document.title'
./scripts/eval.js 'document.querySelectorAll("a").length'

# Capture result for processing
TITLE=$(./scripts/eval.js 'document.title')
echo "Page title: $TITLE"
```
Execute JavaScript in active tab (async context). Use for custom extraction logic.

## Core Command Categories

Quick reference to all available commands. See [references/commands.md](references/commands.md) for complete documentation.

| Category | Commands | Description |
|----------|----------|-------------|
| **Navigation & Control** | `start`, `nav` | Launch browser, navigate to URLs |
| **Screenshots & PDFs** | `screenshot`, `save-pdf` | Capture visual state, generate PDFs |
| **Content Extraction** | `readable`, `eval`, `pick` | Extract text, execute JS, select elements interactively |
| **Element Interaction** | `click`, `type-text`, `fill-form`, `press-key`, `scroll`, `hover` | Manipulate page elements |
| **Dynamic Content** | `wait-for` | Wait for elements to appear/disappear |
| **Cookies & Storage** | `cookies`, `storage`, `dismiss-cookies` | Manage cookies, localStorage, sessionStorage |
| **Debugging** | `capture-logs`, `watch-errors` | Monitor console output and JavaScript errors |
| **Network Control** | `set-headers`, `block-requests`, `mock-response` | Intercept and modify HTTP traffic |
| **Accessibility** | `get-accessibility`, `check-a11y`, `find-by-label` | Inspect accessibility tree, audit issues |

## Common Workflows

### 1. Basic Web Scraping
```bash
# Navigate and wait for page to load
./scripts/nav.js https://example.com/article
./scripts/wait-for.js --selector "article" --visible

# Extract content
CONTENT=$(./scripts/readable.js --text)
echo "$CONTENT"

# Take screenshot for verification
SCREENSHOT=$(./scripts/screenshot.js)
# Use Read tool to verify visual state
```

### 2. Form Submission
```bash
# Navigate to form
./scripts/nav.js https://example.com/contact

# Dismiss cookie dialog if present
sleep 2  # Allow time for dialog to appear
./scripts/dismiss-cookies.js

# Wait for form to load
./scripts/wait-for.js --selector "#contact-form" --visible

# Fill and submit
./scripts/fill-form.js \
  --field "#name" --value "John Doe" \
  --field "#email" --value "john@example.com" \
  --field "#message" --value "Test message" \
  --submit "button[type=submit]"

# Verify submission
./scripts/wait-for.js --text "Thank you" --timeout 10000
SCREENSHOT=$(./scripts/screenshot.js)
# Use Read tool to confirm success message
```

### 3. SPA Navigation
```bash
# Navigate to single-page app
./scripts/nav.js https://app.example.com

# Wait for app to initialize
./scripts/wait-for.js --selector "#app" --visible

# Click to load more content
./scripts/click.js "#load-more"

# Wait for loading to complete
./scripts/wait-for.js --selector ".loading" --disappear

# Extract results
./scripts/eval.js 'Array.from(document.querySelectorAll(".item")).map(el => el.textContent)'
```

### 4. Cookie Management
```bash
# Navigate and establish session
./scripts/nav.js https://app.example.com
# ... perform login ...

# Export session for later reuse
./scripts/cookies.js export session.json

# Later, import session in new browser instance
./scripts/start.js
./scripts/nav.js https://app.example.com
./scripts/cookies.js import session.json

# Verify session restored
./scripts/eval.js 'document.cookie.includes("session")'
```

### 5. Visual Debugging
```bash
# Take screenshot before action
BEFORE=$(./scripts/screenshot.js)

# Perform action
./scripts/click.js "#button"
sleep 1

# Take screenshot after action
AFTER=$(./scripts/screenshot.js)

# Use Read tool to compare both screenshots
# Analyze what changed and report findings
```

See [references/workflows.md](references/workflows.md) for advanced patterns including:
- Login flows with session export
- Infinite scroll extraction
- API testing with mocked responses
- Accessibility audits
- Error recovery patterns with retries

## Decision Guide

### Content Extraction

**Use `readable.js` when:**
- Extracting article content (blogs, news, documentation)
- Need clean text without ads/navigation/boilerplate
- Processing content for LLM analysis
- Example: "Extract the main article from this blog post"

**Use `eval.js` when:**
- Need specific data from page structure
- Complex extraction logic required
- Custom JavaScript manipulation needed
- Example: "Get all product prices from this listing page"

**Use `screenshot.js` when:**
- Visual layout matters for understanding
- Need to see current page state
- Debugging visual issues
- Example: "Show me what the page looks like now"

### Element Identification

**Use `pick.js` when:**
- Selector is unknown or complex
- User needs to identify element visually
- Exploring unfamiliar page structure
- Example: "I need to click something but don't know the selector"

**Use CSS selectors when:**
- Element structure is known/documented
- Automating repeated tasks
- Need faster execution
- Example: `./scripts/click.js "#submit-button"`

**Use `find-by-label.js` when:**
- Accessible name is known but selector isn't
- Writing accessibility-friendly automation
- Selector might change but label won't
- Example: "Find the 'Submit' button by its label"

### Debugging Approaches

**Use `screenshot.js` when:**
- Need visual confirmation of state
- Checking layout or styling issues
- Quick verification of page state
- Example: "Take screenshot to see what went wrong"

**Use `capture-logs.js` when:**
- JavaScript errors suspected
- Need complete console history
- Performance debugging required
- Example: "Capture all logs during page load"

**Use `watch-errors.js` when:**
- Real-time error monitoring needed
- Long-running session debugging
- Catching intermittent async issues
- Example: "Watch for errors while I interact with the page"

## Specialized Features

For detailed documentation, see the reference files:

**Complete Command Reference:** [references/commands.md](references/commands.md)
- Full documentation for all 24 commands
- Detailed options and parameters
- Integration examples for each command
- Grep-searchable with pattern: `## CommandName`

**Advanced Workflows:** [references/workflows.md](references/workflows.md)
- Multi-step automation patterns
- Validation and verification strategies
- Error handling and retry patterns
- Real-world examples (login, infinite scroll, API testing)

**Integration Patterns:** [references/integration.md](references/integration.md)
- How to use script outputs with Claude tools (Bash + Read)
- Screenshot workflows and visual analysis
- JSON processing and data extraction
- Pipeline composition and error handling

**Architecture & Debugging:** [references/architecture.md](references/architecture.md)
- Nix shebang technical details
- Vendored dependency management
- Browser auto-detection logic
- Debug mode (`DEBUG=1`) usage

**Troubleshooting:** [references/troubleshooting.md](references/troubleshooting.md)
- Common errors and solutions
- Recovery workflows
- Browser cleanup procedures
- Element not found, timeout errors, click issues

## Notes

**Script Execution:** All scripts should be executed from the skill directory or with full paths. They use Nix shebangs so no manual dependency installation is required.

**Exit Codes:** Scripts exit with non-zero codes on failure. Check exit status to handle errors:
```bash
if ! ./scripts/click.js "#button"; then
    echo "Click failed"
    ./scripts/screenshot.js  # Capture state for debugging
fi
```

**Debug Mode:** Set `DEBUG=1` for verbose logging:
```bash
DEBUG=1 ./scripts/nav.js https://example.com
```

**Path Handling:** When passing paths to scripts, use absolute paths or ensure correct working directory.

**Browser State:** The browser instance persists across script calls. Use `pkill -f 'chromium.*remote-debugging-port=9222'` to restart if needed (see troubleshooting reference for safe cleanup).
