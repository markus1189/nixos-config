---
name: controlling-browser
description: "Remote controls Chrome/Chromium browsers using Chrome DevTools Protocol for web automation. Use when browsing the web, scraping content, taking screenshots, interacting with web forms, dismissing cookie dialogs, or when the user mentions web automation or browser control."
---

# Web Browser Skill

Minimal CDP tools for collaborative site exploration. Auto-detects Chromium/Chrome on NixOS with zero setup.

## Start Browser

```bash
./scripts/start.js              # Fresh profile
./scripts/start.js --profile    # Copy your profile (cookies, logins)
```

Starts browser on `:9222` with remote debugging

## Navigate

```bash
./scripts/nav.js https://example.com
./scripts/nav.js https://example.com --new
```

Navigate current tab or open new tab.

## Evaluate JavaScript

```bash
./scripts/eval.js 'document.title'
./scripts/eval.js 'document.querySelectorAll("a").length'
```

Execute JavaScript in active tab (async context). Use single quotes for escaping.

## Screenshot

```bash
./scripts/screenshot.js
```

Returns temp file path.

## Extract Readable Content

```bash
./scripts/readable.js              # Full article as JSON
./scripts/readable.js --text       # Plain text only
./scripts/readable.js --html       # Clean HTML only
./scripts/readable.js --title      # Title only
./scripts/readable.js --summary    # Excerpt/summary
```

Uses Mozilla's Readability algorithm (Firefox Reader View) to extract main article content, removing ads, navigation, and boilerplate. Best for articles, blog posts, and news pages.

Returns article metadata (title, author, excerpt) plus clean content. Text format strips all formatting for easy processing.

## Pick Elements

```bash
./scripts/pick.js "Click the submit button"
```

Interactive picker. Click to select, Cmd/Ctrl+Click for multi-select, Enter to finish.

## Dismiss Cookie Dialogs

```bash
./scripts/dismiss-cookies.js          # Accept
./scripts/dismiss-cookies.js --reject # Reject (where possible)
```

Supports major CMPs (OneTrust, Google, Cookiebot, Didomi, Quantcast, and others).

Run after navigation with delay:
```bash
./scripts/nav.js https://example.com && sleep 2 && ./scripts/dismiss-cookies.js
```

## Typical Workflows

**Scraping article content:**
1. `./scripts/start.js`
2. `./scripts/nav.js https://article.com`
3. `sleep 2 && ./scripts/dismiss-cookies.js`
4. `./scripts/readable.js --text > article.txt`

**Visual inspection:**
1. `./scripts/start.js`
2. `./scripts/nav.js https://example.com`
3. `sleep 2 && ./scripts/dismiss-cookies.js`
4. `./scripts/screenshot.js`
5. `./scripts/eval.js 'document.title'`

## Troubleshooting

### Browser Fails to Start

If `start.js` reports "Failed to connect to Chrome/Chromium", there may be conflicting Chromium instances.

The script automatically kills instances using `remote-debugging-port=9222`, but conflicts can still occur.

**Safe manual cleanup** (only kills skill-related browsers):
```bash
# Kill browsers using the scraping cache directory
pkill -f 'chromium.*\.cache/scraping'
pkill -f 'chrome.*\.cache/scraping'

# Kill browsers using port 9222
pkill -f 'chromium.*remote-debugging-port=9222'
pkill -f 'chrome.*remote-debugging-port=9222'
```

**DO NOT** use `pkill chromium` or `pkill chrome` - this kills all browser instances including your work browsers.

After cleanup, run `./scripts/start.js` again.

## Debugging

Set `DEBUG=1` for verbose logging:
```bash
DEBUG=1 ./scripts/nav.js https://example.com
```
