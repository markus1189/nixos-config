---
name: controlling-browser
description: "Remote controls Chrome/Chromium browsers using Chrome DevTools Protocol for web automation. Use when browsing the web, scraping content, taking screenshots, interacting with web forms, dismissing cookie dialogs, or when the user mentions web automation or browser control."
---

# Web Browser Skill

Minimal CDP tools for collaborative site exploration. Auto-detects Chromium/Chrome on NixOS with zero setup.

## Contents
- [Start Browser](#start-browser)
- [Navigate](#navigate)
- [Evaluate JavaScript](#evaluate-javascript)
- [Screenshot](#screenshot)
- [Generate PDF](#generate-pdf)
- [Extract Readable Content](#extract-readable-content)
- [Pick Elements](#pick-elements)
- [Dismiss Cookie Dialogs](#dismiss-cookie-dialogs)
- [Cookies](#cookies)
- [Storage](#storage)
- [Click Elements](#click-elements)
- [Type Text](#type-text)
- [Fill Forms](#fill-forms)
- [Press Keys](#press-keys)
- [Scroll Page](#scroll-page)
- [Hover](#hover)
- [Wait for Elements](#wait-for-elements)
- [Capture Console Logs](#capture-console-logs)
- [Watch Errors](#watch-errors)
- [Set Request Headers](#set-request-headers)
- [Block Requests](#block-requests)
- [Mock Responses](#mock-responses)
- [Troubleshooting](#troubleshooting)
- [Debugging](#debugging)

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

## Generate PDF

```bash
./scripts/save-pdf.js output.pdf
./scripts/save-pdf.js output.pdf --landscape --no-background
./scripts/save-pdf.js output.pdf --paper a4 --margins 0.5
```

Convert current page to PDF. Supports landscape, custom paper sizes (letter/legal/a4/a3), margins, scale, page ranges, and headers/footers with template variables. Run `./scripts/save-pdf.js` without arguments for full options.

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

## Cookies

```bash
./scripts/cookies.js list                    # List all cookies
./scripts/cookies.js list --url <url>        # List cookies for URL
./scripts/cookies.js export session.json     # Export to file
./scripts/cookies.js import session.json     # Import from file
./scripts/cookies.js clear                   # Clear all cookies
./scripts/cookies.js clear --url <url>       # Clear cookies for URL
```

Manage browser cookies. Export/import useful for session transfer between profiles.

## Storage

```bash
./scripts/storage.js list                    # List localStorage items
./scripts/storage.js list --session          # List sessionStorage items
./scripts/storage.js get <key>               # Get localStorage value
./scripts/storage.js set <key> <value>       # Set localStorage value
./scripts/storage.js export backup.json      # Export localStorage
./scripts/storage.js import backup.json      # Import localStorage
./scripts/storage.js clear                   # Clear localStorage
./scripts/storage.js clear --all             # Clear all storage (cache, cookies, etc)
```

Manage localStorage and sessionStorage. Use `--session` flag for sessionStorage.

## Click Elements

```bash
./scripts/click.js "button[type=submit]"
./scripts/click.js "#login-button"
./scripts/click.js ".cookie-accept" --wait 2000
./scripts/click.js "tr.row" --double
```

Click element by CSS selector.

Options:
- `--wait <ms>` - Wait for element to appear before clicking
- `--double` - Double-click instead of single click

## Type Text

```bash
./scripts/type-text.js "Hello world"
./scripts/type-text.js "Search query" --selector "input[name=q]"
./scripts/type-text.js "Password123" --selector "#password" --clear
./scripts/type-text.js "Slow typing" --delay 100
```

Type text into focused element or specific selector.

Options:
- `--selector <sel>` - Click selector before typing to focus
- `--delay <ms>` - Delay between keystrokes (human-like typing)
- `--clear` - Clear existing value before typing

## Fill Forms

```bash
./scripts/fill-form.js --field "#email" --value "user@example.com" --field "#password" --value "secret"
./scripts/fill-form.js --json form-data.json
./scripts/fill-form.js --field "#username" --value "test" --submit "button[type=submit]"
```

Fill multiple form fields at once. Automatically clears existing values and triggers input/change events.

Options:
- `--field <sel> --value <val>` - Selector and value pair (repeatable)
- `--json <file>` - JSON file with `{selector: value}` pairs
- `--submit <sel>` - Click submit button after filling

## Press Keys

```bash
./scripts/press-key.js Enter
./scripts/press-key.js Escape
./scripts/press-key.js "Control+C"
./scripts/press-key.js "Control+Shift+S"
./scripts/press-key.js Tab --times 3
```

Simulate keyboard key presses and shortcuts.

Supported keys: Enter, Escape/Esc, Tab, Backspace, Delete, Space, Arrow keys, Home, End, PageUp, PageDown, F1-F12, any single character.

Modifiers: Control/Ctrl, Alt, Shift, Meta/Cmd/Command

Options:
- `--times <n>` - Press key n times

## Scroll Page

```bash
./scripts/scroll.js --to-bottom
./scripts/scroll.js --to-top
./scripts/scroll.js --pixels 500
./scripts/scroll.js --pixels -300
./scripts/scroll.js --to-selector "#footer"
./scripts/scroll.js --to-selector ".comments" --smooth
./scripts/scroll.js --infinite --delay 1000
```

Scroll the page or to a specific element.

Options:
- `--to-bottom` - Scroll to bottom of page
- `--to-top` - Scroll to top of page
- `--pixels <n>` - Scroll by n pixels (negative for up)
- `--to-selector <sel>` - Scroll element into view
- `--smooth` - Use smooth scrolling animation
- `--infinite` - Scroll incrementally for infinite scroll pages
- `--delay <ms>` - Delay between scroll steps (with --infinite)

## Hover

```bash
./scripts/hover.js ".dropdown-trigger"
./scripts/hover.js "#menu-item" --duration 2000
```

Hover over an element to trigger hover states (dropdowns, tooltips, etc.).

Options:
- `--duration <ms>` - How long to hold hover before returning

## Wait for Elements

```bash
./scripts/wait-for.js --selector "#dynamic-content"
./scripts/wait-for.js --selector ".loading" --disappear
./scripts/wait-for.js --selector "#button" --visible --timeout 10000
./scripts/wait-for.js --text "Loading complete"
```

Wait for element to appear, disappear, or become visible. Essential for SPAs and dynamic content.

Options:
- `--selector <css>` - CSS selector to wait for
- `--text <string>` - Wait for text to appear on page
- `--disappear` - Wait for element to be removed
- `--visible` - Wait for element to be visible (not display:none/hidden)
- `--timeout <ms>` - Max wait time (default: 30000)
- `--interval <ms>` - Polling interval (default: 100)

## Capture Console Logs

```bash
./scripts/capture-logs.js                          # Stream to stdout
./scripts/capture-logs.js --duration 5000          # Capture for 5 seconds
./scripts/capture-logs.js --output logs.txt        # Save to file
./scripts/capture-logs.js --format json --output logs.json
./scripts/capture-logs.js --level error            # Only errors
```

Collect console.log/warn/error messages and JavaScript exceptions.

Options:
- `--output <file>` - Save logs to file
- `--format json|text` - Output format (default: text)
- `--duration <ms>` - Capture duration in milliseconds
- `--level <type>` - Filter by level: log, debug, info, warning, error

## Watch Errors

```bash
./scripts/watch-errors.js                   # Stream errors to stdout
./scripts/watch-errors.js --output errs.txt # Also save to file
```

Real-time error and exception monitoring. Lightweight alternative to capture-logs when you only care about errors.

## Set Request Headers

```bash
./scripts/set-headers.js "Authorization: Bearer token123"
./scripts/set-headers.js "User-Agent: CustomBot" "X-Custom: value"
./scripts/set-headers.js --remove Cookie --remove Accept-Language
```

Globally modify HTTP headers for all requests. Runs until Ctrl+C.

Use cases:
- Add authentication headers for API testing
- Spoof User-Agent
- Remove tracking cookies
- Add custom headers for debugging

## Block Requests

```bash
./scripts/block-requests.js "*/ads/*"
./scripts/block-requests.js "*google-analytics*" "*facebook*" "*tracking*"
```

Block requests matching URL patterns (glob-style, * matches any characters). Runs until Ctrl+C.

## Mock Responses

```bash
./scripts/mock-response.js --url "*/api/user" --body '{"name":"test"}'
./scripts/mock-response.js --url "*/api/*" --file mock.json
./scripts/mock-response.js --url "*/api/*" --body '{}' --status 404
```

Return mock response for requests matching URL pattern. Runs until Ctrl+C.

Options:
- `--url <pattern>` - URL pattern to match (glob-style)
- `--body <content>` - Response body
- `--file <path>` - Read response from file
- `--status <code>` - HTTP status code (default: 200)
- `--content-type <type>` - Content-Type header (auto-detected if not specified)

**Handle dynamic SPA content:**
1. `./scripts/nav.js https://spa-site.com`
2. `./scripts/wait-for.js --selector "#app" --visible`
3. `./scripts/click.js "#load-more"`
4. `./scripts/wait-for.js --selector ".loading" --disappear`
5. `./scripts/screenshot.js`

**Form submission with confirmation:**
1. `./scripts/fill-form.js --field "#email" --value "test@example.com"`
2. `./scripts/click.js "button[type=submit]"`
3. `./scripts/wait-for.js --text "Success" --timeout 10000`

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
