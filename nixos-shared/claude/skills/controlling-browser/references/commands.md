# Browser Control Commands Reference

Complete command reference for controlling Chrome/Chromium browsers via Chrome DevTools Protocol.

## Table of Contents

**Navigation & Control**
- [start](#start) - Start browser with remote debugging
- [nav](#nav) - Navigate to URL

**Screenshots & PDFs**
- [screenshot](#screenshot) - Capture page as image
- [save-pdf](#save-pdf) - Generate PDF from page

**Content Extraction**
- [readable](#readable) - Extract article content
- [eval](#eval) - Execute JavaScript
- [pick](#pick) - Interactive element picker

**Element Interaction**
- [click](#click) - Click elements
- [type-text](#type-text) - Type into inputs
- [fill-form](#fill-form) - Fill multiple form fields
- [press-key](#press-key) - Simulate keyboard input
- [scroll](#scroll) - Scroll page or to elements
- [hover](#hover) - Trigger hover states

**Dynamic Content**
- [wait-for](#wait-for) - Wait for elements or conditions

**Cookies & Storage**
- [cookies](#cookies) - Manage browser cookies
- [storage](#storage) - Manage localStorage/sessionStorage
- [dismiss-cookies](#dismiss-cookies) - Auto-dismiss cookie dialogs

**Debugging**
- [capture-logs](#capture-logs) - Collect console messages
- [watch-errors](#watch-errors) - Monitor errors in real-time

**Network Control**
- [set-headers](#set-headers) - Modify HTTP headers
- [block-requests](#block-requests) - Block network requests
- [mock-response](#mock-response) - Mock API responses

**Accessibility**
- [get-accessibility](#get-accessibility) - Inspect accessibility tree
- [check-a11y](#check-a11y) - Scan for accessibility issues
- [find-by-label](#find-by-label) - Find elements by accessible name

---

## Navigation & Control

### start

Start browser with remote debugging enabled on port 9222.

```bash
./scripts/start.js              # Fresh profile
./scripts/start.js --profile    # Copy your profile (cookies, logins)
```

Launches Chromium/Chrome with CDP enabled. Auto-detects browser on NixOS. Uses temporary cache directory in `~/.cache/scraping` unless `--profile` is specified.

**Options:**
- `--profile` - Copy your user profile (includes cookies, localStorage, logins)

**Integration example:**
```bash
# Start browser and verify it's running
./scripts/start.js

# Check browser status
if pgrep -f 'remote-debugging-port=9222' > /dev/null; then
    echo "Browser is running"
fi
```

### nav

Navigate to URL in current tab or open new tab.

```bash
./scripts/nav.js https://example.com
./scripts/nav.js https://example.com --new
```

Navigates to specified URL. Waits for page load completion before returning.

**Options:**
- `--new` - Open URL in new tab instead of current tab

**Integration example:**
```bash
# Navigate and capture the page title
./scripts/nav.js https://example.com
TITLE=$(./scripts/eval.js 'document.title')
echo "Navigated to: $TITLE"

# Navigate in new tab
./scripts/nav.js https://example.com --new
```

---

## Screenshots & PDFs

### screenshot

Capture current page as image.

```bash
./scripts/screenshot.js
```

Takes full-page screenshot and saves to temporary file. Returns absolute path to image file.

**Integration example:**
```bash
# Capture screenshot and read with Read tool
SCREENSHOT_PATH=$(./scripts/screenshot.js)
echo "Screenshot saved to: $SCREENSHOT_PATH"

# Use Read tool to analyze the screenshot
# Read tool call: file_path = $SCREENSHOT_PATH
```

### save-pdf

Generate PDF from current page.

```bash
./scripts/save-pdf.js output.pdf
./scripts/save-pdf.js output.pdf --landscape --no-background
./scripts/save-pdf.js output.pdf --paper a4 --margins 0.5
./scripts/save-pdf.js output.pdf --pages 1-5 --scale 0.8
```

Converts page to PDF with customizable options. Supports headers/footers with template variables.

**Options:**
- `--landscape` - Landscape orientation (default: portrait)
- `--no-background` - Omit background graphics
- `--paper <size>` - Paper size: letter, legal, a3, a4 (default: letter)
- `--margins <inches>` - Margins in inches (default: 0.4)
- `--scale <factor>` - Scale factor 0.1-2.0 (default: 1.0)
- `--pages <range>` - Page range (e.g., "1-5", "1,3,5")
- `--header <template>` - Header template HTML
- `--footer <template>` - Footer template HTML

**Template variables:** `{page}`, `{total}`, `{title}`, `{url}`, `{date}`

**Integration example:**
```bash
# Generate PDF with custom settings
./scripts/nav.js https://example.com/article
./scripts/save-pdf.js article.pdf --paper a4 --margins 0.5

# Verify PDF was created
if [[ -f article.pdf ]]; then
    echo "PDF generated: $(du -h article.pdf)"
fi
```

---

## Content Extraction

### readable

Extract clean article content using Mozilla's Readability algorithm.

```bash
./scripts/readable.js              # Full article as JSON
./scripts/readable.js --text       # Plain text only
./scripts/readable.js --html       # Clean HTML only
./scripts/readable.js --title      # Title only
./scripts/readable.js --summary    # Excerpt/summary
```

Extracts main article content, removing ads, navigation, and boilerplate. Best for articles, blog posts, and news pages. Uses same algorithm as Firefox Reader View.

**Options:**
- `--text` - Return plain text content only
- `--html` - Return clean HTML only
- `--title` - Return article title only
- `--summary` - Return excerpt/summary only
- (no flags) - Return full JSON with metadata

**Integration example:**
```bash
# Extract article and save for processing
./scripts/nav.js https://blog.example.com/post
ARTICLE_JSON=$(./scripts/readable.js)

# Parse specific fields
TITLE=$(echo "$ARTICLE_JSON" | jq -r '.title')
AUTHOR=$(echo "$ARTICLE_JSON" | jq -r '.byline')
TEXT=$(./scripts/readable.js --text)

echo "Article: $TITLE by $AUTHOR"
echo "$TEXT" | wc -w  # Word count
```

### eval

Execute JavaScript in active tab context.

```bash
./scripts/eval.js 'document.title'
./scripts/eval.js 'document.querySelectorAll("a").length'
./scripts/eval.js 'Array.from(document.querySelectorAll("h1")).map(h => h.textContent)'
```

Evaluates JavaScript expression in page context. Runs in async context. Use single quotes to avoid shell escaping issues.

**Integration example:**
```bash
# Extract structured data from page
./scripts/nav.js https://news.example.com
LINKS=$(./scripts/eval.js 'JSON.stringify(Array.from(document.querySelectorAll("a.headline")).map(a => ({text: a.textContent, href: a.href})))')

# Parse with jq
echo "$LINKS" | jq '.[] | "\(.text): \(.href)"'

# Check for specific element
HAS_LOGIN=$(./scripts/eval.js 'document.querySelector("#login-button") !== null')
if [[ "$HAS_LOGIN" == "true" ]]; then
    echo "Login button found"
fi
```

### pick

Interactive element picker with visual highlighting.

```bash
./scripts/pick.js "Click the submit button"
```

Opens interactive picker mode with instructions. Click elements to select them. Supports multi-select with Cmd/Ctrl+Click. Press Enter when finished.

**Usage:**
- Single click: Select element
- Cmd/Ctrl+Click: Add to selection (multi-select)
- Enter: Confirm selection
- Escape: Cancel

**Integration example:**
```bash
# Let user select elements, then extract info
./scripts/nav.js https://example.com
SELECTED=$(./scripts/pick.js "Select the products you want to compare")

# Parse selected elements (returns JSON array)
echo "$SELECTED" | jq -r '.[] | .selector'
```

---

## Element Interaction

### click

Click element by CSS selector.

```bash
./scripts/click.js "button[type=submit]"
./scripts/click.js "#login-button"
./scripts/click.js ".cookie-accept" --wait 2000
./scripts/click.js "tr.row" --double
```

Clicks first matching element. Scrolls element into view if needed.

**Options:**
- `--wait <ms>` - Wait for element to appear before clicking
- `--double` - Double-click instead of single click

**Integration example:**
```bash
# Click button and verify action
./scripts/nav.js https://example.com
./scripts/click.js "#show-more-button"
./scripts/wait-for.js --selector ".additional-content" --visible

# Conditional clicking
if ./scripts/eval.js 'document.querySelector(".modal") !== null'; then
    echo "Modal detected, closing it"
    ./scripts/click.js ".modal .close-button"
fi
```

### type-text

Type text into focused element or specific selector.

```bash
./scripts/type-text.js "Hello world"
./scripts/type-text.js "Search query" --selector "input[name=q]"
./scripts/type-text.js "Password123" --selector "#password" --clear
./scripts/type-text.js "Slow typing" --delay 100
```

Types text character by character. Can clear existing value first. Supports human-like typing with delays.

**Options:**
- `--selector <css>` - Click selector first to focus element
- `--delay <ms>` - Delay between keystrokes (human-like typing)
- `--clear` - Clear existing value before typing

**Integration example:**
```bash
# Search workflow
./scripts/nav.js https://example.com
./scripts/type-text.js "search term" --selector "#search-input" --clear
./scripts/press-key.js Enter
./scripts/wait-for.js --text "results" --timeout 5000
```

### fill-form

Fill multiple form fields at once.

```bash
./scripts/fill-form.js --field "#email" --value "user@example.com" --field "#password" --value "secret"
./scripts/fill-form.js --json form-data.json
./scripts/fill-form.js --field "#username" --value "test" --submit "button[type=submit]"
```

Fills multiple fields efficiently. Automatically clears existing values and triggers input/change events for framework compatibility.

**Options:**
- `--field <selector> --value <text>` - Selector and value pair (repeatable)
- `--json <file>` - JSON file with `{selector: value}` pairs
- `--submit <selector>` - Click submit button after filling

**JSON format:**
```json
{
  "#username": "testuser",
  "#email": "test@example.com",
  "#password": "secret123"
}
```

**Integration example:**
```bash
# Create form data
cat > /tmp/login-data.json << 'EOF'
{
  "#username": "demo_user",
  "#password": "demo_pass"
}
EOF

# Fill and submit form
./scripts/nav.js https://example.com/login
./scripts/fill-form.js --json /tmp/login-data.json --submit "button[type=submit]"
./scripts/wait-for.js --text "Welcome" --timeout 10000
```

### press-key

Simulate keyboard key presses and shortcuts.

```bash
./scripts/press-key.js Enter
./scripts/press-key.js Escape
./scripts/press-key.js "Control+C"
./scripts/press-key.js "Control+Shift+S"
./scripts/press-key.js Tab --times 3
```

Simulates physical keyboard input including modifiers and shortcuts.

**Supported keys:** Enter, Escape/Esc, Tab, Backspace, Delete, Space, ArrowUp, ArrowDown, ArrowLeft, ArrowRight, Home, End, PageUp, PageDown, F1-F12, any single character

**Modifiers:** Control/Ctrl, Alt, Shift, Meta/Cmd/Command (use + to combine)

**Options:**
- `--times <n>` - Press key n times

**Integration example:**
```bash
# Navigate through form with Tab
./scripts/click.js "#first-input"
./scripts/type-text.js "value1"
./scripts/press-key.js Tab
./scripts/type-text.js "value2"
./scripts/press-key.js Tab
./scripts/press-key.js Enter  # Submit

# Keyboard shortcuts
./scripts/press-key.js "Control+Shift+I"  # Open DevTools
./scripts/press-key.js "F5"               # Refresh
```

### scroll

Scroll page or to specific elements.

```bash
./scripts/scroll.js --to-bottom
./scripts/scroll.js --to-top
./scripts/scroll.js --pixels 500
./scripts/scroll.js --pixels -300
./scripts/scroll.js --to-selector "#footer"
./scripts/scroll.js --to-selector ".comments" --smooth
./scripts/scroll.js --infinite --delay 1000
```

Scrolls page viewport or scrolls elements into view.

**Options:**
- `--to-bottom` - Scroll to bottom of page
- `--to-top` - Scroll to top of page
- `--pixels <n>` - Scroll by n pixels (negative for up)
- `--to-selector <css>` - Scroll element into view
- `--smooth` - Use smooth scrolling animation
- `--infinite` - Scroll incrementally for infinite scroll pages
- `--delay <ms>` - Delay between scroll steps (with --infinite)

**Integration example:**
```bash
# Load infinite scroll content
./scripts/nav.js https://example.com/feed
for i in {1..5}; do
    ./scripts/scroll.js --to-bottom --smooth
    sleep 2
    COUNT=$(./scripts/eval.js 'document.querySelectorAll(".post").length')
    echo "Loaded $COUNT posts"
done

# Scroll to specific section
./scripts/scroll.js --to-selector "#comments"
SCREENSHOT=$(./scripts/screenshot.js)
```

### hover

Hover over element to trigger hover states.

```bash
./scripts/hover.js ".dropdown-trigger"
./scripts/hover.js "#menu-item" --duration 2000
```

Moves mouse over element and holds position. Useful for dropdowns, tooltips, and hover-triggered content.

**Options:**
- `--duration <ms>` - How long to hold hover before returning (default: 1000)

**Integration example:**
```bash
# Trigger dropdown and click item
./scripts/hover.js ".nav-menu" --duration 500
./scripts/wait-for.js --selector ".dropdown-menu" --visible
./scripts/click.js ".dropdown-menu .item-settings"

# Capture tooltip
./scripts/hover.js "[data-tooltip]" --duration 500
TOOLTIP_TEXT=$(./scripts/eval.js 'document.querySelector(".tooltip").textContent')
echo "Tooltip: $TOOLTIP_TEXT"
```

---

## Dynamic Content

### wait-for

Wait for element to appear, disappear, or become visible.

```bash
./scripts/wait-for.js --selector "#dynamic-content"
./scripts/wait-for.js --selector ".loading" --disappear
./scripts/wait-for.js --selector "#button" --visible --timeout 10000
./scripts/wait-for.js --text "Loading complete"
```

Polls for conditions with configurable timeout and interval. Essential for SPAs and dynamic content.

**Options:**
- `--selector <css>` - CSS selector to wait for
- `--text <string>` - Wait for text to appear anywhere on page
- `--disappear` - Wait for element to be removed from DOM
- `--visible` - Wait for element to be visible (not display:none/hidden)
- `--timeout <ms>` - Max wait time in milliseconds (default: 30000)
- `--interval <ms>` - Polling interval in milliseconds (default: 100)

**Integration example:**
```bash
# Handle dynamic SPA content
./scripts/nav.js https://spa-site.com
./scripts/wait-for.js --selector "#app" --visible --timeout 10000
./scripts/click.js "#load-more"
./scripts/wait-for.js --selector ".loading" --disappear
ITEMS=$(./scripts/eval.js 'document.querySelectorAll(".item").length')
echo "Loaded $ITEMS items"

# Wait for success message
./scripts/fill-form.js --field "#email" --value "test@example.com" --submit "button"
if ./scripts/wait-for.js --text "Success" --timeout 10000; then
    echo "Form submitted successfully"
else
    echo "Form submission failed or timed out"
fi
```

---

## Cookies & Storage

### cookies

Manage browser cookies.

```bash
./scripts/cookies.js list                    # List all cookies
./scripts/cookies.js list --url <url>        # List cookies for URL
./scripts/cookies.js export session.json     # Export to file
./scripts/cookies.js import session.json     # Import from file
./scripts/cookies.js clear                   # Clear all cookies
./scripts/cookies.js clear --url <url>       # Clear cookies for URL
```

Inspect, export, import, and clear cookies. Export/import useful for transferring sessions between profiles.

**Options:**
- `--url <url>` - Filter operations to specific URL/domain

**Integration example:**
```bash
# Backup cookies before test
./scripts/cookies.js export /tmp/cookies-backup.json

# Perform test that modifies cookies
./scripts/nav.js https://example.com
./scripts/click.js "#accept-all-cookies"

# Inspect cookies
./scripts/cookies.js list --url https://example.com | jq '.[] | select(.name == "session_id")'

# Restore if needed
./scripts/cookies.js import /tmp/cookies-backup.json
```

### storage

Manage localStorage and sessionStorage.

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

Inspect and manipulate browser storage. Use `--session` flag for sessionStorage operations.

**Options:**
- `--session` - Operate on sessionStorage instead of localStorage
- `--all` - (with clear) Clear all storage types including cache and cookies

**Integration example:**
```bash
# Inspect app state
./scripts/nav.js https://example.com
./scripts/storage.js list | jq '.'
USER_PREFS=$(./scripts/storage.js get user_preferences)
echo "User preferences: $USER_PREFS"

# Modify app state
./scripts/storage.js set debug_mode true
./scripts/nav.js https://example.com  # Reload to apply

# Backup and restore
./scripts/storage.js export /tmp/storage-backup.json
./scripts/storage.js clear
./scripts/storage.js import /tmp/storage-backup.json
```

### dismiss-cookies

Auto-dismiss cookie consent dialogs.

```bash
./scripts/dismiss-cookies.js          # Accept cookies
./scripts/dismiss-cookies.js --reject # Reject cookies (where possible)
```

Automatically detects and dismisses cookie dialogs from major CMPs (OneTrust, Google, Cookiebot, Didomi, Quantcast, and others).

**Options:**
- `--reject` - Attempt to reject cookies instead of accepting (may not work on all sites)

**Integration example:**
```bash
# Navigate and dismiss cookies in one flow
./scripts/nav.js https://news-site.com
sleep 2  # Wait for dialog to appear
./scripts/dismiss-cookies.js

# Verify dialog is gone
HAS_OVERLAY=$(./scripts/eval.js 'document.querySelector(".cookie-dialog") !== null')
if [[ "$HAS_OVERLAY" == "false" ]]; then
    echo "Cookie dialog dismissed successfully"
fi

# Continue with scraping
./scripts/readable.js --text > article.txt
```

---

## Debugging

### capture-logs

Collect console.log/warn/error messages and JavaScript exceptions.

```bash
./scripts/capture-logs.js                          # Stream to stdout
./scripts/capture-logs.js --duration 5000          # Capture for 5 seconds
./scripts/capture-logs.js --output logs.txt        # Save to file
./scripts/capture-logs.js --format json --output logs.json
./scripts/capture-logs.js --level error            # Only errors
```

Attaches to console API and captures all messages. Runs continuously until stopped (Ctrl+C) or duration expires.

**Options:**
- `--output <file>` - Save logs to file (also streams to stdout)
- `--format json|text` - Output format (default: text)
- `--duration <ms>` - Capture duration in milliseconds (exits after duration)
- `--level <type>` - Filter by level: log, debug, info, warning, error

**Integration example:**
```bash
# Capture errors during page load
./scripts/capture-logs.js --level error --output /tmp/errors.txt --duration 3000 &
LOG_PID=$!
./scripts/nav.js https://example.com
wait $LOG_PID

# Check for errors
if [[ -s /tmp/errors.txt ]]; then
    echo "Errors detected:"
    cat /tmp/errors.txt
fi

# Debug API calls
./scripts/capture-logs.js --format json --duration 10000 > /tmp/console.json &
./scripts/click.js "#fetch-data"
sleep 2
pkill -P $$ capture-logs
jq '.[] | select(.message | contains("API"))' /tmp/console.json
```

### watch-errors

Real-time error and exception monitoring.

```bash
./scripts/watch-errors.js                   # Stream errors to stdout
./scripts/watch-errors.js --output errs.txt # Also save to file
```

Lightweight error-only monitoring. Runs until stopped (Ctrl+C).

**Options:**
- `--output <file>` - Also save to file

**Integration example:**
```bash
# Monitor for errors during testing
./scripts/watch-errors.js --output /tmp/test-errors.txt &
ERROR_PID=$!

# Run test sequence
./scripts/nav.js https://example.com/app
./scripts/click.js "#trigger-action"
sleep 5

# Stop monitoring
kill $ERROR_PID

# Check results
if [[ -s /tmp/test-errors.txt ]]; then
    echo "Errors occurred during test:"
    cat /tmp/test-errors.txt
else
    echo "No errors detected"
fi
```

---

## Network Control

### set-headers

Globally modify HTTP headers for all requests.

```bash
./scripts/set-headers.js "Authorization: Bearer token123"
./scripts/set-headers.js "User-Agent: CustomBot" "X-Custom: value"
./scripts/set-headers.js --remove Cookie --remove Accept-Language
```

Intercepts and modifies request headers. Runs until stopped (Ctrl+C). Affects all requests from all tabs.

**Options:**
- `--remove <header>` - Remove header from requests (repeatable)

**Use cases:**
- Add authentication headers for API testing
- Spoof User-Agent
- Remove tracking cookies
- Add custom headers for debugging

**Integration example:**
```bash
# API testing with auth header
./scripts/set-headers.js "Authorization: Bearer test-token-123" &
HEADER_PID=$!

./scripts/nav.js https://api.example.com/dashboard
./scripts/screenshot.js

kill $HEADER_PID

# Test with different User-Agent
./scripts/set-headers.js "User-Agent: Mozilla/5.0 (Mobile)" &
HEADER_PID=$!
./scripts/nav.js https://example.com
MOBILE_VIEW=$(./scripts/screenshot.js)
kill $HEADER_PID
```

### block-requests

Block requests matching URL patterns.

```bash
./scripts/block-requests.js "*/ads/*"
./scripts/block-requests.js "*google-analytics*" "*facebook*" "*tracking*"
```

Blocks network requests matching glob-style patterns. Runs until stopped (Ctrl+C).

**Pattern syntax:** Glob-style where `*` matches any characters

**Integration example:**
```bash
# Block ads and trackers for faster loading
./scripts/block-requests.js "*/ads/*" "*analytics*" "*tracking*" &
BLOCK_PID=$!

./scripts/nav.js https://news-site.com
./scripts/readable.js --text > clean-article.txt

kill $BLOCK_PID

# Test performance impact
START=$(date +%s)
./scripts/nav.js https://example.com
UNBLOCKED_TIME=$(($(date +%s) - START))

./scripts/block-requests.js "*ads*" "*analytics*" &
BLOCK_PID=$!
START=$(date +%s)
./scripts/nav.js https://example.com
BLOCKED_TIME=$(($(date +%s) - START))
kill $BLOCK_PID

echo "Load time without blocking: ${UNBLOCKED_TIME}s"
echo "Load time with blocking: ${BLOCKED_TIME}s"
```

### mock-response

Return mock responses for requests matching URL pattern.

```bash
./scripts/mock-response.js --url "*/api/user" --body '{"name":"test"}'
./scripts/mock-response.js --url "*/api/*" --file mock.json
./scripts/mock-response.js --url "*/api/*" --body '{}' --status 404
```

Intercepts matching requests and returns mock response. Runs until stopped (Ctrl+C).

**Options:**
- `--url <pattern>` - URL pattern to match (glob-style, `*` matches any)
- `--body <content>` - Response body string
- `--file <path>` - Read response from file
- `--status <code>` - HTTP status code (default: 200)
- `--content-type <type>` - Content-Type header (auto-detected if not specified)

**Integration example:**
```bash
# Test API error handling
./scripts/mock-response.js --url "*/api/data" --body '{"error":"Not found"}' --status 404 &
MOCK_PID=$!

./scripts/nav.js https://example.com/app
./scripts/click.js "#load-data"
./scripts/wait-for.js --text "Error" --timeout 5000

kill $MOCK_PID

# Test with mock data file
cat > /tmp/mock-users.json << 'EOF'
{
  "users": [
    {"id": 1, "name": "Test User 1"},
    {"id": 2, "name": "Test User 2"}
  ]
}
EOF

./scripts/mock-response.js --url "*/api/users" --file /tmp/mock-users.json &
MOCK_PID=$!
./scripts/nav.js https://example.com/users
./scripts/screenshot.js
kill $MOCK_PID
```

---

## Accessibility

### get-accessibility

Query accessibility tree to inspect ARIA roles, labels, and properties.

```bash
./scripts/get-accessibility.js --selector "#my-button"
./scripts/get-accessibility.js --full-tree
./scripts/get-accessibility.js --role button
./scripts/get-accessibility.js --name "Submit"
./scripts/get-accessibility.js --full-tree --output a11y-tree.json
```

Inspects how assistive technologies (screen readers) see the page. Useful for understanding element semantics.

**Options:**
- `--selector <css>` - Get accessibility info for specific element
- `--full-tree` - Get complete accessibility tree
- `--role <role>` - Query by ARIA role (button, heading, link, textbox, etc.)
- `--name <name>` - Query by accessible name (exact or partial match)
- `--output <file>` - Save as JSON
- `--depth <n>` - Limit tree depth (with --full-tree)

**Integration example:**
```bash
# Inspect button accessibility
./scripts/nav.js https://example.com
BUTTON_A11Y=$(./scripts/get-accessibility.js --selector "#submit-button")
echo "$BUTTON_A11Y" | jq '.name, .role, .description'

# Find all headings
HEADINGS=$(./scripts/get-accessibility.js --role heading)
echo "$HEADINGS" | jq '.[] | "\(.level): \(.name)"'

# Export full tree for analysis
./scripts/get-accessibility.js --full-tree --output /tmp/a11y-tree.json
# Use Read tool to analyze /tmp/a11y-tree.json
```

### check-a11y

Scan page for common accessibility problems.

```bash
./scripts/check-a11y.js
./scripts/check-a11y.js --report issues.txt
./scripts/check-a11y.js --format json --report a11y.json
```

Automated accessibility audit. Exits with code 1 if errors are found (useful for CI).

**Checks for:**
- Missing alt text on images
- Form controls without labels
- Missing document title
- Skipped heading levels (h1 → h3)
- Empty links and buttons
- Low color contrast (if supported)

**Options:**
- `--report <file>` - Save issues to file
- `--format <type>` - Output format: text (default) or json
- `--severity <level>` - Minimum severity: error, warning, info (default: warning)

**Integration example:**
```bash
# Check accessibility and handle results
./scripts/nav.js https://example.com

if ./scripts/check-a11y.js --report /tmp/a11y-report.txt; then
    echo "No accessibility issues found"
else
    echo "Accessibility issues detected:"
    cat /tmp/a11y-report.txt
fi

# CI integration
./scripts/nav.js https://staging.example.com
./scripts/check-a11y.js --format json --report a11y-report.json --severity error
if [[ $? -eq 0 ]]; then
    echo "Accessibility check passed"
else
    echo "Accessibility check failed"
    jq '.[] | "\(.severity): \(.message)"' a11y-report.json
    exit 1
fi
```

### find-by-label

Find elements by their accessible name.

```bash
./scripts/find-by-label.js "Submit"
./scripts/find-by-label.js "Email" --type textbox
./scripts/find-by-label.js "Sign in" --type button --first
./scripts/find-by-label.js "Close" --exact --selector
```

Searches accessibility tree for elements with matching accessible names. More reliable than text content searching.

**Options:**
- `--type <role>` - Filter by ARIA role (button, textbox, link, heading, etc.)
- `--exact` - Require exact match instead of contains (default: contains)
- `--first` - Return only first match
- `--selector` - Output CSS selector if possible (instead of full info)

**Integration example:**
```bash
# Find and click button by accessible name
./scripts/nav.js https://example.com
BUTTON_INFO=$(./scripts/find-by-label.js "Sign in" --type button --first)
echo "Found button: $BUTTON_INFO"

# Get selector and click
SELECTOR=$(./scripts/find-by-label.js "Sign in" --type button --first --selector)
if [[ -n "$SELECTOR" ]]; then
    ./scripts/click.js "$SELECTOR"
fi

# Find all textbox inputs
INPUTS=$(./scripts/find-by-label.js "" --type textbox)
echo "$INPUTS" | jq '.[] | .name'

# Verify label associations
EMAIL_FIELD=$(./scripts/find-by-label.js "Email address" --type textbox --exact)
if echo "$EMAIL_FIELD" | jq -e '.labeledBy' > /dev/null; then
    echo "Email field is properly labeled"
else
    echo "WARNING: Email field missing proper label"
fi
```

---

## Common Workflows

### Handle Dynamic SPA Content

```bash
# Navigate and wait for app to load
./scripts/nav.js https://spa-site.com
./scripts/wait-for.js --selector "#app" --visible --timeout 10000

# Trigger lazy loading
./scripts/click.js "#load-more"
./scripts/wait-for.js --selector ".loading" --disappear

# Capture result
./scripts/screenshot.js
```

### Form Submission with Confirmation

```bash
# Fill form fields
./scripts/fill-form.js --field "#email" --value "test@example.com" --field "#name" --value "Test User"

# Submit and wait for success
./scripts/click.js "button[type=submit]"
./scripts/wait-for.js --text "Success" --timeout 10000
```

### API Testing with Mock Responses

```bash
# Start mocking API
./scripts/mock-response.js --url "*/api/data" --body '{"result":"mocked"}' &
MOCK_PID=$!

# Test application behavior
./scripts/nav.js https://example.com/app
./scripts/click.js "#fetch-data"
./scripts/wait-for.js --text "mocked" --timeout 5000

# Cleanup
kill $MOCK_PID
```
