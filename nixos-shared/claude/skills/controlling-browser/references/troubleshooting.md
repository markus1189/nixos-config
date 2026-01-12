# Troubleshooting Guide

This document provides comprehensive error handling and troubleshooting guidance for the controlling-browser skill.

## Browser Fails to Start

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

---

## Element Not Found

### Common Causes

1. **Page not fully loaded**: JavaScript-rendered content may not be present yet
2. **Incorrect selector**: CSS selector doesn't match the actual DOM structure
3. **Dynamic content**: Element is generated after initial page load
4. **Frame/iframe context**: Element is inside a frame and requires switching context
5. **Shadow DOM**: Element is inside a shadow root and requires special handling

### Debugging Steps

1. **Take a screenshot** to verify page state:
   ```bash
   ./scripts/screenshot.js output.png
   ```

2. **Inspect the actual DOM structure**:
   ```bash
   ./scripts/eval.js "document.querySelector('body').innerHTML"
   ```

3. **Check if element exists with different selector**:
   ```bash
   # Try ID selector
   ./scripts/eval.js "document.querySelector('#element-id')"

   # Try class selector
   ./scripts/eval.js "document.querySelector('.class-name')"

   # Try attribute selector
   ./scripts/eval.js "document.querySelector('[data-testid=\"value\"]')"
   ```

4. **Verify element visibility**:
   ```bash
   ./scripts/eval.js "
     const el = document.querySelector('selector');
     el ? {
       exists: true,
       visible: el.offsetParent !== null,
       rect: el.getBoundingClientRect()
     } : { exists: false }
   "
   ```

### Resolution Patterns

**For dynamically loaded content**:
```bash
# Use wait-for-element.js instead of immediate interaction
./scripts/wait-for-element.js "selector" --timeout 10000
./scripts/click.js "selector"
```

**For shadow DOM elements**:
```bash
# Use deep selector syntax
./scripts/eval.js "
  document.querySelector('host-element')
    .shadowRoot.querySelector('target')
"
```

**For iframe content**:
```bash
# Switch to frame context first
./scripts/eval.js "
  const frame = document.querySelector('iframe');
  frame.contentDocument.querySelector('target');
"
```

---

## Timeout Errors

### Why Timeouts Occur

1. **Slow network**: Page resources taking longer to load than expected
2. **Heavy JavaScript**: Complex client-side rendering delays page readiness
3. **Server latency**: Backend responses slower than anticipated
4. **Animations**: CSS transitions delaying element availability
5. **Third-party resources**: External scripts/images blocking page load

### How to Adjust Timeouts

**Navigation timeouts**:
```bash
# Default is 30 seconds, increase for slow sites
./scripts/navigate.js "https://slow-site.com" --timeout 60000
```

**Element wait timeouts**:
```bash
# Default is 5 seconds, increase for dynamic content
./scripts/wait-for-element.js "selector" --timeout 15000
```

**Script execution timeouts**:
```bash
# Increase timeout for long-running operations
./scripts/eval.js "expensiveOperation()" --timeout 30000
```

### When to Use Longer Timeouts

**Use longer timeouts (30-60 seconds) when**:
- Loading media-heavy pages (images, videos)
- Accessing sites with extensive third-party integrations
- Working with single-page applications with complex initialization
- Downloading or processing large files
- Dealing with known slow servers or high-latency connections

**Keep shorter timeouts (5-10 seconds) when**:
- Waiting for specific elements on already-loaded pages
- Testing rapid user interactions
- Working with local or fast-responding sites
- Implementing fail-fast patterns for error handling

### Troubleshooting Persistent Timeouts

1. **Check network conditions**: Verify internet connectivity and DNS resolution
2. **Test in regular browser**: Manually visit the URL to see actual load time
3. **Review console logs**: Check for JavaScript errors blocking page load
4. **Disable third-party blocking**: Some sites fail when ads/analytics are blocked
5. **Try headful mode**: Run with `--headful` to observe what's happening visually

---

## Click Not Working

### Common Causes

1. **Element not visible**: Hidden, off-screen, or covered by another element
2. **Overlapping elements**: Another element is intercepting the click
3. **Element not ready**: Button disabled or still initializing
4. **Animation in progress**: Element moving or transitioning
5. **Wrong timing**: Clicked too early (before ready) or too late (after removed)

### Debugging Steps

1. **Verify element is visible**:
   ```bash
   ./scripts/eval.js "
     const el = document.querySelector('button');
     ({
       exists: !!el,
       visible: el && el.offsetParent !== null,
       disabled: el && el.disabled,
       rect: el && el.getBoundingClientRect()
     })
   "
   ```

2. **Check for overlapping elements**:
   ```bash
   ./scripts/eval.js "
     const btn = document.querySelector('button');
     const rect = btn.getBoundingClientRect();
     const x = rect.left + rect.width / 2;
     const y = rect.top + rect.height / 2;
     const topEl = document.elementFromPoint(x, y);
     ({
       targetElement: btn.tagName,
       actualTopElement: topEl.tagName,
       isSameElement: topEl === btn
     })
   "
   ```

3. **Take screenshot before and after**:
   ```bash
   ./scripts/screenshot.js before-click.png
   ./scripts/click.js "button"
   ./scripts/screenshot.js after-click.png
   ```

### Resolution Patterns

**For hidden or off-screen elements**:
```bash
# Scroll into view first
./scripts/eval.js "
  document.querySelector('button').scrollIntoView({
    behavior: 'smooth',
    block: 'center'
  })
"
# Then wait a moment and click
sleep 1
./scripts/click.js "button"
```

**For overlapping elements**:
```bash
# Close overlays first (like cookie dialogs)
./scripts/dismiss-cookie-dialog.js
# Then click target
./scripts/click.js "button"
```

**For disabled elements**:
```bash
# Wait for element to become enabled
./scripts/wait-for-element.js "button:not([disabled])"
./scripts/click.js "button"
```

**For elements with animations**:
```bash
# Wait for animation to complete
./scripts/wait-for-element.js "button.animation-complete"
# Or use fixed delay
sleep 0.5
./scripts/click.js "button"
```

**Alternative interaction methods**:
```bash
# Use JavaScript click instead of CDP click
./scripts/eval.js "document.querySelector('button').click()"

# Use form submission for buttons in forms
./scripts/eval.js "document.querySelector('form').submit()"
```

---

## Cookie Dialog Issues

### CMP Not Recognized

The `dismiss-cookie-dialog.js` script uses autoconsent library with extensive CMP support, but some sites use custom implementations.

**When to file an issue**:
- Cookie dialog is visible and blocks interaction
- Site is popular and worth supporting
- Custom implementation is not a one-off

**Workaround for unrecognized CMPs**:
```bash
# Manual dismissal with custom selector
./scripts/click.js "button[aria-label='Accept cookies']"
# Or
./scripts/eval.js "document.querySelector('.cookie-accept-button').click()"
```

### Manual Dismissal Patterns

**Common cookie dialog patterns**:
```bash
# Accept button with text
./scripts/click.js "button:has-text('Accept')"

# Reject button
./scripts/click.js "button:has-text('Reject all')"

# Close button
./scripts/click.js "[aria-label='Close cookie banner']"

# Overlay removal
./scripts/eval.js "
  document.querySelector('.cookie-overlay').remove();
  document.body.style.overflow = 'auto';
"
```

### Prevention Strategies

**Dismiss early**: Run cookie dismissal immediately after navigation:
```bash
./scripts/navigate.js "https://example.com"
./scripts/dismiss-cookie-dialog.js
# Continue with other actions
```

**Check for dialog before interaction**: Prevent clicks being intercepted:
```bash
./scripts/dismiss-cookie-dialog.js
./scripts/click.js "target-element"
```

---

## Common Error Messages

### "Failed to connect to Chrome/Chromium"
**Meaning**: CDP cannot establish connection to browser on port 9222
**Fix**: Kill conflicting browser instances (see "Browser Fails to Start" section)

### "Navigation timeout of 30000ms exceeded"
**Meaning**: Page didn't finish loading within timeout period
**Fix**: Increase timeout with `--timeout` flag or check network/site issues

### "No node with given id found"
**Meaning**: Element existed when found but was removed before interaction
**Fix**: Page state changed; re-query the element or use faster workflow

### "Element is not visible"
**Meaning**: Element exists in DOM but has `display: none` or `visibility: hidden`
**Fix**: Wait for element to become visible or scroll it into view

### "Node is either not clickable or not an Element"
**Meaning**: Target node is text, comment, or non-interactive element
**Fix**: Verify selector targets an interactive element (button, link, input)

### "Cannot find context with specified id"
**Meaning**: Browser context or frame no longer exists
**Fix**: Browser may have crashed; restart and retry operation

### "Execution context was destroyed"
**Meaning**: Page navigated away during script execution
**Fix**: Wait for navigation to complete before running scripts

---

## Recovery Workflows

### Screenshot for Debugging

When encountering unexpected behavior:

1. **Capture current state**:
   ```bash
   ./scripts/screenshot.js debug-$(date +%s).png
   ```

2. **Inspect the screenshot** to verify:
   - Page loaded correctly
   - Expected elements are visible
   - No overlays or dialogs blocking interaction
   - Element placement matches expectations

3. **Compare with expected state**: Take reference screenshots in regular browser for comparison

### Using eval.js to Inspect State

For detailed debugging beyond visual inspection:

1. **Check page title and URL**:
   ```bash
   ./scripts/eval.js "({ title: document.title, url: location.href })"
   ```

2. **Inspect element properties**:
   ```bash
   ./scripts/eval.js "
     const el = document.querySelector('selector');
     ({
       tagName: el?.tagName,
       className: el?.className,
       id: el?.id,
       textContent: el?.textContent,
       attributes: el ? [...el.attributes].map(a => [a.name, a.value]) : []
     })
   "
   ```

3. **Check console errors**:
   ```bash
   ./scripts/eval.js "
     window.__errors = window.__errors || [];
     window.__errors
   "
   ```

4. **Verify page readiness**:
   ```bash
   ./scripts/eval.js "
     ({
       readyState: document.readyState,
       loaded: document.readyState === 'complete',
       scriptCount: document.scripts.length,
       styleCount: document.styleSheets.length
     })
   "
   ```

### Restart Browser Procedure

When browser becomes unresponsive or corrupted:

1. **Stop current browser**:
   ```bash
   ./scripts/stop.js
   ```

2. **Clean up if necessary** (if stop.js doesn't work):
   ```bash
   pkill -f 'chromium.*\.cache/scraping'
   pkill -f 'chrome.*\.cache/scraping'
   ```

3. **Restart browser**:
   ```bash
   ./scripts/start.js
   ```

4. **Verify connection**:
   ```bash
   ./scripts/eval.js "1 + 1"
   # Should output: 2
   ```

5. **Resume workflow**: Navigate to required page and continue operations

---

## Best Practices for Avoiding Issues

1. **Always wait for navigation to complete** before interacting with page
2. **Use wait-for-element.js** for dynamically loaded content
3. **Dismiss cookie dialogs early** to prevent interaction blocking
4. **Take screenshots** before complex interactions for debugging
5. **Verify element state** (visible, enabled, clickable) before interaction
6. **Use appropriate timeouts** based on site performance characteristics
7. **Handle errors gracefully** with fallback strategies
8. **Test in headful mode** when developing new workflows
9. **Keep browser sessions fresh** by restarting periodically for long-running tasks
10. **Document site-specific quirks** for future reference
