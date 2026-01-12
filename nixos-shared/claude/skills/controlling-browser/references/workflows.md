# Multi-Step Workflows

This document provides comprehensive examples of multi-step browser automation workflows with validation, error handling, and debugging strategies.

## Basic Workflows

### Handle Dynamic SPA Content

When working with single-page applications that load content dynamically:

1. `./scripts/nav.js https://spa-site.com`
2. `./scripts/wait-for.js --selector "#app" --visible`
3. `./scripts/click.js "#load-more"`
4. `./scripts/wait-for.js --selector ".loading" --disappear`
5. `./scripts/screenshot.js`

**Validation**: Always verify element visibility before interaction. Use screenshots to debug layout issues.

### Form Submission with Confirmation

Submit a form and wait for success confirmation:

1. `./scripts/fill-form.js --field "#email" --value "test@example.com"`
2. `./scripts/click.js "button[type=submit]"`
3. `./scripts/wait-for.js --text "Success" --timeout 10000`

**Validation**: Check for error messages with `--text "Error"` as an alternative outcome.

---

## Advanced Workflows

### Login Flow with Session Export

Complete authentication workflow with cookie export for future sessions:

```bash
# 1. Navigate to login page
./scripts/nav.js https://example.com/login

# 2. Verify login form is present
./scripts/wait-for.js --selector "form#login-form" --visible --timeout 10000
if [ $? -ne 0 ]; then
    echo "ERROR: Login form not found"
    ./scripts/screenshot.js
    exit 1
fi

# 3. Fill in credentials
./scripts/fill-form.js \
    --field "#username" --value "user@example.com" \
    --field "#password" --value "secret123"

# 4. Verify fields were filled (optional but recommended)
./scripts/eval.js 'document.querySelector("#username").value' > /tmp/username_check.txt
if ! grep -q "user@example.com" /tmp/username_check.txt; then
    echo "ERROR: Username field not filled correctly"
    ./scripts/screenshot.js
    exit 1
fi

# 5. Submit the form
./scripts/click.js "button[type=submit]"

# 6. Wait for navigation or success indicator
./scripts/wait-for.js --text "Welcome" --timeout 15000
if [ $? -ne 0 ]; then
    # Check for error message
    ./scripts/wait-for.js --text "Invalid credentials" --timeout 2000
    if [ $? -eq 0 ]; then
        echo "ERROR: Login failed - invalid credentials"
        ./scripts/screenshot.js
        exit 1
    fi
    echo "ERROR: Login result unclear"
    ./scripts/screenshot.js
    exit 1
fi

# 7. Export cookies for session reuse
./scripts/cookies-export.js > /tmp/session_cookies.json
echo "Login successful - cookies saved to /tmp/session_cookies.json"

# 8. Verify we're on the expected page
CURRENT_URL=$(./scripts/eval.js 'window.location.href')
if [[ "$CURRENT_URL" == *"/dashboard"* ]]; then
    echo "Successfully logged in and redirected to dashboard"
else
    echo "WARNING: Unexpected URL after login: $CURRENT_URL"
fi
```

**Key Points**:
- Always verify form presence before filling
- Check field values after filling (optional but safer)
- Handle multiple possible outcomes (success, error, timeout)
- Export session data for reuse
- Use screenshots for debugging failures

### Infinite Scroll Extraction

Extract all content from a page with infinite scroll loading:

```bash
# 1. Navigate to the page
./scripts/nav.js https://example.com/feed

# 2. Wait for initial content
./scripts/wait-for.js --selector ".post-item" --visible --timeout 10000

# 3. Initialize tracking
PREV_COUNT=0
SCROLL_ATTEMPTS=0
MAX_ATTEMPTS=50

# 4. Scroll loop until no more content loads
while [ $SCROLL_ATTEMPTS -lt $MAX_ATTEMPTS ]; do
    # Count current items
    CURRENT_COUNT=$(./scripts/eval.js 'document.querySelectorAll(".post-item").length')
    echo "Current items: $CURRENT_COUNT"

    # Check if we've loaded new items
    if [ "$CURRENT_COUNT" -eq "$PREV_COUNT" ]; then
        echo "No new items loaded, checking if at end..."
        # Verify we've reached the end
        END_MARKER=$(./scripts/eval.js 'document.querySelector(".end-of-feed") !== null')
        if [ "$END_MARKER" = "true" ]; then
            echo "Reached end of feed"
            break
        fi
    fi

    # Scroll to bottom
    ./scripts/eval.js 'window.scrollTo(0, document.body.scrollHeight)'

    # Wait for loading indicator to appear and disappear
    ./scripts/wait-for.js --selector ".loading-spinner" --visible --timeout 2000
    ./scripts/wait-for.js --selector ".loading-spinner" --disappear --timeout 10000

    PREV_COUNT=$CURRENT_COUNT
    ((SCROLL_ATTEMPTS++))
    sleep 0.5
done

# 5. Extract all content
./scripts/eval.js 'Array.from(document.querySelectorAll(".post-item")).map(el => ({
    title: el.querySelector(".title")?.textContent,
    author: el.querySelector(".author")?.textContent,
    timestamp: el.querySelector(".timestamp")?.textContent
}))' > /tmp/all_posts.json

# 6. Report results
FINAL_COUNT=$(./scripts/eval.js 'document.querySelectorAll(".post-item").length')
echo "Extraction complete: $FINAL_COUNT items saved to /tmp/all_posts.json"
```

**Key Points**:
- Track item count to detect when loading stops
- Set maximum attempts to prevent infinite loops
- Wait for loading indicators to ensure data is loaded
- Verify end-of-content markers when available
- Extract all data after scrolling is complete

### API Testing with Mocked Responses

Test frontend behavior with controlled backend responses:

```bash
# 1. Start network interception
./scripts/network-intercept-start.js

# 2. Set up mock for user API endpoint
./scripts/network-mock.js \
    --url "*/api/user" \
    --body '{"id":123,"name":"Test User","role":"admin"}' \
    --content-type "application/json"

# 3. Set up mock for failing endpoint
./scripts/network-mock.js \
    --url "*/api/data" \
    --status 500 \
    --body '{"error":"Internal Server Error"}' \
    --content-type "application/json"

# 4. Navigate to the application
./scripts/nav.js https://example.com/app

# 5. Wait for user profile to render with mocked data
./scripts/wait-for.js --text "Test User" --timeout 5000
if [ $? -ne 0 ]; then
    echo "ERROR: Mocked user data not rendered"
    ./scripts/screenshot.js
    ./scripts/network-intercept-stop.js
    exit 1
fi

# 6. Verify admin role is displayed
ROLE_TEXT=$(./scripts/eval.js 'document.querySelector(".user-role")?.textContent')
if [[ "$ROLE_TEXT" == *"admin"* ]]; then
    echo "SUCCESS: Admin role correctly displayed"
else
    echo "ERROR: Role not displayed correctly: $ROLE_TEXT"
    ./scripts/screenshot.js
fi

# 7. Trigger action that hits failing endpoint
./scripts/click.js "#load-data-button"

# 8. Verify error handling
./scripts/wait-for.js --text "Error loading data" --timeout 5000
if [ $? -ne 0 ]; then
    echo "ERROR: Error message not displayed for failed request"
    ./scripts/screenshot.js
    ./scripts/network-intercept-stop.js
    exit 1
fi

# 9. Verify error styling
ERROR_CLASS=$(./scripts/eval.js 'document.querySelector(".error-message")?.className')
if [[ "$ERROR_CLASS" == *"visible"* ]]; then
    echo "SUCCESS: Error properly displayed to user"
else
    echo "WARNING: Error element found but may not be visible"
fi

# 10. Clean up - stop interception
./scripts/network-intercept-stop.js
echo "API testing complete"

# 11. Optional: Test with different response
./scripts/network-intercept-start.js
./scripts/network-mock.js \
    --url "*/api/user" \
    --body '{"id":456,"name":"Regular User","role":"viewer"}' \
    --content-type "application/json"

# 12. Reload and verify different behavior
./scripts/eval.js 'window.location.reload()'
./scripts/wait-for.js --text "Regular User" --timeout 5000
./scripts/wait-for.js --selector ".admin-panel" --disappear --timeout 2000
if [ $? -eq 0 ]; then
    echo "SUCCESS: Admin panel hidden for regular user"
else
    echo "WARNING: Admin panel visibility not verified"
fi

./scripts/network-intercept-stop.js
```

**Key Points**:
- Start interception before navigation
- Set up all mocks before triggering requests
- Verify both success and error handling
- Check visual feedback (error messages, role-based UI)
- Always stop interception when done
- Test multiple scenarios by changing mocks

### Accessibility Audit

Comprehensive accessibility testing workflow:

```bash
# 1. Navigate to page
./scripts/nav.js https://example.com/product/123

# 2. Wait for page to fully load
./scripts/wait-for.js --selector "main" --visible --timeout 10000

# 3. Run automated accessibility checks
./scripts/a11y-check.js --selector "body" > /tmp/a11y_violations.json

# 4. Parse and count violations
VIOLATION_COUNT=$(cat /tmp/a11y_violations.json | jq '.violations | length')
echo "Found $VIOLATION_COUNT accessibility violations"

if [ "$VIOLATION_COUNT" -gt 0 ]; then
    echo "Violations by severity:"
    cat /tmp/a11y_violations.json | jq -r '.violations | group_by(.impact) |
        .[] | "\(.[0].impact): \(length)"'
fi

# 5. Inspect specific critical elements
echo "Inspecting form accessibility..."
FORM_A11Y=$(./scripts/a11y-inspect.js --selector "form#checkout")
echo "$FORM_A11Y" | jq -r '.role, .name, .properties' > /tmp/form_a11y.txt

# 6. Verify all form inputs have labels
UNLABELED_INPUTS=$(./scripts/eval.js 'Array.from(document.querySelectorAll("input, select, textarea"))
    .filter(el => !el.labels || el.labels.length === 0)
    .map(el => ({id: el.id, name: el.name, type: el.type}))')

if [ -n "$UNLABELED_INPUTS" ] && [ "$UNLABELED_INPUTS" != "[]" ]; then
    echo "ERROR: Found inputs without labels:"
    echo "$UNLABELED_INPUTS" | jq .
else
    echo "SUCCESS: All form inputs have labels"
fi

# 7. Check for keyboard navigation
echo "Testing keyboard navigation..."
./scripts/eval.js 'document.querySelector("body").focus()'
TAB_ORDER=$(./scripts/eval.js 'Array.from(document.querySelectorAll("a, button, input, select, textarea"))
    .filter(el => el.tabIndex >= 0 ||
                  (el.tabIndex === -1 && getComputedStyle(el).display !== "none"))
    .map((el, idx) => ({order: idx, tag: el.tagName, id: el.id, tabIndex: el.tabIndex}))')

echo "Interactive elements in tab order:"
echo "$TAB_ORDER" | jq . > /tmp/tab_order.json

# 8. Verify focus indicators
./scripts/eval.js 'document.querySelector("a").focus()'
sleep 0.2
./scripts/screenshot.js  # Visual verification of focus ring

FOCUS_STYLE=$(./scripts/eval.js 'window.getComputedStyle(document.activeElement).outline')
if [ "$FOCUS_STYLE" = "none" ] || [ -z "$FOCUS_STYLE" ]; then
    echo "WARNING: No visible focus indicator detected"
else
    echo "SUCCESS: Focus indicator present: $FOCUS_STYLE"
fi

# 9. Check color contrast (example for specific element)
CONTRAST_DATA=$(./scripts/eval.js 'const el = document.querySelector(".primary-button");
    const style = window.getComputedStyle(el);
    ({
        color: style.color,
        background: style.backgroundColor,
        fontSize: style.fontSize
    })')

echo "Color contrast data for primary button:"
echo "$CONTRAST_DATA" | jq .

# 10. Generate report
cat > /tmp/a11y_report.md << EOF
# Accessibility Audit Report

**Page**: $(./scripts/eval.js 'window.location.href')
**Date**: $(date)

## Summary
- Total violations: $VIOLATION_COUNT
- Unlabeled inputs: $(echo "$UNLABELED_INPUTS" | jq 'length')

## Detailed Violations
$(cat /tmp/a11y_violations.json | jq -r '.violations[] | "### \(.impact | ascii_upcase): \(.help)\n\n\(.description)\n\nAffected elements: \(.nodes | length)\n"')

## Tab Order
See /tmp/tab_order.json for complete tab order analysis

## Recommendations
$(cat /tmp/a11y_violations.json | jq -r '.violations[] | "- \(.help): \(.helpUrl)"')
EOF

echo "Full report saved to /tmp/a11y_report.md"
```

**Key Points**:
- Run automated checks as a baseline
- Inspect critical interactive elements individually
- Verify keyboard navigation and focus indicators
- Check for common issues (unlabeled inputs, contrast)
- Generate structured reports for remediation
- Use Read tool to analyze report content

### Error Recovery with Retry Pattern

Robust workflow with retry logic and debugging:

```bash
#!/usr/bin/env bash
set -euo pipefail

MAX_RETRIES=3
RETRY_COUNT=0
SUCCESS=false

# Function to capture debug info
capture_debug_info() {
    local attempt=$1
    local step=$2
    echo "Capturing debug info for attempt $attempt, step: $step"

    ./scripts/screenshot.js
    mv screenshot-*.png "/tmp/debug_attempt${attempt}_${step}.png"

    ./scripts/eval.js 'window.location.href' > "/tmp/url_attempt${attempt}.txt"
    ./scripts/eval.js 'document.title' > "/tmp/title_attempt${attempt}.txt"
}

# Main workflow with retry
while [ $RETRY_COUNT -lt $MAX_RETRIES ] && [ "$SUCCESS" = false ]; do
    echo "Attempt $((RETRY_COUNT + 1)) of $MAX_RETRIES"

    # Step 1: Navigate
    if ! ./scripts/nav.js https://flaky-site.com/form; then
        echo "Navigation failed"
        capture_debug_info $RETRY_COUNT "navigation"
        ((RETRY_COUNT++))
        sleep 2
        continue
    fi

    # Step 2: Wait for form
    if ! ./scripts/wait-for.js --selector "form#contact" --visible --timeout 10000; then
        echo "Form not found"
        capture_debug_info $RETRY_COUNT "form_wait"
        ((RETRY_COUNT++))
        sleep 2
        continue
    fi

    # Step 3: Fill form
    if ! ./scripts/fill-form.js \
        --field "#name" --value "Test User" \
        --field "#email" --value "test@example.com"; then
        echo "Form filling failed"
        capture_debug_info $RETRY_COUNT "form_fill"
        ((RETRY_COUNT++))
        sleep 2
        continue
    fi

    # Step 4: Submit
    if ! ./scripts/click.js "button[type=submit]"; then
        echo "Submit click failed"
        capture_debug_info $RETRY_COUNT "submit"
        ((RETRY_COUNT++))
        sleep 2
        continue
    fi

    # Step 5: Verify success
    if ./scripts/wait-for.js --text "Thank you" --timeout 15000; then
        echo "SUCCESS: Form submitted successfully"
        SUCCESS=true
    else
        echo "Success message not found"
        capture_debug_info $RETRY_COUNT "verify_success"
        ((RETRY_COUNT++))
        sleep 2
    fi
done

if [ "$SUCCESS" = true ]; then
    echo "Workflow completed successfully after $((RETRY_COUNT + 1)) attempt(s)"
    exit 0
else
    echo "ERROR: Workflow failed after $MAX_RETRIES attempts"
    echo "Debug screenshots saved to /tmp/debug_attempt*.png"
    echo "Check logs and screenshots for details"
    exit 1
fi
```

**Key Points**:
- Wrap each step in error checking
- Capture screenshots and context on failures
- Use exponential backoff or fixed delays between retries
- Log attempt numbers and failure points
- Clean up on success, preserve debugging data on failure

---

## Validation Patterns

### Element Verification Before Click

Always verify element exists and is clickable:

```bash
# Check element exists
ELEMENT_EXISTS=$(./scripts/eval.js 'document.querySelector("#submit-btn") !== null')
if [ "$ELEMENT_EXISTS" != "true" ]; then
    echo "ERROR: Element not found"
    exit 1
fi

# Check element is visible
./scripts/wait-for.js --selector "#submit-btn" --visible --timeout 5000

# Check element is enabled
IS_DISABLED=$(./scripts/eval.js 'document.querySelector("#submit-btn").disabled')
if [ "$IS_DISABLED" = "true" ]; then
    echo "ERROR: Button is disabled"
    exit 1
fi

# Now safe to click
./scripts/click.js "#submit-btn"
```

### Navigation Verification

Verify navigation completed successfully:

```bash
# Before navigation
EXPECTED_URL="https://example.com/page"
./scripts/nav.js "$EXPECTED_URL"

# Wait for navigation to complete
sleep 1

# Verify URL
ACTUAL_URL=$(./scripts/eval.js 'window.location.href')
if [[ "$ACTUAL_URL" != "$EXPECTED_URL"* ]]; then
    echo "ERROR: Navigation failed. Expected: $EXPECTED_URL, Got: $ACTUAL_URL"
    ./scripts/screenshot.js
    exit 1
fi

# Verify page loaded
./scripts/wait-for.js --selector "body" --visible --timeout 5000
```

### Form Fill Verification

Verify form fields contain expected values:

```bash
# Fill form
./scripts/fill-form.js \
    --field "#email" --value "user@example.com" \
    --field "#phone" --value "555-1234"

# Verify email
EMAIL_VALUE=$(./scripts/eval.js 'document.querySelector("#email").value')
if [ "$EMAIL_VALUE" != "user@example.com" ]; then
    echo "ERROR: Email not filled correctly: $EMAIL_VALUE"
    exit 1
fi

# Verify phone
PHONE_VALUE=$(./scripts/eval.js 'document.querySelector("#phone").value')
if [ "$PHONE_VALUE" != "555-1234" ]; then
    echo "ERROR: Phone not filled correctly: $PHONE_VALUE"
    exit 1
fi

echo "SUCCESS: All fields verified"
```

### Screenshot-Based Debugging

Use screenshots to debug visual issues:

```bash
# Capture baseline
./scripts/screenshot.js
mv screenshot-*.png /tmp/baseline.png

# Perform action
./scripts/click.js "#toggle-theme"
sleep 0.5

# Capture after action
./scripts/screenshot.js
mv screenshot-*.png /tmp/after_action.png

# Use Read tool to analyze both screenshots
echo "Compare /tmp/baseline.png and /tmp/after_action.png"
echo "Check for visual changes, error messages, or unexpected UI states"

# Optionally capture specific element
./scripts/screenshot.js --selector ".error-message"
```

---

## Best Practices

1. **Always validate before action**: Check element exists, is visible, and is enabled
2. **Wait for dynamic content**: Use `wait-for.js` instead of fixed `sleep` delays
3. **Handle multiple outcomes**: Check for both success and error conditions
4. **Capture debugging data**: Screenshots and DOM state on failures
5. **Use retries for flaky operations**: Network requests, dynamic content loading
6. **Verify final state**: Don't assume success, check for confirmation
7. **Clean up resources**: Stop network interception, close modals, reset state
8. **Integrate with Read tool**: Save JSON/HTML data and analyze with Read tool
