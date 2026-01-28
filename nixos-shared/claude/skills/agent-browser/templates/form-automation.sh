#!/usr/bin/env nix
#! nix shell nixpkgs#bash nixpkgs#jq --command bash
set -euo pipefail

# Form Automation Template
# Usage: ./form-automation.sh <form-url>

URL="${1:-}"
if [ -z "$URL" ]; then
  echo "Usage: $0 <form-url>"
  echo "Example: $0 https://example.com/contact"
  exit 1
fi

echo "=== Form Automation Workflow ==="
echo "Target URL: $URL"
echo

# Step 1: Navigate to form
echo "[1/5] Navigating to form..."
agent-browser open "$URL"

# Wait for page load
agent-browser wait --load networkidle

# Step 2: Get form structure
echo "[2/5] Analyzing form structure..."
SNAPSHOT=$(agent-browser snapshot -i --json)

# Parse snapshot to identify form fields
echo "Form elements found:"
echo "$SNAPSHOT" | jq -r '.data.snapshot' | grep -E "(textbox|button|checkbox|select)" | head -10

# Step 3: Fill form fields
echo
echo "[3/5] Filling form fields..."

# Example: Extract refs for common field types
# In practice, you'd parse the JSON to find specific fields
# For demonstration, assuming typical contact form structure

# Name field (typically @e1)
echo "  - Filling name..."
agent-browser fill "@e1" "John Doe" 2>/dev/null || echo "  - Name field not found"

# Email field (typically @e2)
echo "  - Filling email..."
agent-browser fill "@e2" "john@example.com" 2>/dev/null || echo "  - Email field not found"

# Message field (typically @e3)
echo "  - Filling message..."
agent-browser fill "@e3" "This is an automated test message." 2>/dev/null || echo "  - Message field not found"

# Step 4: Validate before submission
echo
echo "[4/5] Validating form..."
agent-browser screenshot "form-filled.png"
echo "  - Screenshot saved: form-filled.png"

# Check if submit button is enabled
SUBMIT_ENABLED=$(agent-browser is enabled "@e4" --json 2>/dev/null | jq -r '.data // false')
if [ "$SUBMIT_ENABLED" = "true" ]; then
  echo "  - Submit button is enabled ✓"
else
  echo "  - Submit button is disabled ✗"
fi

# Step 5: Submit form
echo
echo "[5/5] Submitting form..."
agent-browser click "@e4" 2>/dev/null || {
  echo "  - Submit button not found at @e4"
  echo "  - Available buttons:"
  echo "$SNAPSHOT" | jq -r '.data.snapshot' | grep "button"
  exit 1
}

# Wait for submission
agent-browser wait --load networkidle

# Capture result
echo
echo "=== Submission Complete ==="
agent-browser snapshot -i > form-result.txt
agent-browser screenshot "form-result.png"

# Check for success/error messages
if agent-browser wait --text "success" --timeout 2000 2>/dev/null; then
  echo "✓ Form submitted successfully"
elif agent-browser wait --text "error" --timeout 2000 2>/dev/null; then
  echo "✗ Form submission error detected"
else
  echo "⚠ Result unclear - check screenshots"
fi

echo
echo "Output files:"
echo "  - form-filled.png   (before submission)"
echo "  - form-result.png   (after submission)"
echo "  - form-result.txt   (page snapshot)"

# Cleanup
agent-browser close
