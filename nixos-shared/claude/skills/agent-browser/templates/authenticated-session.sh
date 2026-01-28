#!/usr/bin/env nix
#! nix shell nixpkgs#bash nixpkgs#jq --command bash
set -euo pipefail

# Authenticated Session Template
# Usage: ./authenticated-session.sh <login-url> [state-file]

LOGIN_URL="${1:-}"
STATE_FILE="${2:-~/.auth/session.json}"

if [ -z "$LOGIN_URL" ]; then
  echo "Usage: $0 <login-url> [state-file]"
  echo "Example: $0 https://app.example.com/login ~/.auth/app.json"
  exit 1
fi

# Expand tilde
STATE_FILE="${STATE_FILE/#\~/$HOME}"
STATE_DIR=$(dirname "$STATE_FILE")

echo "=== Authenticated Session Workflow ==="
echo "Login URL: $LOGIN_URL"
echo "State file: $STATE_FILE"
echo

# Check if state file exists
if [ -f "$STATE_FILE" ]; then
  echo "[1/3] Loading existing session..."
  agent-browser state load "$STATE_FILE"
  
  # Try to navigate to protected page
  # Extract base URL from login URL
  BASE_URL=$(echo "$LOGIN_URL" | sed -E 's|(https?://[^/]+).*|\1|')
  DASHBOARD_URL="${BASE_URL}/dashboard"
  
  agent-browser open "$DASHBOARD_URL"
  agent-browser wait --load networkidle
  
  # Check if we're still logged in
  CURRENT_URL=$(agent-browser get url)
  
  if [[ "$CURRENT_URL" == *"/login"* ]]; then
    echo "  - Session expired, re-authenticating..."
    rm -f "$STATE_FILE"
  else
    echo "  - ✓ Session still valid!"
    echo
    echo "[2/3] Taking snapshot..."
    agent-browser snapshot -i --json > dashboard-snapshot.json
    echo "  - Saved to: dashboard-snapshot.json"
    
    echo
    echo "[3/3] Session ready for use"
    echo "Use this session:"
    echo "  export AGENT_BROWSER_STATE=$STATE_FILE"
    echo "  agent-browser state load \$AGENT_BROWSER_STATE"
    echo "  agent-browser open $DASHBOARD_URL"
    
    agent-browser close
    exit 0
  fi
fi

# State file doesn't exist or session expired - login required
echo "[1/3] Navigating to login page..."
agent-browser open "$LOGIN_URL"
agent-browser wait --load networkidle

echo
echo "[2/3] Analyzing login form..."
SNAPSHOT=$(agent-browser snapshot -i --json)
echo "$SNAPSHOT" | jq -r '.data.snapshot' | grep -E "(textbox|button)" | head -5

echo
echo "=== Manual Login Required ==="
echo
echo "The browser is now at the login page."
echo "You need to:"
echo "  1. Run with --headed mode to see the browser"
echo "  2. Manually complete the login (including 2FA if needed)"
echo "  3. Navigate to the dashboard/home page"
echo "  4. Run this script again to save the state"
echo
echo "Quick manual login:"
echo "  agent-browser --headed open $LOGIN_URL"
echo "  # Complete login in browser window"
echo "  agent-browser state save $STATE_FILE"
echo
echo "Alternatively, provide credentials via environment variables:"
echo "  export LOGIN_USERNAME='your-username'"
echo "  export LOGIN_PASSWORD='your-password'"
echo "  # Then re-run this script"

# Check if credentials provided
if [ -n "${LOGIN_USERNAME:-}" ] && [ -n "${LOGIN_PASSWORD:-}" ]; then
  echo
  echo "[2/3] Attempting automated login..."
  
  # Fill credentials (assuming typical form structure)
  agent-browser fill "@e1" "$LOGIN_USERNAME" || {
    echo "  - Username field not found at @e1"
    exit 1
  }
  
  agent-browser fill "@e2" "$LOGIN_PASSWORD" || {
    echo "  - Password field not found at @e2"
    exit 1
  }
  
  # Submit
  agent-browser click "@e3" || {
    echo "  - Submit button not found at @e3"
    exit 1
  }
  
  # Wait for login to complete
  agent-browser wait --load networkidle
  agent-browser wait 2000  # Additional wait for redirects
  
  # Check if login successful
  CURRENT_URL=$(agent-browser get url)
  
  if [[ "$CURRENT_URL" == *"/login"* ]]; then
    echo "  - ✗ Login failed - still on login page"
    echo "  - Check credentials or try manual login"
    exit 1
  fi
  
  echo "  - ✓ Login successful!"
  
  echo
  echo "[3/3] Saving session state..."
  mkdir -p "$STATE_DIR"
  agent-browser state save "$STATE_FILE"
  chmod 600 "$STATE_FILE"
  echo "  - ✓ State saved to: $STATE_FILE"
  
  # Take snapshot of logged-in page
  agent-browser snapshot -i --json > dashboard-snapshot.json
  echo "  - ✓ Dashboard snapshot: dashboard-snapshot.json"
  
  echo
  echo "=== Session Established ==="
  echo "Reuse this session:"
  echo "  agent-browser state load $STATE_FILE"
  echo "  agent-browser open $DASHBOARD_URL"
  
  agent-browser close
else
  # No credentials - exit for manual login
  agent-browser close
  exit 1
fi
