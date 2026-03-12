# Complete Command Reference

This document contains the full command reference for agent-browser. For quick start and common commands, see the main SKILL.md.

## Navigation

```bash
agent-browser open <url>  # Navigate to URL (aliases: goto, navigate)
                          # Supports: https://, http://, file://, about:, data://
                          # Auto-prepends https:// if no protocol given
agent-browser back  # Go back
agent-browser forward  # Go forward
agent-browser reload  # Reload page
agent-browser close  # Close browser (aliases: quit, exit)
agent-browser connect 9222  # Connect to browser via CDP port
```

## Device listing (iOS)

```bash
agent-browser device list  # List available iOS Simulators and devices
```

See [ios-simulator.md](ios-simulator.md) for iOS setup and usage.

## Snapshot (page analysis)

```bash
agent-browser snapshot  # Full accessibility tree
agent-browser snapshot -i  # Interactive elements only (recommended)
agent-browser snapshot -i -C  # Include cursor-interactive elements (divs with onclick/cursor:pointer)
agent-browser snapshot -c  # Compact output
agent-browser snapshot -d 3  # Limit depth to 3
agent-browser snapshot -s "#main"  # Scope to CSS selector
agent-browser snapshot -i -c -d 5  # Combine options
```

| Option | Description |
|---|---|
| `-i, --interactive` | Only interactive elements (buttons, links, inputs) |
| `-C, --cursor` | Include cursor-interactive elements (cursor:pointer, onclick, tabindex) |
| `-c, --compact` | Remove empty structural elements |
| `-d, --depth` | Limit tree depth |
| `-s, --selector` | Scope to CSS selector |

## Interactions (use @refs from snapshot)

```bash
agent-browser click @e1  # Click
agent-browser click @e1 --new-tab  # Click, open in new tab
agent-browser dblclick @e1  # Double-click
agent-browser focus @e1  # Focus element
agent-browser fill @e2 "text"  # Clear and type
agent-browser type @e2 "text"  # Type without clearing
agent-browser keyboard type "text"  # Type at current focus (no selector)
agent-browser keyboard inserttext "text"  # Insert text without key events
agent-browser press Enter  # Press key (alias: key)
agent-browser press Control+a  # Key combination
agent-browser keydown Shift  # Hold key down
agent-browser keyup Shift  # Release key
agent-browser hover @e1  # Hover
agent-browser check @e1  # Check checkbox
agent-browser uncheck @e1  # Uncheck checkbox
agent-browser select @e1 "value"  # Select dropdown option
agent-browser select @e1 "a" "b"  # Select multiple options
agent-browser scroll down 500  # Scroll page (default: down 300px)
agent-browser scroll down 500 --selector ".container"  # Scroll within element
agent-browser scrollintoview @e1  # Scroll element into view (alias: scrollinto)
agent-browser drag @e1 @e2  # Drag and drop
agent-browser upload @e1 file.pdf  # Upload files
# iOS-specific (use with -p ios):
agent-browser -p ios tap @e1  # Tap element (alias for click, touch-semantics)
agent-browser -p ios swipe up  # Swipe gesture (up/down/left/right)
agent-browser -p ios swipe down 500  # Swipe with distance in pixels
```

## Get information

```bash
agent-browser get text @e1  # Get element text
agent-browser get html @e1  # Get innerHTML
agent-browser get value @e1  # Get input value
agent-browser get attr @e1 href  # Get attribute
agent-browser get title  # Get page title
agent-browser get url  # Get current URL
agent-browser get count ".item"  # Count matching elements
agent-browser get box @e1  # Get bounding box
agent-browser get styles @e1  # Get computed styles (font, color, bg, etc.)
```

## Check state

```bash
agent-browser is visible @e1  # Check if visible
agent-browser is enabled @e1  # Check if enabled
agent-browser is checked @e1  # Check if checked
```

## Screenshots & PDF

```bash
agent-browser screenshot  # Save to a temporary directory
agent-browser screenshot path.png  # Save to a specific path
agent-browser screenshot --full  # Full page
agent-browser screenshot --annotate ./page.png  # Annotated with numbered element labels ([N] → @eN)
agent-browser pdf output.pdf  # Save as PDF
```

Annotated screenshots overlay numbered labels on interactive elements and also cache refs, so you can interact with elements immediately after without a separate snapshot.

## Video recording

```bash
agent-browser record start ./demo.webm  # Start recording (uses current URL + state)
agent-browser click @e1  # Perform actions
agent-browser record stop  # Stop and save video
agent-browser record restart ./take2.webm  # Stop current + start new recording
```

Recording creates a fresh context but preserves cookies/storage from your session. If no URL is provided, it automatically returns to your current page. For smooth demos, explore first, then start recording.

## Wait

```bash
agent-browser wait @e1  # Wait for element
agent-browser wait 2000  # Wait milliseconds
agent-browser wait --text "Success"  # Wait for text (or -t)
agent-browser wait --url "**/dashboard"  # Wait for URL pattern (or -u)
agent-browser wait --load networkidle  # Wait for network idle (or -l)
agent-browser wait --fn "window.ready"  # Wait for JS condition (or -f)
agent-browser wait --download [path]  # Wait for download completion
```

## Mouse control

```bash
agent-browser mouse move 100 200  # Move mouse
agent-browser mouse down left  # Press button
agent-browser mouse up left  # Release button
agent-browser mouse wheel 100  # Scroll wheel
```

## Semantic locators (alternative to refs)

```bash
agent-browser find role button click --name "Submit"
agent-browser find text "Sign In" click
agent-browser find text "Sign In" click --exact  # Exact match only
agent-browser find label "Email" fill "user@test.com"
agent-browser find placeholder "Search" type "query"
agent-browser find alt "Logo" click
agent-browser find title "Close" click
agent-browser find testid "submit-btn" click
agent-browser find first ".item" click
agent-browser find last ".item" click
agent-browser find nth 2 "a" hover
```

## Diffing

```bash
agent-browser diff snapshot  # Compare current snapshot to last in session
agent-browser diff snapshot --baseline saved.txt  # Compare against saved file
agent-browser diff screenshot --baseline before.png  # Pixel-level visual diff
agent-browser diff url https://a.com https://b.com  # Compare two URLs
agent-browser diff url https://a.com https://b.com --screenshot  # Compare with visual diff
```

| Option | Description |
|---|---|
| `-s, --selector` | Scope comparison to CSS selector |
| `-c, --compact` | Compact diff output |
| `-d, --depth` | Limit tree depth |
| `-t, --threshold <0-1>` | Pixel diff sensitivity (screenshot mode) |
| `-o, --output <path>` | Save diff output to file |
| `--full` | Full page screenshot for visual diff |
| `--wait-until` | Wait condition before comparing |

See [diffing.md](diffing.md) for patterns and examples.

## Auth Vault

Encrypted credential storage for automated login flows:

```bash
agent-browser auth save myapp --url https://app.com \
  --username user --password-stdin  # Save credentials (reads password from stdin)
agent-browser auth save myapp --url https://app.com \
  --username-selector "#email" --password-selector "#pass" \
  --submit-selector "#login"  # Custom selectors
agent-browser auth login myapp  # Login using saved credentials
agent-browser auth list  # List saved profiles
agent-browser auth show myapp  # Show profile metadata (no passwords)
agent-browser auth delete myapp  # Delete profile
```

Credentials encrypted with AES-256-GCM. Key stored at `~/.agent-browser/.encryption-key` (chmod 600).

## Profiler

Chrome DevTools performance profiling:

```bash
agent-browser profiler start  # Start profiling (default categories)
agent-browser profiler start --categories "devtools.timeline,v8.execute"  # Specific categories
agent-browser profiler stop profile.json  # Stop and save
```

Output: Chrome Trace Event format JSON. View in Chrome DevTools Performance panel, Perfetto UI, or chrome://tracing. Chromium-only, 5M event cap, 30s timeout.

## Browser settings

```bash
agent-browser set viewport 1920 1080  # Set viewport size
agent-browser set device "iPhone 14"  # Emulate device
agent-browser set geo 37.7749 -122.4194  # Set geolocation (alias: geolocation)
agent-browser set offline on  # Toggle offline mode
agent-browser set headers '{"X-Key":"v"}'  # Extra HTTP headers
agent-browser set credentials user pass  # HTTP basic auth (alias: auth)
agent-browser set media dark  # Emulate color scheme
agent-browser set media light reduced-motion  # Light mode + reduced motion
```

## Cookies & Storage

```bash
agent-browser cookies  # Get all cookies
agent-browser cookies set name value  # Set cookie
agent-browser cookies clear  # Clear cookies
agent-browser storage local  # Get all localStorage
agent-browser storage local key  # Get specific key
agent-browser storage local set k v  # Set value
agent-browser storage local clear  # Clear all
agent-browser storage session  # Get all sessionStorage
agent-browser storage session key  # Get specific key
agent-browser storage session set k v  # Set value
agent-browser storage session clear  # Clear all
```

## Network

```bash
agent-browser network route <url>  # Intercept requests
agent-browser network route <url> --abort  # Block requests
agent-browser network route <url> --body '{}'  # Mock response
agent-browser network unroute [url]  # Remove routes
agent-browser network requests  # View tracked requests
agent-browser network requests --filter api  # Filter requests
agent-browser network requests --clear  # Clear request log
```

## Tabs & Windows

```bash
agent-browser tab  # List tabs
agent-browser tab new [url]  # New tab
agent-browser tab 2  # Switch to tab by index
agent-browser tab close  # Close current tab
agent-browser tab close 2  # Close tab by index
agent-browser window new  # New window
```

## Frames

```bash
agent-browser frame "#iframe"  # Switch to iframe
agent-browser frame main  # Back to main frame
```

## Dialogs

```bash
agent-browser dialog accept [text]  # Accept dialog
agent-browser dialog dismiss  # Dismiss dialog
```

## Downloads

```bash
agent-browser download @e1 ./file.pdf  # Click element to trigger download
agent-browser wait --download [path]  # Wait for download completion
```

Options: `--download-path <dir>` or `AGENT_BROWSER_DOWNLOAD_PATH` env var to set default directory.

## JavaScript

```bash
agent-browser eval "document.title"  # Run JavaScript
agent-browser eval --stdin <<< 'document.title'  # Read JS from stdin
agent-browser eval -b "ZG9jdW1lbnQudGl0bGU="  # Run base64-encoded JavaScript
```

## Global options

```bash
agent-browser --session <name> ...        # Isolated browser session (in-memory)
agent-browser --session-name <name> ...   # Auto-save/restore state across restarts
agent-browser --profile <path> ...        # Persistent browser profile directory
agent-browser --state <path> ...          # Load storage state from JSON file
agent-browser --json ...                  # JSON output for parsing
agent-browser --headed ...                # Show browser window (not headless)
agent-browser --full, -f ...              # Full page screenshot
agent-browser --annotate ...              # Annotated screenshot with numbered labels
agent-browser --cdp <port|url> ...        # Connect via Chrome DevTools Protocol
agent-browser --auto-connect ...          # Auto-discover and connect to running Chrome
agent-browser -p, --provider <name> ...   # Browser provider (ios, browserbase, kernel, browseruse)
agent-browser --device <name> ...         # Device name for iOS ("iPhone 16 Pro")
agent-browser --proxy <url> ...           # Use proxy server
agent-browser --proxy-bypass <hosts> ...  # Hosts to bypass proxy
agent-browser --headers <json> ...        # HTTP headers scoped to URL's origin
agent-browser --user-agent <ua> ...       # Custom User-Agent string
agent-browser --executable-path <path>    # Custom browser executable
agent-browser --extension <path> ...      # Load browser extension (repeatable)
agent-browser --args <args> ...           # Browser launch args (comma separated)
agent-browser --ignore-https-errors ...   # Ignore HTTPS certificate errors
agent-browser --allow-file-access ...     # Allow file:// access to local files (Chromium only)
agent-browser --color-scheme <scheme> ... # Emulate color scheme (dark/light/no-preference)
agent-browser --download-path <path> ...  # Default download directory
agent-browser --engine <name> ...         # Browser engine (chrome, lightpanda)
agent-browser --native ...                # Use experimental Rust daemon (no Node.js)
agent-browser --content-boundaries ...    # Wrap output with boundary markers (LLM safety)
agent-browser --max-output <chars> ...    # Truncate output at character limit
agent-browser --allowed-domains <list>    # Comma-separated domain allowlist
agent-browser --action-policy <path> ...  # Action policy JSON file
agent-browser --confirm-actions <list>    # Actions requiring confirmation (e.g. eval,download)
agent-browser --config <path> ...         # Custom config file path
agent-browser --debug ...                 # Debug output
agent-browser --help, -h                  # Show help
agent-browser --version, -V               # Show version
agent-browser <command> --help            # Show detailed help for a command
```

### Proxy support

```bash
agent-browser --proxy http://proxy.com:8080 open example.com
agent-browser --proxy http://user:pass@proxy.com:8080 open example.com
agent-browser --proxy socks5://proxy.com:1080 open example.com
agent-browser --proxy http://proxy.com:8080 --proxy-bypass "localhost,127.0.0.1" open example.com
```

## Sessions (parallel browsers)

```bash
agent-browser --session test1 open site-a.com
agent-browser --session test2 open site-b.com
agent-browser session list
```

See [session-management.md](session-management.md) for advanced session patterns.

## JSON output (for parsing)

Add `--json` for machine-readable output:

```bash
agent-browser snapshot -i --json
agent-browser get text @e1 --json
```

## Debugging

```bash
agent-browser --headed open example.com  # Show browser window
agent-browser --cdp 9222 snapshot  # Connect via CDP port
agent-browser connect 9222  # Alternative: connect command
agent-browser console  # View console messages
agent-browser console --clear  # Clear console
agent-browser errors  # View page errors
agent-browser errors --clear  # Clear errors
agent-browser highlight @e1  # Highlight element
agent-browser trace start  # Start recording trace
agent-browser trace stop trace.zip  # Stop and save trace
agent-browser record start ./debug.webm  # Record video from current page
agent-browser record stop  # Save recording
agent-browser record restart ./take2.webm  # Stop current + start new
```

## Action Confirmation

For gated actions (when using `--confirm-actions`):

```bash
agent-browser confirm <confirmation-id>  # Approve a gated action
agent-browser deny <confirmation-id>  # Reject a gated action
```

60-second auto-deny timeout. See [security.md](security.md) for setup.

## Configuration files

Config priority (lowest to highest): config file < env vars < CLI flags.

```bash
~/.agent-browser/config.json  # User-level config
./agent-browser.json  # Project-level config
```

## Environment variables

```bash
AGENT_BROWSER_SESSION="mysession"          # Default session name (in-memory)
AGENT_BROWSER_SESSION_NAME="twitter"       # Auto-save/load state persistence name
AGENT_BROWSER_PROFILE="~/.app-profile"    # Persistent browser profile directory
AGENT_BROWSER_ENCRYPTION_KEY="<64-hex>"   # AES-256-GCM encryption key for state files
AGENT_BROWSER_STATE_EXPIRE_DAYS=7         # Auto-delete states older than N days (default: 30)
AGENT_BROWSER_EXECUTABLE_PATH="/path/chrome"  # Custom browser path
AGENT_BROWSER_EXTENSIONS="/ext1,/ext2"    # Comma-separated extension paths
AGENT_BROWSER_PROVIDER="browserbase"      # Cloud browser provider (browserbase, browseruse, kernel, ios)
AGENT_BROWSER_IOS_DEVICE="iPhone 16 Pro"  # iOS device name (when PROVIDER=ios)
AGENT_BROWSER_IOS_UDID="<udid>"           # iOS device UDID (alternative to device name)
AGENT_BROWSER_STREAM_PORT="9223"          # WebSocket streaming port
AGENT_BROWSER_HOME="/path/to/agent-browser"  # Custom install location (for daemon.js)
AGENT_BROWSER_HEADED=1                    # Show browser window
AGENT_BROWSER_AUTO_CONNECT=1              # Auto-discover Chrome instances
AGENT_BROWSER_ALLOW_FILE_ACCESS=1         # Allow file:// URLs
AGENT_BROWSER_COLOR_SCHEME="dark"         # Emulate color scheme (dark/light/no-preference)
AGENT_BROWSER_DOWNLOAD_PATH="/tmp/dl"     # Default download directory
AGENT_BROWSER_DEFAULT_TIMEOUT=25000       # Playwright timeout in ms
AGENT_BROWSER_ENGINE="chrome"             # Browser engine (chrome, lightpanda)
AGENT_BROWSER_NATIVE=1                    # Use experimental Rust daemon
AGENT_BROWSER_CONTENT_BOUNDARIES=1        # Wrap output with boundary markers
AGENT_BROWSER_MAX_OUTPUT=10000            # Truncate output at char limit
AGENT_BROWSER_ALLOWED_DOMAINS="a.com,*.b.com"  # Domain allowlist
AGENT_BROWSER_ACTION_POLICY="policy.json" # Action policy file
AGENT_BROWSER_CONFIRM_ACTIONS="eval,download"  # Actions requiring confirmation
AGENT_BROWSER_ANNOTATE=1                  # Annotated screenshots by default
AGENT_BROWSER_DEBUG=1                     # Debug output
```

## HTTPS Certificate Errors

For sites with self-signed or invalid certificates:

```bash
agent-browser open https://localhost:8443 --ignore-https-errors
```
