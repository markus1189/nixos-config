---
name: agent-browser
description: Automates browser interactions for web testing, form filling, screenshots, and data extraction. Use when the user needs to navigate websites, interact with web pages, fill forms, take screenshots, test web applications, or extract information from web pages.
allowed-tools: Bash(agent-browser:*)
---

# Browser Automation with agent-browser

> **NixOS**: All `agent-browser` commands require the Nix prefix:
> ```bash
> nix run github:numtide/llm-agents.nix#agent-browser -- <command> [args...]
> ```
> Always pass `--headed` on the first command to open a visible browser window.

## Quick start

```bash
agent-browser --headed open <url>   # Navigate (visible browser)
agent-browser snapshot -i           # Get interactive elements with refs
agent-browser click @e1             # Click element by ref
agent-browser fill @e2 "text"       # Fill input by ref
agent-browser screenshot page.png   # Take screenshot
agent-browser close                 # Close browser
```

## Core workflow

Always open the browser in `--headed` mode so the user can pair browse.

1. Navigate: `agent-browser --headed open <url>`
2. Snapshot: `agent-browser snapshot -i` (returns elements with refs like `@e1`, `@e2`)
3. Interact using refs from the snapshot
4. Re-snapshot after navigation or significant DOM changes

## Common Commands

### Navigation & Snapshots
```bash
agent-browser --headed open <url>   # Navigate (auto-prepends https://)
agent-browser back/forward/reload   # Navigation controls
agent-browser snapshot -i           # Interactive elements with refs (recommended)
agent-browser snapshot -i -C        # Also include cursor-interactive elements
agent-browser snapshot -s "#main"   # Scope to CSS selector
agent-browser close                 # Close browser
```

### Interactions (use @refs from snapshot)
```bash
agent-browser click @e1             # Click element
agent-browser dblclick @e1          # Double-click
agent-browser fill @e2 "text"       # Clear and type
agent-browser type @e2 "text"       # Type without clearing
agent-browser keyboard type "text"  # Type at current focus
agent-browser press Enter           # Press key
agent-browser hover @e1             # Hover element
agent-browser check @e1             # Check checkbox
agent-browser select @e1 "value"    # Select dropdown option
agent-browser upload @e1 file.pdf   # Upload file
agent-browser scroll down 500       # Scroll page
```

### Get Information
```bash
agent-browser get text @e1          # Get element text
agent-browser get url               # Get current URL
agent-browser get title             # Get page title
agent-browser get value @e1         # Get input value
agent-browser get attr @e1 href     # Get attribute
```

### Wait & Screenshots
```bash
agent-browser wait @e1              # Wait for element
agent-browser wait 2000             # Wait milliseconds
agent-browser wait --text "Success" # Wait for text
agent-browser wait --load networkidle  # Wait for network idle
agent-browser screenshot page.png   # Take screenshot
agent-browser screenshot --annotate page.png  # With numbered element labels
agent-browser screenshot --full     # Full page screenshot
```

## Example: Form submission

```bash
agent-browser --headed open https://example.com/form
agent-browser snapshot -i
# Output: textbox "Email" [ref=e1], textbox "Password" [ref=e2], button "Submit" [ref=e3]
agent-browser fill @e1 "user@example.com"
agent-browser fill @e2 "password123"
agent-browser click @e3
agent-browser wait --load networkidle
agent-browser snapshot -i  # Check result
```

## Deep-dive documentation

| Reference | Description |
|-----------|-------------|
| [references/command-reference.md](references/command-reference.md) | Complete command reference: all options, mouse control, semantic locators, network, tabs, frames, JS eval, cookies/storage, browser settings |
| [references/snapshot-refs.md](references/snapshot-refs.md) | Ref lifecycle, `-C` cursor flag, annotated screenshots, troubleshooting |
| [references/session-management.md](references/session-management.md) | Sessions, `--session-name`, state persistence, profiles |
| [references/authentication.md](references/authentication.md) | Login flows, OAuth, 2FA, state reuse, auth vault |
| [references/diffing.md](references/diffing.md) | Snapshot diffs, pixel-level screenshot diffs, URL comparison |
| [references/security.md](references/security.md) | Domain allowlist, action policies, confirmation, content boundaries |
| [references/cloud-providers.md](references/cloud-providers.md) | Cloud browser providers (browserbase, browseruse, kernel) |
| [references/streaming.md](references/streaming.md) | Real-time browser streaming via WebSocket |
| [references/ios-simulator.md](references/ios-simulator.md) | iOS Simulator automation with Safari (macOS only) |
