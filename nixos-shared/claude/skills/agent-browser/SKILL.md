---
name: agent-browser
description: Automates browser interactions for web testing, form filling, screenshots, and data extraction. Use when the user needs to navigate websites, interact with web pages, fill forms, take screenshots, test web applications, or extract information from web pages.
allowed-tools: Bash(agent-browser:*)
---

# Browser Automation with agent-browser

> **NixOS Note**: This environment uses Nix to run agent-browser. Use the following command pattern:
> ```bash
> nix run github:numtide/llm-agents.nix#agent-browser -- <command> [args...]
> ```
> For example: `nix run github:numtide/llm-agents.nix#agent-browser -- screenshot`

## Quick start

```bash
agent-browser open <url>  # Navigate to page
agent-browser snapshot -i  # Get interactive elements with refs
agent-browser click @e1  # Click element by ref
agent-browser fill @e2 "text"  # Fill input by ref
agent-browser close  # Close browser
```

## Core workflow

Default to a headed browser mode, s.t. I can "pair browse" with you.

1. Navigate: `agent-browser open <url>`
2. Snapshot: `agent-browser snapshot -i` (returns elements with refs like `@e1`, `@e2`)
3. Interact using refs from the snapshot
4. Re-snapshot after navigation or significant DOM changes

## Common Commands

### Navigation & Snapshots
```bash
agent-browser open <url>  # Navigate (supports https://, http://, file://)
agent-browser back/forward/reload  # Navigation controls
agent-browser snapshot -i  # Interactive elements with refs (recommended)
agent-browser snapshot -s "#main"  # Scope to CSS selector
agent-browser close  # Close browser
```

### Interactions (use @refs from snapshot)
```bash
agent-browser click @e1  # Click element
agent-browser dblclick @e1  # Double-click
agent-browser fill @e2 "text"  # Clear and type
agent-browser type @e2 "text"  # Type without clearing
agent-browser press Enter  # Press key
agent-browser hover @e1  # Hover element
agent-browser check @e1  # Check checkbox
agent-browser select @e1 "value"  # Select dropdown option
agent-browser upload @e1 file.pdf  # Upload file
```

### Get Information
```bash
agent-browser get text @e1  # Get element text
agent-browser get url  # Get current URL
agent-browser get title  # Get page title
agent-browser get value @e1  # Get input value
agent-browser get attr @e1 href  # Get attribute
```

### Wait & Screenshots
```bash
agent-browser wait @e1  # Wait for element
agent-browser wait 2000  # Wait milliseconds
agent-browser wait --text "Success"  # Wait for text
agent-browser wait --load networkidle  # Wait for network idle
agent-browser screenshot  # Take screenshot
agent-browser screenshot --full  # Full page screenshot
```

### Debugging
```bash
agent-browser --headed open example.com  # Show browser window
agent-browser console  # View console messages
agent-browser errors  # View page errors
agent-browser record start ./debug.webm  # Record video
agent-browser record stop  # Save recording
```

### Sessions & State
```bash
agent-browser --session test1 open site.com  # Isolated session
agent-browser state save auth.json  # Save browser state
agent-browser state load auth.json  # Load saved state
```

For complete command reference including mouse control, semantic locators, network interception, tabs, frames, and all options, see [references/command-reference.md](references/command-reference.md).

## Example: Form submission

```bash
agent-browser open https://example.com/form
agent-browser snapshot -i
# Output shows: textbox "Email" [ref=e1], textbox "Password" [ref=e2], button "Submit" [ref=e3]
agent-browser fill @e1 "user@example.com"
agent-browser fill @e2 "password123"
agent-browser click @e3
agent-browser wait --load networkidle
agent-browser snapshot -i  # Check result
```

## Example: Authentication with saved state

```bash
# Login once
agent-browser open https://app.example.com/login
agent-browser snapshot -i
agent-browser fill @e1 "username"
agent-browser fill @e2 "password"
agent-browser click @e3
agent-browser wait --url "**/dashboard"
agent-browser state save auth.json

# Later sessions: load saved state
agent-browser state load auth.json
agent-browser open https://app.example.com/dashboard
```

## Deep-dive documentation

For detailed patterns and best practices, see:

| Reference | Description |
|-----------|-------------|
| [references/command-reference.md](references/command-reference.md) | Complete command reference with all options |
| [references/snapshot-refs.md](references/snapshot-refs.md) | Ref lifecycle, invalidation rules, troubleshooting |
| [references/session-management.md](references/session-management.md) | Parallel sessions, state persistence, concurrent scraping |
| [references/authentication.md](references/authentication.md) | Login flows, OAuth, 2FA handling, state reuse |
| [references/cloud-providers.md](references/cloud-providers.md) | Using cloud browser providers (browserbase, browseruse) |
| [references/streaming.md](references/streaming.md) | Real-time browser streaming and remote viewing |

## Ready-to-use templates

Executable workflow scripts for common patterns:

| Template | Use Case |
|----------|----------|
| [templates/form-automation.sh](templates/form-automation.sh) | Automated form filling with validation and error handling |
| [templates/authenticated-session.sh](templates/authenticated-session.sh) | Login once, save state, reuse across sessions |
| [templates/capture-workflow.sh](templates/capture-workflow.sh) | Content extraction with screenshots and structured data output |

Usage:

```bash
./templates/form-automation.sh https://example.com/form
./templates/authenticated-session.sh https://app.example.com/login
./templates/capture-workflow.sh https://example.com ./output
```

**Script Execution:** Scripts should be executed from the skill directory.
All scripts use Nix shebangs so no manual dependency installation is required.
