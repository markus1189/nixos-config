# Session Management

## Overview

Sessions provide isolated browser instances with separate:
- Browser contexts
- Cookies and storage
- Navigation history
- Authentication state

## Basic Session Usage

### Named Sessions

```bash
# Create and use different sessions
agent-browser --session project1 open site-a.com
agent-browser --session project2 open site-b.com

# Via environment variable
export AGENT_BROWSER_SESSION=project1
agent-browser snapshot -i
```

### Default Session

Without `--session` flag, uses "default" session:

```bash
# These use the default session
agent-browser open example.com
agent-browser snapshot -i
```

### Session Commands

```bash
# List active sessions
agent-browser session list
# Output:
# Active sessions:
# -> default
#    project1
#    project2

# Show current session
agent-browser session
# Output: default
```

## Session Isolation

Each session has completely isolated:

### 1. Cookies

```bash
# Session 1: Login to account A
agent-browser --session accountA open app.com
agent-browser fill @e1 "userA"
agent-browser fill @e2 "passA"
agent-browser click @e3

# Session 2: Login to account B (different session)
agent-browser --session accountB open app.com
agent-browser fill @e1 "userB"
agent-browser fill @e2 "passB"
agent-browser click @e3

# Sessions maintain separate login states
```

### 2. Local Storage

```bash
agent-browser --session s1 storage local set key "value1"
agent-browser --session s2 storage local set key "value2"

agent-browser --session s1 storage local key  # Returns: value1
agent-browser --session s2 storage local key  # Returns: value2
```

### 3. Navigation History

```bash
agent-browser --session s1 open site-a.com
agent-browser --session s1 open site-b.com
agent-browser --session s1 back  # Returns to site-a.com

agent-browser --session s2 open site-c.com
agent-browser --session s2 back  # Error: no history in s2
```

## Use Cases

### Parallel Web Scraping

```bash
# Scrape multiple sites concurrently
agent-browser --session site1 open https://example1.com &
agent-browser --session site2 open https://example2.com &
agent-browser --session site3 open https://example3.com &

# Process each independently
agent-browser --session site1 snapshot -i --json
agent-browser --session site2 snapshot -i --json
agent-browser --session site3 snapshot -i --json
```

### Multi-Account Testing

```bash
# Test with different user roles
agent-browser --session admin open app.com/login
# Login as admin...
agent-browser --session admin open app.com/admin-panel
agent-browser --session admin snapshot -i

agent-browser --session user open app.com/login
# Login as regular user...
agent-browser --session user open app.com/dashboard
agent-browser --session user snapshot -i
```

### A/B Testing

```bash
# Session A: Control group
agent-browser --session control open app.com
agent-browser --session control screenshot control.png

# Session B: Variant with different settings
agent-browser --session variant set device "iPhone 14"
agent-browser --session variant open app.com
agent-browser --session variant screenshot variant.png
```

### Geographic Testing

```bash
# US session
agent-browser --session us set geo 37.7749 -122.4194
agent-browser --session us open example.com
agent-browser --session us screenshot us-view.png

# UK session
agent-browser --session uk set geo 51.5074 -0.1278
agent-browser --session uk open example.com
agent-browser --session uk screenshot uk-view.png
```

## Session Name Persistence (`--session-name`)

Use `--session-name` to automatically save and restore cookies and localStorage across browser restarts. Unlike `--session` (in-memory only), `--session-name` persists state to disk.

```bash
# Auto-save/load state for "twitter" session
agent-browser --session-name twitter open twitter.com

# Login once - state saves automatically
agent-browser --session-name twitter click "#login"

# Next time (even after browser restart) - still logged in
agent-browser --session-name twitter open twitter.com

# Via environment variable
export AGENT_BROWSER_SESSION_NAME=twitter
agent-browser open twitter.com
```

State files are stored in `~/.agent-browser/sessions/` and automatically loaded on daemon start.

**Session name rules** - must contain only alphanumeric characters, hyphens, and underscores:
```bash
# Valid
agent-browser --session-name my-project open example.com
agent-browser --session-name test_session_v2 open example.com

# Invalid (rejected)
agent-browser --session-name "../bad" open example.com    # path traversal
agent-browser --session-name "my session" open example.com # spaces
```

## State Encryption

Encrypt saved state files (cookies, localStorage) using AES-256-GCM:

```bash
# Generate a 256-bit key (64 hex characters)
openssl rand -hex 32

# Set the encryption key
export AGENT_BROWSER_ENCRYPTION_KEY=<your-64-char-hex-key>

# State files are now encrypted automatically
agent-browser --session-name secure-session open example.com

# List states shows encryption status
agent-browser state list
```

## State Auto-Expiration

Automatically delete old state files to prevent accumulation:

```bash
# Set expiration in days (default: 30 days)
export AGENT_BROWSER_STATE_EXPIRE_DAYS=7

# Manually clean old states
agent-browser state clean --older-than 7
```

## Persistent Profiles

Sessions are ephemeral by default. Use `--profile` for persistence:

```bash
# Create persistent profile
agent-browser --profile ~/.app-profile open app.com
# Login, configure settings...
agent-browser --profile ~/.app-profile state save

# Later: Reuse profile
agent-browser --profile ~/.app-profile open app.com/dashboard
# Still logged in!

# Via environment variable
export AGENT_BROWSER_PROFILE=~/.app-profile
agent-browser open app.com
```

### Profile vs Session

**Session:**
- In-memory only
- Lost when browser closes
- Good for: Temporary isolation, testing, parallel tasks

**Profile:**
- Persisted to disk
- Survives browser restart
- Good for: Reusable auth, long-term state

**Combined:**
```bash
# Multiple profiles with different sessions
agent-browser --profile ~/.profile-a --session task1 open app.com
agent-browser --profile ~/.profile-b --session task2 open app.com
```

## Profile Contents

Profile directory stores:

```
~/.app-profile/
├── Cookies              # HTTP cookies
├── Local Storage/       # localStorage data
├── IndexedDB/           # IndexedDB databases
├── Service Worker/      # Service worker state
├── Cache/               # Browser cache
└── Session Storage/     # sessionStorage (if persisted)
```

### Profile Management

```bash
# Create profile
agent-browser --profile ~/.new-profile open example.com

# Backup profile
cp -r ~/.app-profile ~/.app-profile-backup

# Clean profile (remove cache)
rm -rf ~/.app-profile/Cache/*

# Delete profile
rm -rf ~/.app-profile
```

## Advanced Patterns

### Session Pool for Scraping

```bash
#!/bin/bash
# Create pool of sessions for parallel scraping

urls=(
  "https://example1.com"
  "https://example2.com"
  "https://example3.com"
  "https://example4.com"
)

for i in "${!urls[@]}"; do
  session="scraper-$i"
  url="${urls[$i]}"
  
  (
    agent-browser --session "$session" open "$url"
    agent-browser --session "$session" snapshot -i --json > "output-$i.json"
    agent-browser --session "$session" close
  ) &
done

wait  # Wait for all to complete
```

### Profile per Environment

```bash
# Development profile
agent-browser --profile ~/.dev-profile open https://dev.app.com

# Staging profile
agent-browser --profile ~/.staging-profile open https://staging.app.com

# Production profile (read-only testing)
agent-browser --profile ~/.prod-profile open https://app.com
```

### Rotating Sessions for Rate Limiting

```bash
#!/bin/bash
# Rotate through sessions to avoid rate limits

for i in {1..100}; do
  session="session-$(($i % 5))"  # 5 rotating sessions
  
  agent-browser --session "$session" open "https://api.example.com/data?page=$i"
  agent-browser --session "$session" snapshot -i --json > "page-$i.json"
  
  sleep 2  # Rate limit: 0.5 requests/second per session
done
```

## Environment Variables

| Variable | Description |
|---|---|
| `AGENT_BROWSER_SESSION` | Browser session ID (default: "default") |
| `AGENT_BROWSER_SESSION_NAME` | Auto-save/load state persistence name |
| `AGENT_BROWSER_PROFILE` | Persistent browser profile directory path |
| `AGENT_BROWSER_ENCRYPTION_KEY` | 64-char hex key for AES-256-GCM state encryption |
| `AGENT_BROWSER_STATE_EXPIRE_DAYS` | Auto-delete states older than N days (default: 30) |

## Best Practices

1. **Use descriptive session names** - `admin-session`, `test-user-1`, etc.
2. **Clean up sessions** - Close when done to free resources
3. **Combine with profiles** - For persistent authenticated sessions
4. **Limit concurrent sessions** - Too many can exhaust system resources
5. **Use environment variable** - For consistent session across commands
6. **Session cleanup** - Sessions persist until browser closed or timeout

## Troubleshooting

### "Session not found"

**Cause:** Session was closed or timed out

**Solution:**
```bash
# Check active sessions
agent-browser session list

# Recreate session
agent-browser --session mysession open example.com
```

### Too many sessions

**Cause:** Sessions accumulate without cleanup

**Solution:**
```bash
# Close specific session
agent-browser --session old-session close

# Or restart daemon to clean all sessions
# (Sessions are in-memory, lost on restart)
```

### Profile not persisting

**Cause:** Using session without profile flag

**Solution:**
```bash
# Wrong: Session only (ephemeral)
agent-browser --session temp open app.com

# Right: Profile for persistence
agent-browser --profile ~/.my-profile open app.com
```

### Sessions interfering

**Cause:** Accidentally using same session name

**Solution:**
```bash
# Use unique session names
agent-browser --session "task-$(date +%s)" open example.com

# Or use environment variable scoping
export AGENT_BROWSER_SESSION="unique-task-1"
```
