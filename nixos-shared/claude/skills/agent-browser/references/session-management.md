# Session Management

## Overview

Sessions provide isolated browser instances with separate cookies, storage, navigation history, and authentication state.

## Basic Usage

```bash
# Named sessions (in-memory, lost on browser close)
agent-browser --session project1 open site-a.com
agent-browser --session project2 open site-b.com

# List active sessions
agent-browser session list

# Default session (without --session flag)
agent-browser open example.com

# Via environment variable
export AGENT_BROWSER_SESSION=project1
```

## Session Name Persistence (`--session-name`)

Auto-save and restore cookies/localStorage across browser restarts:

```bash
# Auto-save/load state for "twitter" session
agent-browser --session-name twitter open twitter.com

# Login once - state saves automatically
# Next time (even after restart) - still logged in
agent-browser --session-name twitter open twitter.com

# Via env var
export AGENT_BROWSER_SESSION_NAME=twitter
```

State files stored in `~/.agent-browser/sessions/`. Session names must be alphanumeric, hyphens, or underscores only.

## State Encryption

```bash
# Generate 256-bit key
openssl rand -hex 32

# Set encryption key - state files encrypted with AES-256-GCM
export AGENT_BROWSER_ENCRYPTION_KEY=<64-char-hex-key>
agent-browser --session-name secure open example.com
```

## State Auto-Expiration

```bash
export AGENT_BROWSER_STATE_EXPIRE_DAYS=7  # Default: 30
agent-browser state clean --older-than 7  # Manual cleanup
```

## Persistent Profiles

Full browser profile directory (cache, extensions, etc.) - unlike sessions which are in-memory:

```bash
agent-browser --profile ~/.app-profile open app.com
# Login, configure... profile persists to disk
# Later:
agent-browser --profile ~/.app-profile open app.com  # Still logged in

export AGENT_BROWSER_PROFILE=~/.app-profile  # Via env var
```

## Session vs Profile vs Session-Name

| | `--session` | `--session-name` | `--profile` |
|---|---|---|---|
| Storage | In-memory | JSON state file | Full browser dir |
| Survives restart | No | Yes (cookies/storage) | Yes (everything) |
| Use case | Parallel isolation | Repeated logins | Complex auth, extensions |

Combine them: `agent-browser --profile ~/.profile-a --session task1 open app.com`

## State Commands

```bash
agent-browser state save auth.json   # Save browser state
agent-browser state load auth.json   # Load saved state
agent-browser state list             # List saved states
agent-browser state show auth.json   # Show state summary
agent-browser state rename old new   # Rename state file
agent-browser state clear --all      # Clear all states
```

## Environment Variables

| Variable | Description |
|---|---|
| `AGENT_BROWSER_SESSION` | In-memory session name (default: "default") |
| `AGENT_BROWSER_SESSION_NAME` | Auto-persist state name |
| `AGENT_BROWSER_PROFILE` | Persistent browser profile directory |
| `AGENT_BROWSER_ENCRYPTION_KEY` | 64-char hex key for AES-256-GCM state encryption |
| `AGENT_BROWSER_STATE_EXPIRE_DAYS` | Auto-delete states older than N days (default: 30) |
