# Security Features

## Domain Allowlist

Restrict which domains the browser can access. Blocks navigations, sub-resources, WebSocket, EventSource, and sendBeacon to unlisted domains.

```bash
agent-browser --allowed-domains "example.com,*.api.com" open example.com
# Or via env var
AGENT_BROWSER_ALLOWED_DOMAINS="example.com,*.api.com" agent-browser open example.com
```

Supports wildcards: `*.example.com` matches all subdomains.

## Action Policy

JSON file defining allow/deny lists across 13 action categories:

```bash
agent-browser --action-policy policy.json open example.com
# Or via env var
AGENT_BROWSER_ACTION_POLICY=policy.json agent-browser open example.com
```

Action categories: `navigate`, `click`, `fill`, `eval`, `download`, `upload`, `snapshot`, `scroll`, `wait`, `get`, `interact`, `network`, `state`.

## Action Confirmation

Require explicit approval for sensitive actions:

```bash
# Specify which action categories need confirmation
agent-browser --confirm-actions eval,download open example.com

# When a gated action is triggered, approve or deny it:
agent-browser confirm <confirmation-id>
agent-browser deny <confirmation-id>
```

60-second auto-deny timeout if no response.

## Content Boundary Markers

Wraps page-sourced output with boundary markers to distinguish trusted tool output from untrusted page content. Helps prevent prompt injection from web pages:

```bash
agent-browser --content-boundaries snapshot -i
# Or via env var
AGENT_BROWSER_CONTENT_BOUNDARIES=1 agent-browser snapshot -i
```

Output format:
```
--- AGENT_BROWSER_PAGE_CONTENT nonce=<random> origin=<url> ---
[page content]
--- END_AGENT_BROWSER_PAGE_CONTENT nonce=<random> ---
```

## Auth Vault Encryption

Credentials stored with AES-256-GCM encryption:

```bash
# Save credentials (password via stdin to avoid shell history)
echo "mypassword" | agent-browser auth save myapp \
  --url https://app.com --username user --password-stdin

# Encryption key location
~/.agent-browser/.encryption-key  # chmod 600
```

## Output Truncation

Limit output size to prevent context window overflow:

```bash
agent-browser --max-output 10000 snapshot
# Or via env var
AGENT_BROWSER_MAX_OUTPUT=10000 agent-browser snapshot
```
