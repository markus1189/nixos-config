# Authentication Workflows

## Four Approaches

1. **Interactive Login** - Navigate and fill login forms
2. **Header-based Auth** - Set Authorization headers directly
3. **State Reuse** - Save/load authenticated state (recommended for automation)
4. **Auth Vault** - Encrypted credential storage with automated login

## Interactive Login

```bash
agent-browser --headed open https://app.com/login
agent-browser snapshot -i
agent-browser fill @e1 "username"
agent-browser fill @e2 "password"
agent-browser click @e3
agent-browser wait --url "**/dashboard"
agent-browser wait --load networkidle
```

### OAuth Flow

```bash
agent-browser --headed open "https://app.com/login"
agent-browser snapshot -i
agent-browser click @e1  # "Sign in with Google"
agent-browser wait --url "accounts.google.com"
agent-browser snapshot -i
agent-browser fill @e5 "user@gmail.com"
agent-browser click @e6  # Next
agent-browser wait 2000
agent-browser fill @e7 "password"
agent-browser click @e8  # Sign in
agent-browser wait --url "**/dashboard"
```

### Two-Factor Authentication

After login form submission, wait for 2FA prompt:

```bash
agent-browser wait --text "Enter verification code"
agent-browser snapshot -i
agent-browser fill @e4 "123456"  # User provides code
agent-browser click @e5
```

For TOTP/SMS, the user may need to provide the code interactively.

## Header-based Auth

Skip login flows entirely:

```bash
# Bearer token (scoped to origin)
agent-browser open api.example.com --headers '{"Authorization": "Bearer eyJhbG..."}'

# API key
agent-browser open api.service.com --headers '{"X-API-Key": "abc123"}'

# Basic auth
agent-browser set credentials username password
agent-browser open app.com
```

Headers are scoped to origin (protocol + domain + port). Only sent to matching origin.

## State Reuse (Recommended)

Login once, save state, reuse across sessions:

```bash
# Login and save
agent-browser --headed open https://app.com/login
# ... fill and submit login form ...
agent-browser wait --url "**/dashboard"
agent-browser state save ~/.auth/app.json

# Later: load and use
agent-browser state load ~/.auth/app.json
agent-browser open https://app.com/dashboard  # Already authenticated
```

State includes cookies, localStorage, sessionStorage. Protect with `chmod 600`.

## Auth Vault

Encrypted credential storage with automated login:

```bash
# Save (password via stdin to avoid shell history)
echo "mypassword" | agent-browser auth save myapp \
  --url https://app.com --username user --password-stdin

# With custom selectors
agent-browser auth save myapp --url https://app.com \
  --username user --password-stdin \
  --username-selector "#email" --password-selector "#pass" \
  --submit-selector "#login"

# Login
agent-browser auth login myapp

# Manage
agent-browser auth list
agent-browser auth show myapp    # Metadata only, no passwords
agent-browser auth delete myapp
```

Credentials encrypted with AES-256-GCM. Key at `~/.agent-browser/.encryption-key` (chmod 600).

### Auth Vault vs State Files

- **Auth Vault**: Stable login forms, credentials don't expire, supports 2FA between runs
- **State Files**: Long session TTL, OAuth/SAML, form structure changes frequently

## Security

- Protect state files: `chmod 600 ~/.auth/*.json`
- Don't commit state files: `echo "*.auth.json" >> .gitignore`
- Use env vars for tokens: `export TOKEN=$(cat ~/.secrets/token.txt)`
