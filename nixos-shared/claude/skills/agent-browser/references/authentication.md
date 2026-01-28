# Authentication Workflows

## Three Authentication Approaches

1. **Interactive Login** - Navigate and fill login forms
2. **Header-based Auth** - Set Authorization headers
3. **State Reuse** - Save/load authenticated state

## Interactive Login

### Basic Form Authentication

```bash
# Navigate to login page
agent-browser open https://app.example.com/login

# Get form elements
agent-browser snapshot -i --json
# Parse output to find username, password, submit button refs

# Fill credentials
agent-browser fill @e1 "username"
agent-browser fill @e2 "password"

# Submit
agent-browser click @e3

# Wait for navigation to complete
agent-browser wait --url "**/dashboard"
agent-browser wait --load networkidle

# Verify login
agent-browser snapshot -i --json
```

### OAuth Flow

```bash
# Start OAuth flow
agent-browser open "https://app.com/login"
agent-browser snapshot -i --json

# Click OAuth provider button
agent-browser click @e1  # "Sign in with Google"

# Wait for OAuth redirect
agent-browser wait --url "accounts.google.com"

# Fill Google credentials
agent-browser snapshot -i --json
agent-browser fill @e5 "user@gmail.com"
agent-browser click @e6  # Next
agent-browser wait 2000
agent-browser fill @e7 "password"
agent-browser click @e8  # Sign in

# Wait for OAuth callback
agent-browser wait --url "**/callback"
agent-browser wait --url "**/dashboard"

# Verify authenticated
agent-browser snapshot -i --json
```

### Two-Factor Authentication (2FA)

```bash
# Login with username/password
agent-browser open "https://app.com/login"
agent-browser snapshot -i --json
agent-browser fill @e1 "username"
agent-browser fill @e2 "password"
agent-browser click @e3

# Wait for 2FA prompt
agent-browser wait --text "Enter verification code"
agent-browser snapshot -i --json

# User provides 2FA code
# agent-browser fill @e4 "123456"
# agent-browser click @e5

# Or wait for authenticator app / SMS
# Then continue once code is entered externally
```

**Note:** For actual 2FA, you may need:
- User interaction for TOTP codes
- SMS integration
- Authenticator app integration
- Backup codes

## Header-based Authentication

Skip login flows by setting Authorization headers:

### Bearer Token

```bash
# Set auth header scoped to API domain
agent-browser open api.example.com/v1 --headers '{"Authorization": "Bearer eyJhbG..."}'

# All requests to api.example.com include the header
agent-browser snapshot -i --json
agent-browser click @e1

# Navigate to different domain - headers NOT sent
agent-browser open other-site.com
```

### API Key

```bash
agent-browser open api.service.com --headers '{"X-API-Key": "abc123xyz"}'
```

### Basic Auth

```bash
# Option 1: Via headers
agent-browser open app.com --headers '{"Authorization": "Basic dXNlcjpwYXNz"}'

# Option 2: Via set credentials
agent-browser set credentials username password
agent-browser open app.com
```

### Multiple Auth Domains

```bash
# Auth for API domain
agent-browser open api.example.com --headers '{"Authorization": "Bearer token1"}'

# Auth for different API domain
agent-browser open api.acme.com --headers '{"Authorization": "Bearer token2"}'
```

**Important:** Headers are scoped to origin (protocol + domain + port). Only sent to matching origin.

## State Reuse (Recommended)

Most efficient for repeated tasks:

### Save Authenticated State

```bash
# Login once
agent-browser open https://app.example.com/login
agent-browser snapshot -i --json
agent-browser fill @e1 "username"
agent-browser fill @e2 "password"
agent-browser click @e3
agent-browser wait --url "**/dashboard"

# Save state (cookies, storage, etc.)
agent-browser state save ~/.auth/app-example.json

# Close browser
agent-browser close
```

### Load Authenticated State

```bash
# Load saved state
agent-browser state load ~/.auth/app-example.json

# Navigate directly to protected page
agent-browser open https://app.example.com/dashboard

# Already authenticated!
agent-browser snapshot -i --json
```

### State File Contents

State includes:
- Cookies
- localStorage
- sessionStorage
- IndexedDB (if supported)

### State Management

```bash
# Save state
agent-browser state save auth.json

# Load state
agent-browser state load auth.json

# Organize by app/user
mkdir -p ~/.auth
agent-browser state save ~/.auth/app-admin.json
agent-browser state save ~/.auth/app-user.json

# Load specific state
agent-browser state load ~/.auth/app-admin.json
```

## Persistent Profiles

Alternative to state files - entire browser profile:

```bash
# First time: Login with profile
agent-browser --profile ~/.app-profile open app.com/login
agent-browser snapshot -i --json
agent-browser fill @e1 "username"
agent-browser fill @e2 "password"
agent-browser click @e3
agent-browser wait --url "**/dashboard"

# Close browser (profile persisted)
agent-browser close

# Later: Reuse profile
agent-browser --profile ~/.app-profile open app.com/dashboard
# Still logged in!
```

### Profile vs State File

**State File (`state save/load`):**
- Lightweight JSON file
- Easy to share/version control
- Fast to load
- Cross-platform

**Profile (`--profile`):**
- Full browser profile directory
- Includes cache, extensions, etc.
- Better compatibility with complex auth
- Larger disk usage

## Common Patterns

### Multi-User Testing

```bash
# Save states for different users
agent-browser open app.com/login
# Login as admin
agent-browser state save ~/.auth/admin.json
agent-browser close

agent-browser open app.com/login
# Login as user
agent-browser state save ~/.auth/user.json
agent-browser close

# Test as admin
agent-browser state load ~/.auth/admin.json
agent-browser open app.com/admin-panel
agent-browser snapshot -i --json

# Test as user
agent-browser --session user state load ~/.auth/user.json
agent-browser --session user open app.com/dashboard
agent-browser --session user snapshot -i --json
```

### Session + Profile

```bash
# Combine for isolated authenticated sessions
agent-browser --profile ~/.profile-a --session task1 state load auth-a.json
agent-browser --profile ~/.profile-b --session task2 state load auth-b.json
```

### Refresh Expired Sessions

```bash
# Try to load state
agent-browser state load auth.json
agent-browser open app.com/dashboard

# Check if still authenticated
agent-browser get url
# If redirected to /login, re-authenticate

agent-browser snapshot -i --json
agent-browser fill @e1 "username"
agent-browser fill @e2 "password"
agent-browser click @e3
agent-browser wait --url "**/dashboard"

# Save fresh state
agent-browser state save auth.json
```

### API Testing with Auth

```bash
# Test authenticated API
agent-browser open api.example.com/swagger --headers '{"Authorization": "Bearer token"}'

# Navigate API documentation
agent-browser snapshot -i --json

# Test endpoints
agent-browser click @e5  # Try endpoint
agent-browser wait --load networkidle

# Get response
agent-browser get text @e10 --json
```

## Best Practices

1. **Use state files** - For repeated automated tasks
2. **Use profiles** - For complex auth (OAuth, SAML)
3. **Use headers** - For API testing, simple token auth
4. **Secure credentials** - Don't commit state files with tokens
5. **Refresh strategy** - Handle expired sessions gracefully
6. **Session isolation** - Use `--session` for parallel auth states
7. **Test logout** - Verify state cleanup works

## Security Considerations

### State Files

```bash
# Protect state files
chmod 600 ~/.auth/*.json

# Don't commit to version control
echo "*.auth.json" >> .gitignore

# Use environment variables for secrets
export TOKEN=$(cat ~/.secrets/token.txt)
agent-browser open api.com --headers "{\"Authorization\": \"Bearer $TOKEN\"}"
```

### Profile Directories

```bash
# Set restrictive permissions
chmod 700 ~/.app-profile

# Clean sensitive data
rm -rf ~/.app-profile/Cache/*
rm -rf ~/.app-profile/Cookies
```

### Headers

```bash
# Don't log headers
agent-browser open api.com --headers '{"Authorization": "Bearer ***"}' 2>/dev/null

# Use temporary environment variables
TOKEN="secret" agent-browser open api.com --headers "{\"Authorization\": \"Bearer $TOKEN\"}"
# TOKEN not persisted in shell history
```

## Troubleshooting

### Authentication Fails

```bash
# Debug with headed mode
agent-browser --headed open app.com/login

# Check for captcha, rate limiting
agent-browser screenshot debug.png

# Verify form elements
agent-browser snapshot -i

# Check network errors
agent-browser errors
```

### State Doesn't Persist

```bash
# Verify state file exists
ls -lh auth.json

# Check state was saved after login
agent-browser state save auth.json
cat auth.json  # Should contain cookies

# Load and verify
agent-browser state load auth.json
agent-browser cookies  # Should show loaded cookies
```

### Headers Not Working

```bash
# Verify header syntax
agent-browser open api.com --headers '{"Authorization": "Bearer token"}' --debug

# Check origin matching
# Headers only sent to exact origin (protocol + domain + port)

# For all domains, use set headers
agent-browser set headers '{"X-Custom": "value"}'
```

### OAuth Callback Issues

```bash
# OAuth callback may have special handling
# Use wait for URL pattern
agent-browser wait --url "**/callback"
agent-browser wait --url "**/auth/complete"

# Or wait for specific text
agent-browser wait --text "Successfully logged in"

# Check for errors in console
agent-browser console
```
