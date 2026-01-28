# Cloud Browser Providers

Connect to remote browser infrastructure when local browsers aren't available.

## Supported Providers

- **Browserbase** - Scalable browser automation platform
- **Browser Use** - Managed browser service
- **Kernel** - Stealth mode browsers with persistent profiles

## Configuration

### Via Flag (Recommended)

```bash
agent-browser -p browserbase open https://example.com
agent-browser -p browseruse open https://example.com
agent-browser -p kernel open https://example.com
```

### Via Environment Variable

```bash
export AGENT_BROWSER_PROVIDER=browserbase
agent-browser open https://example.com
```

**Note:** The `-p` flag takes precedence over `AGENT_BROWSER_PROVIDER`.

## Browserbase

Scalable browser automation platform.

### Setup

```bash
export BROWSERBASE_API_KEY="your-api-key"
export BROWSERBASE_PROJECT_ID="your-project-id"
```

### Usage

```bash
# Via flag
agent-browser -p browserbase open https://example.com
agent-browser -p browserbase snapshot -i --json

# Via environment variable
export AGENT_BROWSER_PROVIDER=browserbase
agent-browser open https://example.com
```

### Features

- Scalable infrastructure
- Geographic distribution
- Proxy support
- Session recording
- Screenshot/video capture

### When to Use

- High-volume scraping
- Geo-distributed testing
- Scalability requirements
- Serverless environments

## Browser Use

Managed browser service.

### Setup

```bash
export BROWSER_USE_API_KEY="your-api-key"
```

### Usage

```bash
# Via flag
agent-browser -p browseruse open https://example.com

# Via environment variable
export AGENT_BROWSER_PROVIDER=browseruse
agent-browser open https://example.com
```

### Features

- Simple API
- Pay-per-use pricing
- No infrastructure management
- Quick setup

### When to Use

- Quick prototyping
- Low-volume automation
- No local browser available
- Cloud-first workflows

## Kernel

Stealth mode browsers with persistent profiles.

### Setup

```bash
export KERNEL_API_KEY="your-api-key"

# Optional: Persistent profile
export KERNEL_PROFILE_NAME="my-profile"
```

### Usage

```bash
# Via flag
agent-browser -p kernel open https://example.com

# With persistent profile
export KERNEL_PROFILE_NAME="authenticated-profile"
agent-browser -p kernel open https://app.example.com/dashboard

# Via environment variable
export AGENT_BROWSER_PROVIDER=kernel
agent-browser open https://example.com
```

### Features

- Stealth mode (anti-detection)
- Persistent profiles across sessions
- Advanced fingerprinting resistance
- Bot detection bypass

### When to Use

- Sites with bot detection
- Long-term authenticated sessions
- Anti-scraping countermeasures
- Persistent browser state needed

## Comparison

| Feature | Browserbase | Browser Use | Kernel |
|---------|-------------|-------------|---------|
| Stealth Mode | ❌ | ❌ | ✅ |
| Persistent Profiles | ❌ | ❌ | ✅ |
| Geographic Distribution | ✅ | ❌ | ❌ |
| Scalability | High | Medium | Medium |
| Bot Detection Bypass | Basic | Basic | Advanced |
| Pricing | Tiered | Pay-per-use | Tiered |

## Best Practices

### Choose the Right Provider

**Use Browserbase when:**
- High volume scraping (1000+ requests/hour)
- Geographic testing needed
- Scalability is priority

**Use Browser Use when:**
- Prototyping or testing
- Low volume automation
- Simple use case

**Use Kernel when:**
- Bot detection is concern
- Persistent auth needed
- Stealth is priority

### Cost Optimization

```bash
# Use local browser for development
agent-browser open example.com

# Switch to cloud for production
agent-browser -p browserbase open example.com
```

### Fallback Strategy

```bash
#!/bin/bash
# Try local browser first, fallback to cloud

if agent-browser open example.com 2>/dev/null; then
  echo "Using local browser"
  agent-browser snapshot -i
else
  echo "Fallback to cloud"
  agent-browser -p browserbase open example.com
  agent-browser -p browserbase snapshot -i
fi
```

### Session Management

Cloud providers may have different session limits:

```bash
# Create multiple cloud sessions
agent-browser -p browserbase --session task1 open site-a.com
agent-browser -p browserbase --session task2 open site-b.com
```

### Profile Persistence (Kernel)

```bash
# First run: Login with persistent profile
export KERNEL_PROFILE_NAME="app-profile"
agent-browser -p kernel open app.com/login
agent-browser -p kernel fill @e1 "username"
agent-browser -p kernel fill @e2 "password"
agent-browser -p kernel click @e3

# Later runs: Reuse profile
export KERNEL_PROFILE_NAME="app-profile"
agent-browser -p kernel open app.com/dashboard
# Already authenticated!
```

## Troubleshooting

### "Provider not configured"

**Cause:** Missing API credentials

**Solution:**
```bash
# Set required environment variables
export BROWSERBASE_API_KEY="..."
export BROWSERBASE_PROJECT_ID="..."

# Or for Browser Use
export BROWSER_USE_API_KEY="..."

# Or for Kernel
export KERNEL_API_KEY="..."
```

### Slow Performance

**Cause:** Network latency to cloud provider

**Solution:**
- Choose provider with nearby infrastructure
- Reduce snapshot frequency
- Use `-i` flag to reduce snapshot size
- Consider local browser for development

### Connection Timeout

**Cause:** Provider API unreachable

**Solution:**
```bash
# Check provider status
curl https://api.browserbase.com/health

# Retry with timeout
timeout 30s agent-browser -p browserbase open example.com

# Fallback to different provider
agent-browser -p browseruse open example.com
```

### Authentication Issues

**Cause:** Expired or invalid API key

**Solution:**
```bash
# Verify credentials
echo $BROWSERBASE_API_KEY
echo $BROWSERBASE_PROJECT_ID

# Generate new API key from provider dashboard

# Update environment
export BROWSERBASE_API_KEY="new-key"
```

### Profile Not Persisting (Kernel)

**Cause:** Profile name not set

**Solution:**
```bash
# Set profile name
export KERNEL_PROFILE_NAME="my-persistent-profile"

# Verify profile is used
agent-browser -p kernel open example.com
# Profile name should appear in logs
```

## Advanced Patterns

### Multi-Provider Strategy

Use different providers for different tasks:

```bash
#!/bin/bash

# Fast scraping: Browserbase
scrape_task() {
  agent-browser -p browserbase --session scraper open "$1"
  agent-browser -p browserbase --session scraper snapshot -i --json
}

# Stealth task: Kernel
stealth_task() {
  agent-browser -p kernel --session stealth open "$1"
  agent-browser -p kernel --session stealth snapshot -i --json
}

# Development: Local
dev_task() {
  agent-browser open "$1"
  agent-browser snapshot -i --json
}
```

### Cost Tracking

```bash
# Log provider usage
log_usage() {
  echo "$(date),$(printenv AGENT_BROWSER_PROVIDER),$1" >> usage.csv
}

agent-browser -p browserbase open example.com
log_usage "page-load"

agent-browser -p browserbase snapshot -i
log_usage "snapshot"
```

### Geographic Rotation

Some providers support geo-selection:

```bash
# Browserbase with geo hint (check provider docs)
export BROWSERBASE_REGION="us-east"
agent-browser -p browserbase open example.com

export BROWSERBASE_REGION="eu-west"
agent-browser -p browserbase open example.com
```

## Provider-Specific Tips

### Browserbase

- Supports session recording - save entire session for replay
- Provides screenshot/video APIs
- Good for CI/CD integration

### Browser Use

- Simple pay-per-use model
- No session management needed
- Good for occasional use

### Kernel

- Best stealth capabilities
- Persistent profiles critical for long-term auth
- Use for sites with aggressive bot detection

## Migration Guide

### Local to Cloud

```bash
# Before: Local browser
agent-browser open example.com
agent-browser snapshot -i

# After: Cloud provider
agent-browser -p browserbase open example.com
agent-browser -p browserbase snapshot -i
```

**Changes needed:**
- Add provider credentials to environment
- Add `-p` flag to commands
- Consider session/profile implications

### Cloud to Cloud

```bash
# From Browserbase
export BROWSERBASE_API_KEY="..."
agent-browser -p browserbase open example.com

# To Kernel (for stealth)
export KERNEL_API_KEY="..."
agent-browser -p kernel open example.com
```

**Considerations:**
- Different credential format
- Profile compatibility
- Feature parity
- Pricing differences
