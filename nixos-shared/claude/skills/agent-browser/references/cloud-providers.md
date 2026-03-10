# Cloud Browser Providers

Connect to remote browser infrastructure when local browsers aren't available.

## Setup

### Browserbase

```bash
export BROWSERBASE_API_KEY="your-api-key"
export BROWSERBASE_PROJECT_ID="your-project-id"
agent-browser -p browserbase open https://example.com
```

### Browser Use

```bash
export BROWSER_USE_API_KEY="your-api-key"
agent-browser -p browseruse open https://example.com
```

### Kernel

```bash
export KERNEL_API_KEY="your-api-key"
agent-browser -p kernel open https://example.com

# With persistent profile
export KERNEL_PROFILE_NAME="my-profile"
agent-browser -p kernel open https://app.com/dashboard
```

Set provider via env var instead of flag: `export AGENT_BROWSER_PROVIDER=browserbase`

## Comparison

| Feature | Browserbase | Browser Use | Kernel |
|---------|-------------|-------------|---------|
| Stealth Mode | No | No | Yes |
| Persistent Profiles | No | No | Yes |
| Geographic Distribution | Yes | No | No |
| Scalability | High | Medium | Medium |
| Bot Detection Bypass | Basic | Basic | Advanced |
| Pricing | Tiered | Pay-per-use | Tiered |

## When to Use

- **Browserbase**: High-volume scraping, geo-distributed testing, CI/CD
- **Browser Use**: Prototyping, low-volume, simple use cases
- **Kernel**: Bot detection avoidance, persistent auth, stealth required

## Sessions with Cloud Providers

```bash
agent-browser -p browserbase --session task1 open site-a.com
agent-browser -p browserbase --session task2 open site-b.com
```

Cloud providers may have different session limits than local browsers.
