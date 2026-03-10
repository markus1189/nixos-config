# Diffing

Compare page snapshots, screenshots, or URLs to detect changes.

## Snapshot Diff

Line-level comparison of accessibility trees using `+`/`-` notation:

```bash
# Compare current snapshot to last snapshot taken in this session
agent-browser diff snapshot

# Compare against a saved baseline file
agent-browser diff snapshot --baseline saved-snapshot.txt

# Scoped comparison
agent-browser diff snapshot -s "#main-content" -c
```

## Screenshot Diff

Pixel-level visual comparison. Changed pixels highlighted in red:

```bash
# Compare current screenshot against baseline image
agent-browser diff screenshot --baseline before.png

# With sensitivity threshold (0=exact, 1=ignore all differences)
agent-browser diff screenshot --baseline before.png -t 0.1

# Full page comparison
agent-browser diff screenshot --baseline before.png --full

# Save diff output image
agent-browser diff screenshot --baseline before.png -o diff-result.png
```

## URL Diff

Navigate to two URLs and compare both snapshot and screenshot:

```bash
# Snapshot diff between two URLs
agent-browser diff url https://staging.app.com https://prod.app.com

# Include visual diff
agent-browser diff url https://staging.app.com https://prod.app.com --screenshot

# Scoped to specific section
agent-browser diff url https://a.com https://b.com -s "#header"
```

## Options

| Option | Description |
|---|---|
| `-s, --selector` | Scope comparison to CSS selector |
| `-c, --compact` | Compact diff output |
| `-d, --depth` | Limit tree depth |
| `-t, --threshold <0-1>` | Pixel diff sensitivity (screenshot mode) |
| `-o, --output <path>` | Save diff output to file |
| `--full` | Full page screenshot for visual diff |
| `--wait-until` | Wait condition before comparing |
| `--baseline <path>` | Baseline file for comparison |

## Use Cases

### Regression Testing

```bash
# Save baseline
agent-browser open https://app.com
agent-browser snapshot -i > baseline.txt
agent-browser screenshot baseline.png

# After changes, compare
agent-browser open https://app.com
agent-browser diff snapshot --baseline baseline.txt
agent-browser diff screenshot --baseline baseline.png -t 0.05
```

### A/B Page Comparison

```bash
agent-browser diff url https://app.com/old https://app.com/new --screenshot
```

### Monitoring Changes

```bash
# Check if page changed since last visit
agent-browser open https://example.com
agent-browser diff snapshot  # Compares to previous snapshot in session
```
