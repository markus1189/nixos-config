# Snapshot and Refs Deep Dive

## Ref Lifecycle

Refs are generated during snapshot and remain valid until:

1. **Page navigation** - New URL invalidates all refs
2. **DOM mutation** - Element removal or significant changes
3. **New snapshot** - Generates fresh refs, old ones may be reused for same elements

## Ref Format

Refs follow the pattern `@e{number}`:
- `@e1`, `@e2`, `@e3`, etc.
- Generated sequentially during snapshot
- Correspond to elements in accessibility tree

## Snapshot Output Format

**Text output:**
```
- heading "Example Domain" [ref=e1] [level=1]
- button "Submit" [ref=e2]
- textbox "Email" [ref=e3] [placeholder="Enter email"]
- link "More information" [ref=e4]
```

**JSON output:**
```json
{
  "success": true,
  "data": {
    "snapshot": "- heading \"Title\" [ref=e1]\n- button \"Submit\" [ref=e2]",
    "refs": {
      "e1": {
        "role": "heading",
        "name": "Title",
        "level": 1
      },
      "e2": {
        "role": "button",
        "name": "Submit"
      }
    }
  }
}
```

## Filtering Strategies

### Interactive Only (`-i`)

Most useful for AI agents - returns only actionable elements:

```bash
agent-browser snapshot -i
```

Returns:
- Buttons
- Links
- Text inputs
- Checkboxes/radios
- Select dropdowns
- Clickable elements

Excludes:
- Headings (unless clickable)
- Paragraphs
- Divs
- Structural elements

### Cursor-Interactive Elements (`-C`)

Many modern web apps use custom clickable elements (divs, spans) instead of standard buttons or links. The `-C` flag detects these by looking for:

- `cursor: pointer` CSS style
- `onclick` attribute or handler
- `tabindex` attribute (keyboard focusable)

```bash
agent-browser snapshot -i -C
# Output includes:
# @e1 [button] "Submit"
# @e2 [link] "Learn more"
# Cursor-interactive elements:
# @e3 [clickable] "Menu Item" [cursor:pointer, onclick]
# @e4 [clickable] "Card" [cursor:pointer]
```

Use `-i -C` together when `-i` alone misses custom interactive components.

### Compact Mode (`-c`)

Removes empty structural elements:

```bash
agent-browser snapshot -c
```

Good for: Reducing noise from layout containers

### Depth Limiting (`-d`)

Limit tree depth to prevent overwhelming context:

```bash
agent-browser snapshot -d 3  # Only 3 levels deep
```

Good for: Very deep DOMs, initial exploration

### Scoped Snapshots (`-s`)

Focus on specific page sections:

```bash
agent-browser snapshot -s "#main-content"
agent-browser snapshot -s ".product-list"
```

Good for: Large pages, specific workflows

### Combined Filtering

```bash
agent-browser snapshot -i -c -d 5 -s "#app"
```

Returns: Interactive elements within #app, compact output, max 5 levels deep

## Annotated Screenshots

For visual context alongside snapshots, use `screenshot --annotate` to overlay numbered labels on interactive elements. Each label `[N]` maps to ref `@eN`:

```bash
agent-browser screenshot --annotate ./page.png
# -> Screenshot saved to ./page.png
#    [1] @e1 button "Submit"
#    [2] @e2 link "Home"
#    [3] @e3 textbox "Email"
agent-browser click @e2  # Use refs immediately
```

Annotated screenshots **also cache refs**, so you can interact with elements right after without a separate snapshot call. Useful when:
- Text snapshot is insufficient (unlabeled icons, canvas content)
- Visual layout verification is needed
- Elements lack accessible names

## Best Practices

1. **Start with `-i`** - Most AI workflows only need interactive elements
2. **Add `-C` for SPAs** - Many React/Vue apps use non-semantic clickable divs
3. **Re-snapshot liberally** - After clicks, form submissions, or navigation
4. **Use JSON for parsing** - `--json` flag for structured data
5. **Scope large pages** - Use `-s` to focus on relevant sections
6. **Combine filters** - `-i -c` works well for most scenarios
7. **Use `--annotate`** - When visual context or icon-only buttons make text refs ambiguous

## Troubleshooting

### "Ref not found" errors

**Cause:** Ref invalidated by page change

**Solution:**
```bash
# Always re-snapshot after page changes
agent-browser click @e2
agent-browser wait --load networkidle
agent-browser snapshot -i --json  # Fresh refs
```

### Too many elements in snapshot

**Solutions:**
```bash
# Use interactive-only filter
agent-browser snapshot -i

# Limit depth
agent-browser snapshot -i -d 4

# Scope to section
agent-browser snapshot -i -s "#main"

# Combine all filters
agent-browser snapshot -i -c -d 5 -s "#content"
```

### Missing expected elements

**Check if element is interactive:**
```bash
# Without -i to see all elements
agent-browser snapshot

# Element might not be in viewport
agent-browser scroll down 500
agent-browser snapshot -i
```

**Check if element is in iframe:**
```bash
# Switch to iframe first
agent-browser frame "#my-iframe"
agent-browser snapshot -i
```

### Ref points to wrong element

**Cause:** DOM changed between snapshot and action

**Solution:**
```bash
# Take fresh snapshot immediately before action
agent-browser snapshot -i --json
# Parse to find target ref
agent-browser click @e5  # Use immediately
```

## Advanced Patterns

### Progressive Exploration

For very large pages, explore incrementally:

```bash
# 1. Get overview (shallow depth)
agent-browser snapshot -i -d 2

# 2. Identify section of interest
agent-browser snapshot -i -s "#products"

# 3. Deep dive into section
agent-browser snapshot -i -s "#products .item:first-child"
```

### Pagination Workflows

```bash
# Get current page items
agent-browser snapshot -i -s ".items"

# Process items...

# Click next, re-snapshot
agent-browser click @e99  # Next button
agent-browser wait --load networkidle
agent-browser snapshot -i -s ".items"  # Fresh refs for new page
```

### Dynamic Content

For dynamically loaded content:

```bash
# Initial snapshot
agent-browser snapshot -i

# Trigger dynamic load (e.g., scroll)
agent-browser scroll down 1000

# Wait for content to load
agent-browser wait 2000

# Re-snapshot to get new refs
agent-browser snapshot -i
```

## Performance Optimization

### Snapshot Size Estimation

- Full snapshot: 10-100KB for typical pages
- `-i` flag: Reduces by 50-80%
- `-c` flag: Reduces by 20-30%
- `-d 3`: Reduces by 30-60% depending on DOM depth

### When to Snapshot

**Do snapshot:**
- After navigation (`open`, `back`, `forward`)
- After clicking navigation elements
- After form submission
- Before interacting with page

**Don't snapshot:**
- After every single action
- If page hasn't changed
- Multiple times in rapid succession

**Optimal pattern:**
```bash
# Navigate once, snapshot once
agent-browser open https://example.com
agent-browser snapshot -i --json

# Multiple actions without re-snapshot (if DOM stable)
agent-browser fill @e1 "text1"
agent-browser fill @e2 "text2"
agent-browser check @e3

# Re-snapshot only when needed
agent-browser click @e4  # Submit
agent-browser wait --load networkidle
agent-browser snapshot -i --json  # Now re-snapshot
```
