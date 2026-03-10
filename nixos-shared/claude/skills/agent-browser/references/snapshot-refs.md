# Snapshot and Refs Deep Dive

## Ref Lifecycle

Refs are generated during snapshot and remain valid until:

1. **Page navigation** - New URL invalidates all refs
2. **DOM mutation** - Element removal or significant changes
3. **New snapshot** - Generates fresh refs, old ones may be reused for same elements

## Ref Format

Pattern `@e{number}`: `@e1`, `@e2`, `@e3`, etc. Generated sequentially during snapshot.

## Snapshot Output

**Text:**
```
- heading "Example Domain" [ref=e1] [level=1]
- button "Submit" [ref=e2]
- textbox "Email" [ref=e3] [placeholder="Enter email"]
- link "More information" [ref=e4]
```

**JSON** (`--json`):
```json
{
  "success": true,
  "data": {
    "snapshot": "- heading \"Title\" [ref=e1]\n- button \"Submit\" [ref=e2]",
    "refs": {
      "e1": { "role": "heading", "name": "Title", "level": 1 },
      "e2": { "role": "button", "name": "Submit" }
    }
  }
}
```

## Filtering Options

| Flag | Effect |
|---|---|
| `-i, --interactive` | Only actionable elements (buttons, links, inputs, checkboxes, selects) |
| `-C, --cursor` | Include cursor-interactive elements (cursor:pointer, onclick, tabindex) |
| `-c, --compact` | Remove empty structural elements |
| `-d, --depth <N>` | Limit tree depth |
| `-s, --selector <css>` | Scope to CSS selector |

**Start with `-i`** for most workflows. Add `-C` for SPAs with custom clickable divs. Combine: `-i -c -d 5 -s "#app"`.

## Cursor-Interactive Elements (`-C`)

Many React/Vue apps use non-semantic clickable elements. The `-C` flag detects:
- `cursor: pointer` CSS style
- `onclick` attribute or handler
- `tabindex` attribute

Use `-i -C` together when `-i` alone misses custom interactive components.

## Annotated Screenshots

Overlay numbered labels on interactive elements. Each `[N]` maps to `@eN`:

```bash
agent-browser screenshot --annotate ./page.png
# [1] @e1 button "Submit"
# [2] @e2 link "Home"
agent-browser click @e2  # Use refs immediately
```

Also caches refs - no separate snapshot needed. Useful for icon-only buttons or unlabeled elements.

## Best Practices

1. **Start with `-i`** for most workflows
2. **Add `-C` for SPAs** with non-semantic clickable divs
3. **Re-snapshot after** clicks, form submissions, or navigation
4. **Scope large pages** with `-s` to reduce noise
5. **Use `--annotate`** when text refs are ambiguous (icons, canvas)

## Troubleshooting

### "Ref not found"
Re-snapshot after page changes:
```bash
agent-browser click @e2
agent-browser wait --load networkidle
agent-browser snapshot -i  # Fresh refs
```

### Too many elements
```bash
agent-browser snapshot -i -c -d 5 -s "#content"
```

### Missing expected elements
- Try without `-i` to see all elements
- Scroll down: `agent-browser scroll down 500`
- Check iframes: `agent-browser frame "#my-iframe"` then snapshot

### Progressive exploration (large pages)
```bash
agent-browser snapshot -i -d 2           # Overview
agent-browser snapshot -i -s "#products" # Section
```
