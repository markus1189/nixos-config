#!/usr/bin/env nix
#! nix shell nixpkgs#bash nixpkgs#jq --command bash
set -euo pipefail

# Content Capture Workflow Template
# Usage: ./capture-workflow.sh <url> <output-dir>

URL="${1:-}"
OUTPUT_DIR="${2:-./output}"

if [ -z "$URL" ]; then
  echo "Usage: $0 <url> <output-dir>"
  echo "Example: $0 https://example.com/article ./captures"
  exit 1
fi

echo "=== Content Capture Workflow ==="
echo "Target URL: $URL"
echo "Output directory: $OUTPUT_DIR"
echo

# Create output directory
mkdir -p "$OUTPUT_DIR"

# Step 1: Navigate to page
echo "[1/6] Navigating to page..."
agent-browser open "$URL"
agent-browser wait --load networkidle

# Get page metadata
PAGE_TITLE=$(agent-browser get title)
PAGE_URL=$(agent-browser get url)

echo "  - Title: $PAGE_TITLE"
echo "  - URL: $PAGE_URL"

# Step 2: Capture initial snapshot
echo
echo "[2/6] Capturing accessibility tree..."
agent-browser snapshot --json > "$OUTPUT_DIR/snapshot-full.json"
agent-browser snapshot -i --json > "$OUTPUT_DIR/snapshot-interactive.json"
echo "  - Full snapshot: $OUTPUT_DIR/snapshot-full.json"
echo "  - Interactive snapshot: $OUTPUT_DIR/snapshot-interactive.json"

# Step 3: Extract structured content
echo
echo "[3/6] Extracting content..."

# Try to find main content area
MAIN_CONTENT=$(agent-browser get text "main" --json 2>/dev/null | jq -r '.data // ""')
if [ -n "$MAIN_CONTENT" ]; then
  echo "$MAIN_CONTENT" > "$OUTPUT_DIR/main-content.txt"
  echo "  - Main content: $OUTPUT_DIR/main-content.txt"
fi

# Extract article body (common selectors)
for selector in "article" ".article-body" ".post-content" ".content"; do
  CONTENT=$(agent-browser get text "$selector" --json 2>/dev/null | jq -r '.data // ""')
  if [ -n "$CONTENT" ] && [ ${#CONTENT} -gt 100 ]; then
    echo "$CONTENT" > "$OUTPUT_DIR/article-body.txt"
    echo "  - Article body: $OUTPUT_DIR/article-body.txt"
    break
  fi
done

# Extract HTML for structure
agent-browser get html "body" > "$OUTPUT_DIR/page-html.html" 2>/dev/null || true

# Step 4: Take screenshots
echo
echo "[4/6] Capturing screenshots..."

# Viewport screenshot
agent-browser screenshot "$OUTPUT_DIR/screenshot-viewport.png"
echo "  - Viewport: $OUTPUT_DIR/screenshot-viewport.png"

# Full page screenshot
agent-browser screenshot --full "$OUTPUT_DIR/screenshot-full.png"
echo "  - Full page: $OUTPUT_DIR/screenshot-full.png"

# Step 5: Capture metadata
echo
echo "[5/6] Extracting metadata..."

cat > "$OUTPUT_DIR/metadata.json" <<EOF
{
  "url": "$PAGE_URL",
  "title": "$PAGE_TITLE",
  "captured_at": "$(date -Iseconds)",
  "viewport": "$(agent-browser get url --json | jq -r '.data // ""')"
}
EOF

# Try to extract Open Graph metadata
OG_TITLE=$(agent-browser eval "document.querySelector('meta[property=\"og:title\"]')?.content || ''" 2>/dev/null || echo "")
OG_DESC=$(agent-browser eval "document.querySelector('meta[property=\"og:description\"]')?.content || ''" 2>/dev/null || echo "")
OG_IMAGE=$(agent-browser eval "document.querySelector('meta[property=\"og:image\"]')?.content || ''" 2>/dev/null || echo "")

if [ -n "$OG_TITLE" ] || [ -n "$OG_DESC" ]; then
  cat > "$OUTPUT_DIR/og-metadata.json" <<EOF
{
  "og_title": "$OG_TITLE",
  "og_description": "$OG_DESC",
  "og_image": "$OG_IMAGE"
}
EOF
  echo "  - Open Graph metadata: $OUTPUT_DIR/og-metadata.json"
fi

# Step 6: Extract links
echo
echo "[6/6] Extracting links..."

# Get all links
agent-browser eval "Array.from(document.querySelectorAll('a')).map(a => ({text: a.textContent.trim(), href: a.href})).filter(l => l.href).slice(0, 100)" > "$OUTPUT_DIR/links.json" 2>/dev/null || true

if [ -f "$OUTPUT_DIR/links.json" ]; then
  LINK_COUNT=$(jq length "$OUTPUT_DIR/links.json" 2>/dev/null || echo "0")
  echo "  - Extracted $LINK_COUNT links: $OUTPUT_DIR/links.json"
fi

# Optional: Save as PDF
echo
echo "Saving as PDF..."
agent-browser pdf "$OUTPUT_DIR/page.pdf"
echo "  - PDF: $OUTPUT_DIR/page.pdf"

# Summary
echo
echo "=== Capture Complete ==="
echo "Output directory: $OUTPUT_DIR"
echo
echo "Files created:"
ls -lh "$OUTPUT_DIR" | tail -n +2 | awk '{printf "  %s\t%s\n", $5, $9}'

echo
echo "Content summary:"
if [ -f "$OUTPUT_DIR/main-content.txt" ]; then
  WORD_COUNT=$(wc -w < "$OUTPUT_DIR/main-content.txt")
  CHAR_COUNT=$(wc -c < "$OUTPUT_DIR/main-content.txt")
  echo "  - $WORD_COUNT words, $CHAR_COUNT characters"
fi

# Cleanup
agent-browser close

echo
echo "To view content:"
echo "  cat $OUTPUT_DIR/main-content.txt"
echo "  open $OUTPUT_DIR/screenshot-full.png"
echo "  open $OUTPUT_DIR/page.pdf"
