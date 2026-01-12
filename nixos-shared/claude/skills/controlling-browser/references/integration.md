# Browser Script Integration Patterns

This guide explains how to use browser script outputs with Claude's tools (Bash + Read).

## Pattern 1: Screenshot Workflow (30 lines)

### Basic Screenshot Capture
```bash
# Capture screenshot and store path
SCREENSHOT=$(./scripts/screenshot.js --full-page)
echo "Screenshot saved to: $SCREENSHOT"
```

**Then: Use Read tool**
- Read the file at `$SCREENSHOT` to view the image visually
- Claude's multimodal capabilities will process the image
- Describe what you see to the user in detail

**Processing Results:**
```bash
# Check if screenshot was successful
if [ -f "$SCREENSHOT" ]; then
    # Use Read tool on $SCREENSHOT
    echo "Successfully captured page screenshot"
else
    echo "Error: Screenshot failed" >&2
    exit 1
fi
```

**Use Cases:**
- Visual debugging: "Show me what the page looks like"
- Layout verification: "Check if the element is visible"
- State inspection: "Capture the current page state"

**Error Handling:**
- Check if file exists before using Read tool
- Verify file is not empty (size > 0)
- Handle timeout errors gracefully

## Pattern 2: Content Extraction (40 lines)

### Extract and Process JSON/Text
```bash
# Extract structured data from page
CONTENT=$(./scripts/extract-content.js --selector 'article' --format json)

# Check exit code
if [ $? -eq 0 ]; then
    # Save to temporary file for Read tool
    TEMP_FILE=$(mktemp -t claude-code.XXXXXX.json)
    echo "$CONTENT" > "$TEMP_FILE"
    echo "$TEMP_FILE"
else
    echo "Error: Content extraction failed" >&2
    exit 1
fi
```

**Then: Use Read tool**
- Read the temporary JSON file
- Parse and analyze the structured data
- Present findings to user in readable format

**Alternative: Direct Processing**
```bash
# For small outputs, process directly without Read tool
TITLE=$(./scripts/extract-content.js --selector 'h1' --text)
echo "Page title: $TITLE"

# For large outputs, always save and use Read tool
ARTICLE=$(./scripts/extract-content.js --selector 'article')
ARTICLE_FILE=$(mktemp -t claude-code.XXXXXX.html)
echo "$ARTICLE" > "$ARTICLE_FILE"
# Then: Use Read tool on $ARTICLE_FILE
```

**Processing Guidelines:**
- Small text (< 1000 chars): Process directly in bash
- Large text or structured data: Save to temp file, use Read tool
- HTML content: Save as .html, read and parse structure
- JSON data: Save as .json, read and analyze fields

**Presenting Results:**
- Summarize key findings first
- Show relevant excerpts or data points
- Suggest next actions based on content

## Pattern 3: Interactive Picking (30 lines)

### Element Selection Workflow
```bash
# Step 1: List available elements
./scripts/list-elements.js --selector 'button' --format table

# User reviews output and chooses

# Step 2: Interact with specific element
RESULT=$(./scripts/click-element.js --selector 'button[data-id="submit"]')
if [ $? -eq 0 ]; then
    echo "Successfully clicked element"

    # Step 3: Capture result (optional screenshot)
    AFTER_SCREENSHOT=$(./scripts/screenshot.js)
    # Use Read tool on $AFTER_SCREENSHOT to verify action
else
    echo "Error: Failed to click element" >&2
fi
```

**Interactive Pattern:**
1. List options using browser script
2. Present options to user in readable format
3. Accept user selection
4. Execute action with selected element
5. Verify result (screenshot or content check)

**Use Cases:**
- Form field selection: "Which input should I fill?"
- Link navigation: "Which link should I click?"
- Multi-step workflows: "Select product, then checkout"

## Pattern 4: JSON Processing (30 lines)

### Structured Data Workflow
```bash
# Execute script that returns JSON
JSON_OUTPUT=$(./scripts/get-network-requests.js --filter 'api')

# Save to file for Read tool
JSON_FILE=$(mktemp -t claude-code.XXXXXX.json)
echo "$JSON_OUTPUT" > "$JSON_FILE"

# Then: Use Read tool on $JSON_FILE to analyze structure
```

**After Reading JSON:**
```bash
# Extract specific fields using jq
echo "$JSON_OUTPUT" | jq -r '.requests[] | select(.status == 200) | .url'

# Count items
REQUEST_COUNT=$(echo "$JSON_OUTPUT" | jq '.requests | length')
echo "Found $REQUEST_COUNT API requests"

# Filter and format
echo "$JSON_OUTPUT" | jq '[.requests[] | {url: .url, status: .status, time: .timing.duration}]'
```

**Presentation Strategy:**
1. Read full JSON with Read tool (understand structure)
2. Use jq for targeted extraction
3. Format findings in tables or bullet points
4. Highlight anomalies or important patterns

## Pattern 5: Multi-command Pipelines (20 lines)

### Chaining Browser Operations
```bash
# Navigate, wait, extract, screenshot
./scripts/navigate.js --url "https://example.com" && \
./scripts/wait-for-element.js --selector '.content' --timeout 5000 && \
CONTENT=$(./scripts/extract-content.js --selector '.content' --format json) && \
SCREENSHOT=$(./scripts/screenshot.js --selector '.content')

# Check pipeline success
if [ $? -eq 0 ]; then
    CONTENT_FILE=$(mktemp -t claude-code.XXXXXX.json)
    echo "$CONTENT" > "$CONTENT_FILE"
    # Use Read tool on both $CONTENT_FILE and $SCREENSHOT
    echo "Pipeline completed: $CONTENT_FILE $SCREENSHOT"
else
    echo "Pipeline failed at step $?" >&2
fi
```

## Error Handling

### Exit Code Checking
```bash
./scripts/navigate.js --url "https://example.com"
EXIT_CODE=$?

if [ $EXIT_CODE -ne 0 ]; then
    echo "Navigation failed with exit code: $EXIT_CODE" >&2
    case $EXIT_CODE in
        1) echo "Timeout or network error" ;;
        2) echo "Invalid arguments" ;;
        *) echo "Unknown error" ;;
    esac
    exit $EXIT_CODE
fi
```

### Capturing stderr
```bash
# Capture both stdout and stderr
OUTPUT=$(./scripts/screenshot.js 2>&1)
EXIT_CODE=$?

if [ $EXIT_CODE -ne 0 ]; then
    echo "Script error: $OUTPUT" >&2
    # Check for specific error patterns
    if echo "$OUTPUT" | grep -q "timeout"; then
        echo "Increasing timeout and retrying..."
        ./scripts/screenshot.js --timeout 10000
    fi
fi
```

### Recovery Strategies
```bash
# Retry with exponential backoff
MAX_RETRIES=3
RETRY=0
SUCCESS=false

while [ $RETRY -lt $MAX_RETRIES ] && [ "$SUCCESS" = false ]; do
    if OUTPUT=$(./scripts/navigate.js --url "$URL" 2>&1); then
        SUCCESS=true
    else
        RETRY=$((RETRY + 1))
        WAIT=$((2 ** RETRY))
        echo "Retry $RETRY/$MAX_RETRIES after ${WAIT}s..." >&2
        sleep $WAIT
    fi
done

if [ "$SUCCESS" = false ]; then
    echo "Failed after $MAX_RETRIES attempts" >&2
    exit 1
fi
```

### Cleanup on Error
```bash
# Always cleanup temporary files
TEMP_FILE=$(mktemp -t claude-code.XXXXXX.json)
trap "rm -f $TEMP_FILE" EXIT

# Run script with error handling
if OUTPUT=$(./scripts/extract-content.js 2>&1); then
    echo "$OUTPUT" > "$TEMP_FILE"
    # Use Read tool on $TEMP_FILE
else
    echo "Extraction failed: $OUTPUT" >&2
    # Temp file automatically cleaned up by trap
    exit 1
fi
```
