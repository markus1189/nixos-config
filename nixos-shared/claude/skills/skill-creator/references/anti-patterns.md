# Anti-Patterns to Avoid

## Contents
- Description Anti-Patterns
- Structure Anti-Patterns
- Content Anti-Patterns
- Script Anti-Patterns

## Description Anti-Patterns

### Wrong Point of View

Descriptions are injected into the system prompt. Inconsistent POV causes discovery problems.

**Bad:**
```yaml
description: "I can help you process Excel files"      # First person
description: "You can use this to process files"       # Second person
```

**Good:**
```yaml
description: "Processes Excel files and generates reports"  # Third person
```

### Vague Descriptions

Claude uses descriptions to choose from 100+ skills. Be specific.

**Bad:**
```yaml
description: "Helps with documents"
description: "Processes data"
description: "Does stuff with files"
```

**Good:**
```yaml
description: "Extracts text and tables from PDF files, fills forms, merges documents. Use when working with PDF files or when the user mentions PDFs, forms, or document extraction."
```

## Structure Anti-Patterns

### Deeply Nested References

Claude may partially read files when they're referenced from other referenced files.

**Bad (too deep):**
```
SKILL.md → advanced.md → details.md → actual_info.md
```

**Good (one level deep):**
```
SKILL.md → advanced.md
SKILL.md → details.md
SKILL.md → reference.md
```

### Windows-Style Paths

Always use forward slashes, even on Windows.

**Bad:** `scripts\helper.py`, `reference\guide.md`
**Good:** `scripts/helper.py`, `reference/guide.md`

### Backticked References Instead of Links

Reference files must use markdown link syntax so Claude can navigate to them.

**Bad:**
```markdown
See: `references/guide.md`
See references/guide.md for details
```

**Good:**
```markdown
See [references/guide.md](references/guide.md)
**Advanced features**: See [references/advanced.md](references/advanced.md)
```

### Missing Table of Contents

For reference files >100 lines, include a TOC so Claude can preview scope.

## Content Anti-Patterns

### Too Many Options

Don't overwhelm with choices. Provide a default with escape hatch.

**Bad:**
```markdown
You can use pypdf, or pdfplumber, or PyMuPDF, or pdf2image, or...
```

**Good:**
```markdown
Use pdfplumber for text extraction:
```python
import pdfplumber
```

For scanned PDFs requiring OCR, use pdf2image with pytesseract instead.
```

### Time-Sensitive Information

Information that becomes outdated causes problems.

**Bad:**
```markdown
If you're doing this before August 2025, use the old API.
After August 2025, use the new API.
```

**Good:**
```markdown
## Current method

Use the v2 API endpoint: `api.example.com/v2/messages`

## Old patterns

<details>
<summary>Legacy v1 API (deprecated)</summary>
The v1 API used: `api.example.com/v1/messages`
This endpoint is no longer supported.
</details>
```

### Inconsistent Terminology

Pick one term and use it throughout.

**Bad:** Mix "API endpoint", "URL", "API route", "path"
**Good:** Always "API endpoint"

### Explaining What Claude Already Knows

Claude knows what PDFs are. Don't waste tokens.

**Bad (~150 tokens):**
```markdown
PDF (Portable Document Format) files are a common file format that contains
text, images, and other content. To extract text from a PDF, you'll need to
use a library. There are many libraries available for PDF processing...
```

**Good (~50 tokens):**
```python
import pdfplumber
with pdfplumber.open("file.pdf") as pdf:
    text = pdf.pages[0].extract_text()
```

## Script Anti-Patterns

### Punting to Claude

Scripts should solve problems, not fail and let Claude figure it out.

**Bad:**
```python
def process_file(path):
    return open(path).read()  # Just fails
```

**Good:**
```python
def process_file(path):
    try:
        return open(path).read()
    except FileNotFoundError:
        print(f"Creating default: {path}")
        open(path, 'w').write('')
        return ''
```

### Voodoo Constants

Configuration parameters should be justified and documented.

**Bad:**
```python
TIMEOUT = 47  # Why 47?
RETRIES = 5   # Why 5?
```

**Good:**
```python
# HTTP requests typically complete within 30 seconds
# Longer timeout accounts for slow connections
REQUEST_TIMEOUT = 30

# Three retries balances reliability vs speed
# Most intermittent failures resolve by the second retry
MAX_RETRIES = 3
```

### Assuming Tools Are Installed

Don't assume packages are available.

**Bad:**
```markdown
Use the pdf library to process the file.
```

**Good:**
```markdown
Install required package: `pip install pypdf`

Then use it:
```python
from pypdf import PdfReader
reader = PdfReader("file.pdf")
```
```

### Missing MCP Tool Prefixes

When referencing MCP tools, use fully qualified names.

**Bad:** `Use the bigquery_schema tool`
**Good:** `Use the BigQuery:bigquery_schema tool`
