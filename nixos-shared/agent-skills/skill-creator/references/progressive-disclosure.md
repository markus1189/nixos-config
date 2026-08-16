# Progressive Disclosure Patterns

## Contents
- Pattern 1: High-level guide with references
- Pattern 2: Domain-specific organization
- Pattern 3: Conditional details

## Pattern 1: High-level guide with references

Keep main instructions in SKILL.md, link to detailed docs.

```markdown
# PDF Processing

## Quick start

Extract text with pdfplumber:
```python
import pdfplumber
with pdfplumber.open("file.pdf") as pdf:
    text = pdf.pages[0].extract_text()
```

## Advanced features

- **Form filling**: See [FORMS.md](references/FORMS.md) for complete guide
- **API reference**: See [REFERENCE.md](references/REFERENCE.md) for all methods
- **Examples**: See [EXAMPLES.md](references/EXAMPLES.md) for common patterns
```

Claude loads FORMS.md, REFERENCE.md, or EXAMPLES.md only when needed.

## Pattern 2: Domain-specific organization

For skills with multiple domains, organize by domain to avoid loading irrelevant context.

**Example: Analytics skill**
```
bigquery-skill/
├── SKILL.md (overview and navigation)
└── references/
    ├── finance.md (revenue, billing metrics)
    ├── sales.md (opportunities, pipeline)
    ├── product.md (API usage, features)
    └── marketing.md (campaigns, attribution)
```

**SKILL.md content:**
```markdown
# BigQuery Data Analysis

## Available datasets

**Finance**: Revenue, ARR, billing → See [references/finance.md](references/finance.md)
**Sales**: Opportunities, pipeline, accounts → See [references/sales.md](references/sales.md)
**Product**: API usage, features, adoption → See [references/product.md](references/product.md)
**Marketing**: Campaigns, attribution, email → See [references/marketing.md](references/marketing.md)

## Quick search

Find specific metrics using grep:
```bash
grep -i "revenue" references/finance.md
grep -i "pipeline" references/sales.md
```
```

When user asks about sales metrics, Claude only reads sales.md.

**Example: Multi-framework skill**
```
cloud-deploy/
├── SKILL.md (workflow + provider selection)
└── references/
    ├── aws.md (AWS deployment patterns)
    ├── gcp.md (GCP deployment patterns)
    └── azure.md (Azure deployment patterns)
```

When user chooses AWS, Claude only reads aws.md.

## Pattern 3: Conditional details

Show basic content in SKILL.md, link to advanced content when needed.

```markdown
# DOCX Processing

## Creating documents

Use docx-js for new documents. See [references/DOCX-JS.md](references/DOCX-JS.md).

## Editing documents

For simple edits, modify the XML directly.

**For tracked changes**: See [references/REDLINING.md](references/REDLINING.md)
**For OOXML details**: See [references/OOXML.md](references/OOXML.md)
```

Claude reads REDLINING.md or OOXML.md only when user needs those features.

## Key Guidelines

1. **Keep references ONE level deep** - SKILL.md links directly to all reference files
2. **Structure files >100 lines** - Include table of contents at top
3. **Be explicit about when to read** - Don't assume Claude will discover references
4. **Name files descriptively** - `form_validation_rules.md` not `doc2.md`
