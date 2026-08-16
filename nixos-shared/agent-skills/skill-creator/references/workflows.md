# Workflow Patterns

## Contents
- Sequential Workflows
- Conditional Workflows
- Feedback Loops
- Plan-Validate-Execute
- Visual Analysis
- Checklist Pattern
- Multi-MCP Coordination
- Context-Aware Tool Selection
- Domain-Specific Intelligence

## Sequential Workflows

For complex tasks, break operations into clear, sequential steps:

```markdown
Filling a PDF form involves these steps:

1. Analyze the form (run analyze_form.py)
2. Create field mapping (edit fields.json)
3. Validate mapping (run validate_fields.py)
4. Fill the form (run fill_form.py)
5. Verify output (run verify_output.py)
```

## Conditional Workflows

For tasks with branching logic, guide Claude through decision points:

```markdown
1. Determine the modification type:
   **Creating new content?** → Follow "Creation workflow" below
   **Editing existing content?** → Follow "Editing workflow" below

2. Creation workflow: [steps]
3. Editing workflow: [steps]
```

## Feedback Loops

The "run validator → fix errors → repeat" pattern greatly improves output quality.

**Pattern: Validate after each change**

```markdown
## Document editing process

1. Make your edits to `word/document.xml`
2. **Validate immediately**: `python scripts/validate.py unpacked_dir/`
3. If validation fails:
   - Review the error message carefully
   - Fix the issues
   - Run validation again
4. **Only proceed when validation passes**
5. Rebuild: `python scripts/pack.py unpacked_dir/ output.docx`
```

**Pattern: Style guide compliance (without code)**

```markdown
## Content review process

1. Draft content following STYLE_GUIDE.md
2. Review against checklist:
   - Check terminology consistency
   - Verify examples follow standard format
   - Confirm all required sections present
3. If issues found:
   - Note each issue with specific section reference
   - Revise content
   - Review checklist again
4. Only finalize when all requirements met
```

## Plan-Validate-Execute

For complex open-ended tasks (batch operations, destructive changes, high-stakes mutations), insert a verifiable intermediate output between analysis and execution. Claude first writes a plan as structured data, a script validates the plan, then execution consumes it.

**Pattern: analyze → plan file → validate → execute → verify**

```markdown
## Bulk field update workflow

1. **Analyze**: `./scripts/extract_fields.py input.pdf > fields.json`
2. **Plan**: Write proposed changes to `changes.json` (field → new value)
3. **Validate**: `./scripts/validate_changes.py fields.json changes.json`
   - Checks field existence, type compatibility, required-field coverage
   - Fix errors in changes.json and re-validate until clean
4. **Execute**: `./scripts/apply_changes.py input.pdf changes.json output.pdf`
5. **Verify**: `./scripts/verify_output.py output.pdf`
```

**Why this works:**
- Catches errors before they touch the artifact
- Machine-verifiable (objective assertions, not vibes)
- Plan is reversible — iterate freely without re-running expensive steps
- Validation errors point at a single line in a small file, not a failed multi-step operation

**When to use:** batch operations over N items, destructive/irreversible steps, non-trivial validation rules, anything where partial failure is expensive to recover from.

**Implementation tip:** Make validators verbose with actionable error messages. `"Field 'signature_date' not found. Available: customer_name, order_total, signature_date_signed"` beats `"Invalid field"`.

## Visual Analysis

When inputs have spatial structure (forms, diagrams, layouts, rendered UI), convert them to images and let Claude's vision do the analysis instead of parsing raw structure.

```markdown
## Form layout analysis

1. Render pages to images: `./scripts/pdf_to_images.py form.pdf`
2. Claude reads each page image and identifies field locations, types, and groupings visually
3. Output structured field map
```

**When to use:** PDF form layouts, chart/diagram interpretation, screenshot-driven UI testing, OCR-free document structure extraction, visual diffs. Cheaper and more robust than writing brittle coordinate-parsing code for inputs humans read visually.

**When NOT to use:** pure text content (extract text directly), high-volume batch processing where vision cost dominates, inputs with reliable structured representations (use the structure).

## Checklist Pattern

For complex workflows, provide a checklist Claude can track:

```markdown
## PDF form filling workflow

Copy this checklist and check off items as you complete them:

Task Progress:
- [ ] Step 1: Analyze the form (run analyze_form.py)
- [ ] Step 2: Create field mapping (edit fields.json)
- [ ] Step 3: Validate mapping (run validate_fields.py)
- [ ] Step 4: Fill the form (run fill_form.py)
- [ ] Step 5: Verify output (run verify_output.py)
```

Checklists help both Claude and users track progress through multi-step workflows.

## Multi-MCP Coordination

For workflows spanning multiple services, organize into clear phases:

```markdown
### Phase 1: Design Export (Figma MCP)
1. Export design assets from Figma
2. Generate design specifications
3. Create asset manifest

### Phase 2: Asset Storage (Drive MCP)
1. Create project folder in Drive
2. Upload all assets
3. Generate shareable links

### Phase 3: Task Creation (Linear MCP)
1. Create development tasks
2. Attach asset links to tasks
3. Assign to engineering team
```

Key techniques:
- Clear phase separation
- Data passing between MCPs explicitly noted
- Validation before moving to next phase
- Centralized error handling

## Context-Aware Tool Selection

When the same outcome can use different tools depending on context:

```markdown
## Smart File Storage

### Decision Tree
1. Check file type and size
2. Determine best storage location:
   - Large files (>10MB): Use cloud storage MCP
   - Collaborative docs: Use Notion/Docs MCP
   - Code files: Use GitHub MCP
   - Temporary files: Use local storage

### Execute Storage
Based on decision:
- Call appropriate MCP tool
- Apply service-specific metadata
- Generate access link

### Provide Context to User
Explain why that storage was chosen
```

Key techniques:
- Clear decision criteria
- Fallback options
- Transparency about choices

## Domain-Specific Intelligence

Embed specialized knowledge that goes beyond tool access:

```markdown
## Payment Processing with Compliance

### Before Processing (Compliance Check)
1. Fetch transaction details via MCP
2. Apply compliance rules:
   - Check sanctions lists
   - Verify jurisdiction allowances
   - Assess risk level
3. Document compliance decision

### Processing
IF compliance passed:
- Call payment processing MCP tool
- Apply appropriate fraud checks
- Process transaction
ELSE:
- Flag for review
- Create compliance case

### Audit Trail
- Log all compliance checks
- Record processing decisions
- Generate audit report
```

Key techniques:
- Domain expertise embedded in logic
- Compliance/validation before action
- Comprehensive documentation
- Clear governance rules