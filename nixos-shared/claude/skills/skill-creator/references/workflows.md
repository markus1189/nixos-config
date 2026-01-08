# Workflow Patterns

## Contents
- Sequential Workflows
- Conditional Workflows
- Feedback Loops
- Checklist Pattern

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