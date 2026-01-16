---
name: skill-creator
description: Guides creation of effective skills that extend Claude's capabilities with specialized knowledge, workflows, or tool integrations. Use when users want to create a new skill, update an existing skill, or ask about skill best practices and structure.
license: Complete terms in LICENSE.txt
---

# Skill Creator

This skill provides guidance for creating effective skills.

## About Skills

Skills are modular, self-contained packages that extend Claude's capabilities by providing
specialized knowledge, workflows, and tools. Think of them as "onboarding guides" for specific
domains or tasks—they transform Claude from a general-purpose agent into a specialized agent
equipped with procedural knowledge that no model can fully possess.

### What Skills Provide

1. Specialized workflows - Multi-step procedures for specific domains
2. Tool integrations - Instructions for working with specific file formats or APIs
3. Domain expertise - Company-specific knowledge, schemas, business logic
4. Bundled resources - Scripts, references, and assets for complex and repetitive tasks

## Core Principles

### Concise is Key

The context window is a public good. Skills share context with system prompt, conversation history, other Skills' metadata, and the user's request.

**Default assumption: Claude is already very smart.** Only add context Claude doesn't already have. Challenge each piece: "Does Claude really need this explanation?" and "Does this paragraph justify its token cost?"

Prefer concise examples over verbose explanations.

**Good** (~50 tokens):
```python
import pdfplumber
with pdfplumber.open("file.pdf") as pdf:
    text = pdf.pages[0].extract_text()
```

**Bad** (~150 tokens): "PDF files are a common file format that contains text, images... there are many libraries available... we recommend pdfplumber because..."

### Set Appropriate Degrees of Freedom

Match specificity to the task's fragility:

- **High freedom** (text instructions): Multiple approaches valid, context-dependent decisions
- **Medium freedom** (pseudocode/parameterized scripts): Preferred pattern exists, some variation acceptable
- **Low freedom** (specific scripts, no parameters): Fragile operations, consistency critical, exact sequence required

Analogy: Narrow bridge with cliffs = low freedom (exact instructions). Open field = high freedom (general direction).

### Anatomy of a Skill

Every skill has a required SKILL.md and optional bundled resources:

```
skill-name/                # Gerund form preferred: "processing-pdfs", "analyzing-data"
├── SKILL.md (required)
│   ├── YAML frontmatter (name + description required)
│   └── Markdown body (under 500 lines)
└── Bundled Resources (optional)
    ├── scripts/          - Executable code
    ├── references/       - Documentation loaded on-demand
    └── assets/           - Output files (templates, images, fonts)
```

#### SKILL.md Frontmatter

- **name**: max 64 chars, lowercase/numbers/hyphens only, no reserved words ("anthropic", "claude")
- **description**: max 1024 chars, **third person only**, no XML tags. This is the primary trigger mechanism.

**Critical**: Description determines when skill triggers. Include both what the skill does AND when to use it.

**Good description** (third person, specific triggers):
```yaml
description: "Extracts text and tables from PDF files, fills forms, merges documents. Use when working with PDF files or when the user mentions PDFs, forms, or document extraction."
```

**Bad descriptions**:
```yaml
description: "I can help you process Excel files"      # First person
description: "You can use this to process files"       # Second person
description: "Helps with documents"                    # Vague
```

#### Bundled Resources (optional)

##### Scripts (`scripts/`)

Executable code for deterministic reliability or frequently-rewritten operations.

- **When to include**: Code rewritten repeatedly or needing deterministic reliability
- **Benefits**: Token efficient (executed without loading into context), reliable, consistent
- **Key principle - Solve, don't punt**: Scripts should handle errors explicitly, not fail and let Claude figure it out

```python
# Good - handles errors:
def process_file(path):
    try:
        return open(path).read()
    except FileNotFoundError:
        print(f"Creating default: {path}")
        open(path, 'w').write('')
        return ''

# Bad - punts to Claude:
def process_file(path):
    return open(path).read()  # Just fails
```

##### References (`references/`)

Documentation loaded on-demand to inform Claude's process.

- **Use cases**: Database schemas, API docs, domain knowledge, company policies, workflow guides
- **External API/library docs**: When wrapping a library or API with publicly accessible documentation, link to official docs (both library wrapper and underlying API if applicable). Skip if docs require authentication or are otherwise inaccessible.
- **Benefits**: Keeps SKILL.md lean, loaded only when needed
- **Link syntax**: Always use markdown links: `See [references/guide.md](references/guide.md)`
- **Best practice**: For large files (>10k words), include grep search patterns in SKILL.md
- **Avoid duplication**: Content lives in SKILL.md OR references, not both
- **Structure**: For files >100 lines, include table of contents at top so Claude can preview scope

##### Assets (`assets/`)

Files used in output, not loaded into context.

- **Use cases**: Templates, images, icons, boilerplate code, fonts, sample documents
- **Benefits**: Separates output resources from documentation, zero context cost

#### What NOT to Include

Do NOT create extraneous documentation:
- README.md, INSTALLATION_GUIDE.md, QUICK_REFERENCE.md, CHANGELOG.md

Skills are for AI agents, not humans. No setup docs, changelogs, or user-facing documentation.

### Progressive Disclosure Design Principle

Skills use three-level loading:

1. **Metadata (name + description)** - Always in context (~100 words)
2. **SKILL.md body** - When skill triggers (<500 lines)
3. **Bundled resources** - As needed (scripts executed without loading)

Keep SKILL.md under 500 lines. Split content into reference files when approaching this limit, clearly describing when to read them.

**Key principle:** Keep only core workflow in SKILL.md. Move variant-specific details to reference files.

**Critical:** Keep references ONE level deep from SKILL.md. Nested references (A→B→C) cause incomplete reads.

See [references/progressive-disclosure.md](references/progressive-disclosure.md) for detailed patterns.

## Skill Creation Process

Skill creation involves these steps:

1. Understand the skill with concrete examples
2. Plan reusable skill contents (scripts, references, assets)
3. Initialize the skill (run init_skill.py)
4. Edit the skill (implement resources and write SKILL.md)
5. Package the skill (run package_skill.py)
6. Iterate based on real usage

Follow these steps in order, skipping only if there is a clear reason why they are not applicable.

### Step 1: Understanding the Skill with Concrete Examples

Skip this step only when the skill's usage patterns are already clearly understood. It remains valuable even when working with an existing skill.

To create an effective skill, clearly understand concrete examples of how the skill will be used. This understanding can come from either direct user examples or generated examples that are validated with user feedback.

For example, when building an image-editor skill, relevant questions include:

- "What functionality should the image-editor skill support? Editing, rotating, anything else?"
- "Can you give some examples of how this skill would be used?"
- "I can imagine users asking for things like 'Remove the red-eye from this image' or 'Rotate this image'. Are there other ways you imagine this skill being used?"
- "What would a user say that should trigger this skill?"

To avoid overwhelming users, avoid asking too many questions in a single message. Start with the most important questions and follow up as needed for better effectiveness.

Conclude this step when there is a clear sense of the functionality the skill should support.

### Step 2: Planning the Reusable Skill Contents

To turn concrete examples into an effective skill, analyze each example by:

1. Considering how to execute on the example from scratch
2. Identifying what scripts, references, and assets would be helpful when executing these workflows repeatedly

Example: When building a `pdf-editor` skill to handle queries like "Help me rotate this PDF," the analysis shows:

1. Rotating a PDF requires re-writing the same code each time
2. A `scripts/rotate_pdf.py` script would be helpful to store in the skill

Example: When designing a `frontend-webapp-builder` skill for queries like "Build me a todo app" or "Build me a dashboard to track my steps," the analysis shows:

1. Writing a frontend webapp requires the same boilerplate HTML/React each time
2. An `assets/hello-world/` template containing the boilerplate HTML/React project files would be helpful to store in the skill

Example: When building a `big-query` skill to handle queries like "How many users have logged in today?" the analysis shows:

1. Querying BigQuery requires re-discovering the table schemas and relationships each time
2. A `references/schema.md` file documenting the table schemas would be helpful to store in the skill

To establish the skill's contents, analyze each concrete example to create a list of the reusable resources to include: scripts, references, and assets.

### Step 3: Initializing the Skill

At this point, it is time to actually create the skill.

Skip this step only if the skill being developed already exists, and iteration or packaging is needed. In this case, continue to the next step.

When creating a new skill from scratch, always run the `init_skill.py` script. The script conveniently generates a new template skill directory that automatically includes everything a skill requires, making the skill creation process much more efficient and reliable.

Usage:

```bash
./scripts/init_skill.py <skill-name> --path <output-directory>
```

**Note:** The scripts have proper shebangs and should be executed directly - no need for `python` or `python3` prefix.

The script:

- Creates the skill directory at the specified path
- Generates a SKILL.md template with proper frontmatter and TODO placeholders
- Creates example resource directories: `scripts/`, `references/`, and `assets/`
- Adds example files in each directory that can be customized or deleted

After initialization, customize or remove the generated SKILL.md and example files as needed.

### Step 4: Edit the Skill

The skill is for another Claude instance. Include non-obvious procedural knowledge, domain-specific details, and reusable assets.

#### Design Pattern References

- **Multi-step processes**: See [references/workflows.md](references/workflows.md)
- **Output formats**: See [references/output-patterns.md](references/output-patterns.md)
- **Anti-patterns to avoid**: See [references/anti-patterns.md](references/anti-patterns.md)
- **Final checklist**: See [references/checklist.md](references/checklist.md)

#### Implementation

1. Start with reusable resources (`scripts/`, `references/`, `assets/`)
2. Test added scripts by running them
3. Delete unused example files from initialization
4. Update SKILL.md frontmatter and body

**Frontmatter guidelines:**
- `name`: Skill name (see naming rules above)
- `description`: Primary trigger mechanism - include what it does AND when to use it
- **YAML syntax**: Wrap in double quotes if description contains colons or special characters

**Body guidelines:**
- Use imperative/infinitive form
- Avoid time-sensitive information (use "old patterns" sections instead)
- Use consistent terminology throughout

### Step 5: Packaging a Skill

Package into a distributable .skill file:

```bash
./scripts/package_skill.py <path/to/skill-folder> [output-directory]
```

The script validates first (frontmatter, naming, structure), then creates `skill-name.skill` (zip format).

### Step 6: Test and Iterate

**Test with multiple models:** Skills behave differently across models.
- Haiku may need more explicit guidance
- Opus may be over-explained by detailed instructions

**Evaluation-driven development:**
1. Run Claude on representative tasks WITHOUT the skill - document failures
2. Create 3+ test scenarios that exercise these gaps
3. Write minimal instructions to address gaps
4. Test again, compare against baseline, refine

**Iteration workflow:**
1. Use skill on real tasks
2. Observe struggles, missed connections, or ignored content
3. Update SKILL.md or bundled resources
4. Re-test

**What to observe:**
- Does Claude read files in unexpected order? → Structure may be unintuitive
- Does Claude miss references? → Links need to be more prominent
- Does Claude repeatedly read the same file? → Move that content to SKILL.md
- Does Claude never access a file? → May be unnecessary or poorly signaled
