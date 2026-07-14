---
name: skill-creator
description: Guides creation of effective skills that extend Claude's capabilities with specialized knowledge, workflows, or tool integrations. Use when users want to create a new skill, update an existing skill, review a skill, or ask about skill best practices and structure.
license: Complete terms in LICENSE.txt
---

# Skill Creator

This skill provides guidance for creating effective skills.

## About Skills

Skills are modular, self-contained packages that extend Claude's capabilities by providing
specialized knowledge, workflows, and tools. Think of them as "onboarding guides" for specific
domains or tasks—they transform Claude from a general-purpose agent into a specialized agent
equipped with procedural knowledge that no model can fully possess.

### Common Skill Categories

1. **Document & Asset Creation** - Create consistent output (reports, presentations, designs, code)
2. **Workflow Automation** - Multi-step processes with consistent methodology
3. **MCP Enhancement** - Workflow guidance on top of MCP tool access (turns raw tools into reliable workflows)

### Skill Archetypes

Orthogonal to the categories above, most skills follow one of two structural patterns:

- **Toolbox** — Scripts that encapsulate complexity + SKILL.md that teaches when/how to use them. Claude focuses on orchestration, not implementation.
- **Knowledge Injection** — A batch of domain knowledge Claude didn't have before. References and docs that make Claude instantly expert in a topic.

Most skills are a mix, but naming the patterns helps decide *what kind of value* the skill provides and where to invest effort (scripts vs. references).

### Problem-First vs. Tool-First Design

Choose your approach early:
- **Problem-first**: User describes outcome ("set up a project workspace") → skill orchestrates tools
- **Tool-first**: User has tool access ("I have Notion MCP") → skill provides expertise and best practices

Most skills lean one direction. Problem-first skills focus on workflow orchestration; tool-first skills embed domain knowledge and optimal patterns.

## Core Principles

### Communicating with the User

Pay attention to context cues to understand the user's familiarity with coding concepts. Users range from first-time terminal users to experienced developers. In the default case:
- "evaluation" and "benchmark" are borderline but OK
- For "JSON" and "assertion", look for cues the user knows these terms before using them without explanation
- When in doubt, briefly explain terms with a short inline definition

### Principle of Lack of Surprise

Skills must not contain malware, exploit code, or content that could compromise system security. A skill's contents should not surprise the user in their intent if described.

### Concise is Key

The context window is a public good. Skills share context with system prompt, conversation history, other Skills' metadata, and the user's request.

An invoked SKILL.md enters the conversation as one message and stays there for the rest of the session — it is not re-read on later turns. Every line is a recurring cost, not a one-time read.

**Default assumption: Claude is already very smart.** For every line in a skill, apply this three-part test:

1. **Is this outside my training data?** (learned through research, experimentation, experience)
2. **Is this context-specific?** (I know it now but won't after context clears)
3. **Does this align future Claude with current Claude?** (behavioral guidance for consistency)

If none of these are true, delete the line. Skills with no external signal actively hurt performance — they dilute the context with things the model already knows.

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

**Err on the side of broad descriptions.** The error costs are asymmetric: if Claude loads a skill then decides not to use it — small token cost. If Claude doesn't load a skill and spirals on an already-solved problem — the session is ruined.

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

**Why scripts beat prose:** Over a long session, SKILL.md text gets pushed out of Claude's active attention window. But a script's interface (`--help`, argument names, error messages) remains discoverable via tool use at any point. Invest in clear `--help` output and descriptive argument names — script API design matters more than SKILL.md prose quality for long-running sessions.

**Reducing mistake surface area:** Over a session, Claude *will* make mistakes — typos, forgotten flags, wrong directories, skipped steps. Scripts reduce the surface area for these errors:
1. **Single-touch** — fold setup and teardown into the tool. One command does the whole job.
2. **Clean primitives** — expose composable operations, not monolithic scripts with complex interdependencies.
3. **Repo-specific** — generic tools already exist. The unique workflows in your repo are where automation pays off most.

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

**Script path conventions:**

When documenting script usage in SKILL.md, always use simple relative paths from the skill directory:

```bash
# Good - Simple and standard
./scripts/script-name.sh [args]
./scripts/another-script.py --flag value
```

**Never** use complex path resolution patterns:

```bash
# Bad - Unnecessarily complex
SKILL_DIR=$(dirname /path/to/skill/SKILL.md)
cd "$SKILL_DIR/scripts" && ./script.sh

# Bad - Absolute paths
~/.claude/skills/skill-name/scripts/script.sh
```

**Rationale:** When Claude loads a skill from `~/.claude/skills/skill-name/SKILL.md`, it can determine the skill directory from the file path. The simple `./scripts/` pattern is:
- Consistent across all skills
- Easy to read and understand
- Works when Claude changes to the skill directory before execution

**Standard note to include at the end of SKILL.md:**

```markdown
**Script Execution:** Scripts should be executed from the skill directory. 
All scripts use Nix shebangs so no manual dependency installation is required.
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

0. Experiment with the domain before writing
1. Understand the skill with concrete examples
2. Plan reusable skill contents (scripts, references, assets)
3. Initialize the skill (run init_skill.py)
4. Edit the skill (implement resources and write SKILL.md)
5. Package the skill (run package_skill.py)
6. Iterate based on real usage

Follow these steps in order, skipping only if there is a clear reason why they are not applicable.

### Step 0: Experiment Before Writing

Before creating a skill, experiment with the domain. Research CLIs and libraries, try them out, see what works and what breaks. Skills built on speculation are worse than no skill at all — LLM-generated context files that encode no real signal actively hurt performance. Only write from direct experience.

### Step 1: Understanding the Skill with Concrete Examples

Skip this step only when the skill's usage patterns are already clearly understood. It remains valuable even when working with an existing skill.

#### Capture Intent

Start by understanding what the user wants. The current conversation may already contain a workflow to capture (e.g., "turn this into a skill"). If so, extract answers from the conversation history first — the tools used, the sequence of steps, corrections made, input/output formats observed. The user may need to fill gaps, and should confirm before proceeding.

Key questions:
1. What should this skill enable Claude to do?
2. When should this skill trigger? (what user phrases/contexts)
3. What's the expected output format?
4. Should we set up test cases? (Skills with objectively verifiable outputs benefit; subjective skills often don't.)

#### Concrete Examples

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
- **Eval and benchmarking pipeline**: See [references/eval-workflow.md](references/eval-workflow.md)
- **JSON schemas for eval data**: See [references/schemas.md](references/schemas.md)

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
- Write standing instructions, not one-time steps. The body persists all session, so guidance phrased as "first, do X" reads as spent once X is done.
- Avoid time-sensitive information (use "old patterns" sections instead)
- Use consistent terminology throughout

### Step 5: Packaging a Skill

Package into a distributable .skill file:

```bash
./scripts/package_skill.py <path/to/skill-folder> [output-directory]
```

The script validates first (frontmatter, naming, structure), then creates `skill-name.skill` (zip format).

### Step 6: Test and Iterate

**Pro tip: Start with a single challenging task.** Iterate until Claude succeeds, then extract the winning approach into the skill. This provides faster signal than broad testing upfront.

**Test with multiple models:** Skills behave differently across models.
- Haiku may need more explicit guidance
- Opus may be over-explained by detailed instructions

**Success metrics** (aspirational targets):
- Triggers on 90%+ of relevant queries
- Completes workflow in expected tool call count
- 0 failed API calls per workflow
- Users don't need to redirect or clarify

**Evaluation-driven development:**
1. Run Claude on representative tasks WITHOUT the skill - document failures
2. Create 3+ test scenarios that exercise these gaps
3. Write minimal instructions to address gaps
4. Test again, compare against baseline, refine

**Full eval and benchmark pipeline:** For skills with objectively verifiable outputs, use the structured eval pipeline:
- Spawn with-skill + baseline subagent runs in parallel
- Grade outputs against assertions
- Aggregate into benchmark (pass_rate, timing, tokens with mean +/- stddev)
- Launch the interactive viewer for user review
- Read feedback, improve, repeat

See [references/eval-workflow.md](references/eval-workflow.md) for the complete eval workflow, including grading, benchmarking, viewer usage, and platform-specific notes. Skip this for subjective skills — direct user feedback is sufficient.

**Author-agent / test-agent loop (Claude A / Claude B):** Work with one Claude instance as the *author* (helps design and refine the skill) and a separate, fresh instance as the *tester* (loads the skill and runs real tasks). The author understands skill structure; the tester reveals gaps through actual usage. Loop: tester runs a real task → observe specific failure → return to author with the observation ("tester forgot to filter test accounts — filter rule is present but not prominent") → apply refinement → retest. This beats self-review because the tester has no prior context and can't compensate for unclear instructions.

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

**Debugging tip:** Ask Claude "When would you use the [skill name] skill?" - it will quote the description back, revealing what's missing or unclear.

### Description Optimization

After a skill is functionally complete, optimize the `description` field for triggering accuracy:

1. **Create trigger eval queries** — ~20 queries (mix of should-trigger and should-not-trigger), realistic and detailed with edge cases
2. **Review with user** — present via HTML template (`assets/eval_review.html`), user edits and exports
3. **Run the optimization loop** — `./scripts/run_loop.py` handles the full loop: train/test split, evaluation (3 runs per query), AI-powered improvement with extended thinking, iterates up to 5 times
4. **Apply the result** — take `best_description` from output, update SKILL.md frontmatter

This requires the `claude` CLI (`claude -p`) and an `ANTHROPIC_API_KEY`. See [references/eval-workflow.md](references/eval-workflow.md) for the detailed workflow.

### Common Issues and Fixes

**Undertriggering** (skill doesn't load when it should):
- Add more trigger phrases and keywords to description
- Include technical terms users might say

**Overtriggering** (skill loads for unrelated queries):
- Add negative triggers: "Do NOT use for simple data exploration"
- Be more specific about scope

**Instructions not followed:**
- Before adding emphasis, subtract. A rule that keeps getting ignored is usually drowning in a too-long file; prune competing lines before shouting louder.
- Don't assume it fell out of context. If a skill stops influencing behavior after the first response, the content is almost always still present and Claude is preferring other approaches. Strengthen the description, or move the requirement into a hook or script where it is deterministic rather than advisory.
- Put critical instructions at the top with ## Critical headers
- For validations, use scripts (code is deterministic; language isn't)
- Combat model "laziness" by adding to user prompts (not SKILL.md):
  - "Take your time to do this thoroughly"
  - "Quality is more important than speed"
  - "Do not skip validation steps"

**Large context issues** (slow responses, degraded quality):
- Keep SKILL.md under 5,000 words
- Move detailed docs to references/
- If 20+ skills enabled simultaneously, recommend selective enablement

**Compaction keeps only the head of the file.** After auto-compaction, the most recent invocation of each skill is re-attached, keeping the first 5,000 tokens of each, within a 25,000-token combined budget across all skills (most-recently-invoked first; older skills can be dropped entirely). Put load-bearing instructions early — content past the head is what vanishes first in a long session.
