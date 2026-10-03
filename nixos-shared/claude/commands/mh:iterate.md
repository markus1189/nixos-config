---
description: Iteratively refine a task through N sequential improvement passes
argument-hint: [N] <task-description>
---

# Iterative Task Refinement

Execute a task through sequential refinement passes where each iteration improves upon the previous result. Each agent critiques its predecessor and builds a progressively better solution.

## Argument Parsing

Extract iteration count and task description from arguments:

### Syntax
```
/mh:iterate <task-description>              # Default: 3 iterations
/mh:iterate [N] <task-description>          # Explicit: N iterations (1-5)
```

### Parsing Logic
1. **Leading integer?** If the first argument is an integer, it is N; clamp to 1-5 and tell the user if clamped ("Maximum 5 iterations, using N=5"). Otherwise N=3 and the whole argument string is the task.
2. **Remaining arguments**: the task description. If empty, stop and ask for one (`/mh:iterate [N] <task-description>`).

### Examples
- `/mh:iterate write a Python CSV parser` → N=3, task="write a Python CSV parser"
- `/mh:iterate 4 create a REST API design` → N=4
- `/mh:iterate 12 complex task` → N=5 (clamped, warn user)
- `/mh:iterate 0 complex task` → N=1 (clamped, warn user)

## Clarification (If Needed)

Before starting iterations, assess if the task requires clarification. Use AskUserQuestion when fundamental ambiguities would lead to significantly different solutions.

**Ask about**:
- Language/technology choice: "write a CSV parser" → Which language?
- Scope boundaries: "make it faster" → What component?
- Output expectations: "design a REST API" → What domain/resources?

**Don't ask about**:
- Style preferences, minor details, edge cases (can refine in iterations)
- Common conventions (modern practices, standard formats)

**Principle**: Clarify fundamental requirements upfront, but leverage the iterative process for refinement of details.

## Execution Flow

After argument parsing and any necessary clarification, proceed with the iterative refinement:

### Step 1: Create Task List

Add N + 1 items to your task list: `Initial pass: [task]`, then `Refinement pass [I]: improve iteration [I-1] result` for I = 2..N, then `Synthesize final report`.

### Step 2: Sequential Iteration Loop

For each iteration from 1 to N:

#### Mark task in_progress
Mark the current iteration's task `in_progress` before starting work.

#### Build Agent Prompt

**For Iteration 1 (Initial Pass):**

```
You are working on the initial implementation of a task that will be iteratively refined.

Your task: [TASK_DESCRIPTION]

## Your Role
This is iteration 1 of [N] total refinement passes. You're creating the initial version that will be improved in subsequent iterations.

## Requirements
1. Complete the task thoroughly and professionally
2. Focus on correctness and clarity
3. Use best practices and appropriate patterns
4. Consider edge cases and error handling
5. Provide working, complete solutions (not stubs or placeholders)

## CRITICAL: Self-Critique Required
After completing your work, you MUST provide a self-critique section analyzing your implementation. This critique will guide the next iteration's improvements.

## Required Output Structure

### Implementation
[Your complete work here - be thorough and professional]


### Self-Critique
**REQUIRED**: Analyze your own work with these subsections:

**Strengths:**
- [What works well, good decisions]

**Areas for Improvement:**
- [Weaknesses, limitations, alternative approaches]

**Specific Suggestions for Next Iteration:**
- [Concrete, specific improvements and issues to address]

Remember: Your self-critique will guide the next iteration's improvements. Be honest, specific, and constructive. Identify real opportunities for enhancement.
```

**For Iterations 2 through N (Refinement Passes):**

```
You are refining previous work through iterative improvement.

Original task: [TASK_DESCRIPTION]

This is iteration [CURRENT] of [N] total refinement passes.

## Previous Iteration Result

[PREVIOUS_RESULT_FULL_TEXT]

## Previous Iteration's Self-Critique

[PREVIOUS_CRITIQUE_FULL_TEXT]

## Your Role
Improve upon the previous iteration by:
1. Addressing issues identified in the self-critique
2. Implementing suggested enhancements
3. Refining weak areas
4. Adding improvements you identify beyond the critique

## Requirements
- Start from the previous result (don't start from scratch unless fundamentally broken)
- Address the specific improvement suggestions from the critique
- Maintain what's already working well
- Add meaningful refinements, not superficial changes
- Consider if the previous critique missed anything important
- Provide complete, working solutions (not TODOs or placeholders)

## CRITICAL: Improvement Notes Required
After completing your refinement, you MUST document what you improved and why.

## Required Output Structure

### Refined Implementation
[Your improved version here - build on previous work]

[The complete refined version, not just the changes]

### Improvement Notes
**REQUIRED**: Document your refinements with these subsections:

**Changes Made:**
- [What you changed and why it's better]

**Critique Items Addressed:**
- [How you addressed each previous suggestion]

**Additional Refinements:**
- [Improvements beyond the critique]

[ONLY FOR NON-FINAL ITERATIONS (when CURRENT < N):]
**Remaining Considerations:**
- [What could still be improved; remaining trade-offs]

Remember: Each iteration should be meaningfully better than the last. Show clear improvement, not just superficial changes.
```

#### Launch Subagent

Launch one general-purpose subagent with the subagent (Agent) tool: description `Iteration [I]/[N]: [brief task]`, prompt = the complete prompt above.

#### Extract Result

Keep the agent's full output (passed to the next iteration), plus its implementation section (for the final report) and its Self-Critique / Improvement Notes section (for the evolution summary). Split on the markdown headers and tolerate format variations; missing sections → see Edge Cases.

#### Mark task completed

Mark the current iteration's task `completed` immediately after it succeeds.

#### Continue to Next Iteration

If current iteration < N, proceed to next iteration with extracted result as input.

### Step 3: Synthesis and Final Output

After all N iterations complete, synthesize the evolution into a comprehensive final report.

Do NOT dump the N iterations sequentially or present raw output. Put the final result first and complete; summarize the evolution concisely (3-5 bullets per iteration: what changed and why, never re-pasting whole implementations).

## Output Structure

```markdown
# Iterative Refinement Results

**Task**: [Task description]
**Iterations completed**: [N]

## Final Result

[The complete final implementation from iteration N — what the user gets]

## Evolution Summary

### Iteration 1: Initial Implementation
**Approach taken:** [main decisions]
**Self-identified issues:** [what it flagged]

### Iteration [I]: Refinement (repeat for 2..N)
**Improvements made:** [specific enhancements]
**Key changes from iteration [I-1]:** [what changed and why]

## Quality Progression

**Initial → Final:** [how the solution evolved]
**Most significant improvements:**
1. [...]
2. [...]
3. [...]
```

### Edge Cases

- **N=1**: skip the evolution summary; present the result with its self-critique and the note "Single-pass execution (no iterative refinement performed)"
- **Iteration failed**: say "Iteration [I] failed: [reason]", present the last successful result as final, suggest retrying with a more specific task
- **Critique/notes missing**: warn, pass the full result text on, and tell the next iteration the critique was incomplete so it reviews the output itself
- **Very long results**: always pass full text to the next iteration and never truncate the final deliverable; condense only the evolution summary
- **No improvements suggested**: valid; remaining iterations focus on polish and validation

## Key Principles

1. **Sequential, Not Parallel**: Each iteration waits for previous to complete. NEVER launch iterations in parallel.
2. **Self-Improving Loop**: Each iteration uses previous result + critique as input for targeted improvement.
3. **Structured Reflection**: Agents must provide critique/improvement notes in structured format.
4. **Respect Limits**: 1-5 iterations only, default 3.
5. **Task Tracking**: Keep your task list current so progress through the iterations is visible.
6. **Complete Outputs**: Each iteration produces complete, working solutions, not stubs or TODOs.
7. **Meaningful Refinement**: Each iteration should add genuine value, not superficial changes.

---

Remember: The goal is **progressive refinement through structured self-critique and improvement**, delivering a polished final result with transparent evolution tracking that demonstrates the value of the iterative process.