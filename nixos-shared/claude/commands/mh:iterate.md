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
1. **First argument check**: Is it a single-digit number (1-9)?
   - If YES and between 1-5: Extract as iteration count N
   - If YES but >5: Set N=5, inform user about maximum limit
   - If NO: It's part of the task description, default N=3
2. **Remaining arguments**: Combine all remaining arguments as task description
3. **Validation**:
   - N must be between 1 and 5 (inclusive)
   - Task description must not be empty
   - If N < 1: Set N=1, inform user of minimum
   - If task empty: Stop immediately, ask user to provide task description

### Examples
- `/mh:iterate write a Python CSV parser` → N=3, task="write a Python CSV parser"
- `/mh:iterate 4 create a REST API design` → N=4, task="create a REST API design"
- `/mh:iterate 1 analyze this code pattern` → N=1, task="analyze this code pattern"
- `/mh:iterate 7 complex task` → N=5 (capped), warn user: "Maximum 5 iterations allowed, using N=5"
- `/mh:iterate 5 write bash script for backups` → N=5, task="write bash script for backups"

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

### Step 1: Create TODO List

Use TodoWrite to create a tracking structure with N items plus one synthesis item:

For iterations 1 to N:
- **Iteration 1**:
  - content: `"Initial pass: [task-description]"`
  - activeForm: `"Initial pass: [gerund form of first word] [rest of task]"`
  - status: "pending"

- **Iterations 2-N**:
  - content: `"Refinement pass [I]: improve iteration [I-1] result"`
  - activeForm: `"Refinement pass [I]: improving iteration [I-1] result"`
  - status: "pending"

**Examples of proper gerund form for activeForm**:
- "write a script" → "writing a script"
- "design an API" → "designing an API"
- "explain monads" → "explaining monads"
- "analyze the code" → "analyzing the code"
- "create a parser" → "creating a parser"

### Step 2: Sequential Iteration Loop

For each iteration from 1 to N:

#### Mark TODO as in_progress
Update the current iteration's TODO status to `in_progress` before starting work.

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

[For code tasks: Include complete, working code with proper structure]
[For design tasks: Include comprehensive design with all key components]
[For explanation tasks: Provide clear, complete explanations]

### Self-Critique
**REQUIRED**: Analyze your own work with these subsections:

**Strengths:**
- [What works well in your implementation]
- [Good decisions you made]
- [Strong points of the approach]

**Areas for Improvement:**
- [What could be better]
- [Alternative approaches to consider]
- [Weaknesses or limitations]

**Specific Suggestions for Next Iteration:**
- [Concrete improvement 1 with specific details]
- [Concrete improvement 2 with specific details]
- [Issues to address]
- [Enhancements to consider]

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

[Include the complete refined version, not just changes]
[Maintain all good aspects from previous iteration]
[Integrate your improvements thoroughly]

### Improvement Notes
**REQUIRED**: Document your refinements with these subsections:

**Changes Made:**
- [Specific improvement 1: what you changed and why it's better]
- [Specific improvement 2: what you changed and why it's better]
- [Additional changes and their rationale]

**Critique Items Addressed:**
- [How you addressed previous suggestion 1]
- [How you addressed previous suggestion 2]
- [How you addressed other critique points]

**Additional Refinements:**
- [Improvements you made beyond the critique]
- [New enhancements you identified and added]
- [Quality improvements not explicitly requested]

[ONLY FOR NON-FINAL ITERATIONS (when CURRENT < N):]
**Remaining Considerations:**
- [What could still be improved in future iterations if any]
- [Trade-offs or limitations that remain]
- [Potential next-level enhancements]

Remember: Each iteration should be meaningfully better than the last. Show clear improvement, not just superficial changes.
```

#### Launch Subagent

Use the Task tool to launch a general-purpose subagent with the constructed prompt:

```
Tool: Task
subagent_type: general-purpose
description: Iteration [I]/[N]: [brief task description]
prompt: [Complete agent prompt from above - full text with all sections]
```

#### Extract Result

When the subagent completes, extract and store from the agent's response:

**For iteration 1:**
- **Full result text**: Complete output (for passing to next iteration)
- **Implementation portion**: The actual work/solution (for final report)
- **Self-Critique portion**: The reflection section (for guiding next iteration)

**For iterations 2-N:**
- **Full result text**: Complete output (for passing to next iteration if not final)
- **Refined Implementation portion**: The improved work (for final report)
- **Improvement Notes portion**: The refinement documentation (for evolution summary)

**Extraction rules:**
- If critique/notes sections are missing or minimal: Issue warning but continue
- Store complete text to pass to next iteration for full context
- Parse out sections using markdown headers as boundaries
- Handle variations in agent output format gracefully

#### Mark TODO as completed

Update the current iteration's TODO status to `completed` immediately after successful completion.

#### Continue to Next Iteration

If current iteration < N, proceed to next iteration with extracted result as input.

### Step 3: Synthesis and Final Output

After all N iterations complete, synthesize the evolution into a comprehensive final report.

**IMPORTANT PRINCIPLES:**
- Do NOT simply dump all N iterations sequentially
- Do NOT present raw unprocessed output
- DO create a structured summary showing progression
- DO highlight the final polished result prominently
- DO show evolution concisely

## Output Structure

Present results in this format:

```markdown
# Iterative Refinement Results

**Task**: [Task description]
**Iterations completed**: [N]

---

## Final Result

[The complete final implementation from iteration N]
[This is what the user gets - present it clearly and completely]
[For code: Include all code with proper formatting]
[For designs: Include complete design]
[For explanations: Include full refined explanation]

---

## Evolution Summary

### Iteration 1: Initial Implementation
**Approach taken:**
- [Main decisions and approach in iteration 1]
- [Core structure established]

**Self-identified issues:**
- [Key issues iteration 1 flagged for improvement]
- [Limitations noted]

### Iteration 2: First Refinement
**Improvements made:**
- [Specific enhancement 1]
- [Specific enhancement 2]
- [Additional improvements]

**Key changes from iteration 1:**
- [What changed and why]
- [How quality improved]

[Continue for each iteration through N]

### Iteration [N]: Final Refinement
**Improvements made:**
- [Final improvements in last iteration]
- [Polishing and refinements]

**Key changes from iteration [N-1]:**
- [Final iteration changes]
- [Quality enhancements]

---

## Quality Progression

**Initial → Final transformation:**
- [High-level summary of how the solution evolved]
- [Key quality dimensions that improved through iterations]
- [Overall impact of refinement process]

**Most significant improvements across all iterations:**
1. [Major improvement category 1: description]
2. [Major improvement category 2: description]
3. [Major improvement category 3: description]

---

## Notes

- Total iterations: [N]
- Refinement approach: Sequential improvement with structured critique
- Each iteration built upon and refined the previous work
- Critique-driven improvement process ensured systematic enhancement
```

### Synthesis Guidelines

1. **Concise Evolution Tracking**
   - Don't reproduce full results for each iteration in the summary
   - Summarize key decisions and improvements at each stage
   - Focus on what changed and why, not repeating entire implementations
   - Keep each iteration summary to 3-5 bullet points

2. **Highlight Progression**
   - Show how quality improved across iterations
   - Identify breakthrough moments or key refinements
   - Note cumulative improvements
   - Demonstrate the value of the iterative process

3. **Final Result Prominence**
   - The final iteration's implementation must be clearly presented at the top
   - This is what the user primarily cares about
   - Evolution summary provides context and confidence in the quality
   - Make it easy to find and use the final result

4. **Handle Edge Cases**
   - **If N=1**: Skip evolution summary section, just present result with note: "Single-pass execution (no iterative refinement performed)"
   - **If iteration failed**: Note the failure, show last successful result, explain what happened
   - **If critique was missing**: Note this in evolution summary but continue with best-effort improvement
   - **If no improvements suggested**: Note that previous iteration was considered complete, final iteration focused on polish

## Edge Case Handling

### N=1 (Single Pass)
- Create simple TODO list with one item
- Run single agent with initial pass prompt (including self-critique requirement)
- Present result with note: "Single-pass execution (no iterative refinement performed). Self-critique provided for reference."
- Still include self-critique section as it provides valuable reflection on the work

### Agent Failure
If an agent fails or returns error during any iteration:
- Note the failure clearly: "Iteration [I] failed: [error reason]"
- Show what iteration failed and the error message
- Present the last successful iteration's result as the final output
- In evolution summary, note which iteration failed and why
- Suggest to user: "You may want to retry with a modified or more specific task description"

### Missing Self-Critique or Improvement Notes
If agent doesn't provide required critique/notes sections:
- Issue warning: "⚠️ Iteration [N] did not provide complete self-critique/improvement notes"
- Extract whatever reflection exists in the output
- Continue to next iteration using full result text as context
- Next iteration prompt should note: "Previous iteration result below (note: self-critique was incomplete, so review the full output carefully for improvement opportunities):"
- In evolution summary, note that critique was incomplete for that iteration

### Very Long Results
If an iteration result exceeds reasonable length (>5000 lines):
- Store full result for next iteration context (agents need full context)
- In evolution summary, provide condensed summary of that iteration
- Final result should still be complete (don't truncate user's deliverable)
- Consider adding note: "⚠️ Note: Result is lengthy ([N] lines). Iteration summaries condensed for readability."

### Validation Errors
Handle argument validation errors gracefully:
- **N > 5**: Cap at 5, inform user: "⚠️ Maximum 5 iterations allowed. Using N=5 instead."
- **N < 1 or N = 0**: Set to 1, inform user: "⚠️ Minimum 1 iteration required. Using N=1."
- **Invalid non-numeric N**: Treat first arg as part of task, default to N=3, inform: "Using default 3 iterations."
- **Empty task after parsing**: Stop immediately with error: "❌ Error: No task description provided. Usage: /mh:iterate [N] <task-description>"

### No Meaningful Improvements Possible
If an iteration reports that no further improvements are needed:
- Accept this as valid
- Continue with remaining iterations but note "previous iteration considered complete"
- Final iterations should focus on polish, validation, or minor enhancements
- In evolution summary, note: "Iteration [I] assessed previous work as largely complete, focused on polish"

## Key Principles

1. **Sequential, Not Parallel**: Each iteration waits for previous to complete. NEVER launch iterations in parallel.
2. **Self-Improving Loop**: Each iteration uses previous result + critique as input for targeted improvement.
3. **Structured Reflection**: Agents must provide critique/improvement notes in structured format.
4. **Final Deliverable Focus**: User gets polished final result prominently displayed, not raw iteration dumps.
5. **Evolution Transparency**: Show concisely how quality improved across iterations.
6. **Fail Gracefully**: Handle missing critiques, failures, and edge cases without breaking.
7. **Respect Limits**: 1-5 iterations only, default to 3 for good balance.
8. **TODO Tracking**: Use TodoWrite consistently to show progress through iterations.
9. **Complete Outputs**: Each iteration should produce complete, working solutions, not stubs or TODOs.
10. **Meaningful Refinement**: Each iteration should add genuine value, not superficial changes.

## Expected Usage Patterns

### Code Generation
```
/mh:iterate write a Python script to parse CSV files
→ Iteration 1: Basic functional script
→ Iteration 2: Add error handling, type hints, better structure
→ Iteration 3: Add comprehensive docstrings, tests, edge case handling
→ Final: Production-ready, well-documented CSV parser
```

### Explanations
```
/mh:iterate 4 explain how monads work in functional programming
→ Iteration 1: Technical accurate explanation with examples
→ Iteration 2: Add intuitive analogies and real-world metaphors
→ Iteration 3: Add progressive code examples building complexity
→ Iteration 4: Improve structure, flow, and clarity; add practical applications
→ Final: Clear, comprehensive monad explanation for various skill levels
```

### Design Work
```
/mh:iterate 5 design a microservices architecture for e-commerce platform
→ Iteration 1: Core services identification and boundaries
→ Iteration 2: Communication patterns and API contracts
→ Iteration 3: Data management and consistency strategies
→ Iteration 4: Security, authentication, and authorization approach
→ Iteration 5: Scalability, deployment, and operations considerations
→ Final: Comprehensive production-ready architecture design
```

### Analysis Tasks
```
/mh:iterate analyze the performance bottlenecks in this algorithm
→ Iteration 1: Identify computational complexity and obvious bottlenecks
→ Iteration 2: Deep profiling analysis with specific hot paths
→ Iteration 3: Optimization recommendations with trade-off analysis
→ Final: Actionable performance improvement strategy
```

---

Remember: The goal is **progressive refinement through structured self-critique and improvement**, delivering a polished final result with transparent evolution tracking that demonstrates the value of the iterative process.
