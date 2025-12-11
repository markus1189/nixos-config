---
description: Race multiple agents competitively on a task with complementary perspectives
argument-hint: [N] [task-verb] <target>
---

# Agent Racing Competition

Launch multiple agents (2-4+) in parallel to compete on the same task
with complementary perspectives. The competitive framework encourages
thoroughness; complementary emphases maximize coverage; synthesis
delivers a unified superior report.

## Argument Parsing

Extract task details from arguments with optional agent count
override:

### Syntax Options
```
/mh:agent-race [task-verb] <target>           # Claude decides agent count (2-4, soft limit)
/mh:agent-race [N] [task-verb] <target>       # Explicit count override
```

### Parsing Logic
1. **First argument**: Check if it's a number → agent count override
2. **Next argument (optional)**: Task verb (review, analyze,
   investigate, compare, audit, evaluate, etc.)
3. **Remaining arguments**: Target/subject of the task
4. **If no verb provided**: Default to "review" or intelligently infer
   from context

### Agent Count Guidelines
- **Default**: Claude intelligently chooses 2-4 agents
- **Soft limit**: 4 agents covers most scenarios
- **Exceeding limit**: If Claude thinks >4 agents would add value, ASK
  user for confirmation first
- **User override**: If user specifies >4 explicitly, respect it (they
  know what they want)

### Examples
- `/mh:agent-race review src/auth/handler.go` → auto-decide count
  (2-4), verb="review"
- `/mh:agent-race 3 review src/auth/handler.go` → force 3 agents,
  verb="review"
- `/mh:agent-race 5 analyze massive-refactor` → user wants 5 agents,
  allow it
- `/mh:agent-race src/api/users.ts` → auto-decide count, infer verb
  (likely "review")

## Determining Agent Count

If agent count not explicitly provided, analyze the task and decide
2-4 agents:

### Decision Framework

**2 Agents** (default for most tasks):
- Standard code reviews
- Simple analysis tasks
- Small scope investigations
- General comparisons

**3 Agents** (when additional perspective adds value):
- Security-critical code (add security specialist)
- API design reviews (add integration/contract perspective)
- Performance-critical systems (add deep performance specialist)
- Complex business logic (add domain modeling perspective)
- Testing strategy (add test/quality perspective)

**4 Agents** (for highly complex, multi-dimensional tasks):
- Distributed systems architecture (add scalability/operations)
- Critical infrastructure (add security + operations + reliability)
- Large-scale refactoring (add migration strategy specialist)
- Multi-concern systems (e.g., auth + perf + scale + ux all critical)

### Guidelines
- **Don't over-agent**: Only add agents when their perspective
  meaningfully increases coverage
- **Overlap is good**: All agents remain comprehensive, just different
  emphases
- **Soft limit at 4**: Most tasks don't need more than 4 complementary
  perspectives
- **Exceeding soft limit**: If you think >4 agents would add significant value, use AskUserQuestion tool to confirm:
  ```
  "This task seems highly complex and multi-dimensional. I recommend [N] agents
   to cover: [list perspectives]. This exceeds the typical 4-agent limit. Proceed?"
  ```
- **User override respected**: If user explicitly specifies count
  (even >4), use it
- **Diminishing returns**: Each additional agent should add
  meaningfully distinct value

## Task Execution

Launch N agents IN PARALLEL using a single message with N Task tool
calls (typically N=2-4, can exceed with confirmation):

```
Task 1 (Agent 1): general-purpose subagent [always]
Task 2 (Agent 2): general-purpose subagent [always]
Task 3 (Agent 3): general-purpose subagent [if N >= 3]
Task 4 (Agent 4): general-purpose subagent [if N >= 4]
Task 5+ (Agent 5+): general-purpose subagent [if N > 4, with user confirmation]
```

All agents receive competitive framing with **complementary
perspectives** to maximize coverage while maintaining overlap.

### Core Agent Perspectives (Agents 1-2)

These two agents form the foundation for all tasks:

### Agent 1 Instructions Template (Correctness & Maintainer Lens)

```
You are Agent 1 competing with another agent for a performance prize based on quality and thoroughness.

Your task: [TASK_VERB] [TARGET]

## Competition Context
- You are racing against a peer agent performing the same analysis with a different emphasis
- Your work will be directly compared to theirs
- The winner is determined by: depth of analysis, actionable insights, thoroughness, and clarity
- Quality and completeness matter more than speed

## Your Analytical Lens
While remaining comprehensive, emphasize:
- **Correctness & Security**: Edge cases, error handling, vulnerabilities, data integrity
- **Maintainer Perspective**: Long-term maintainability, code clarity, technical debt
- **Conservative Approach**: Safe, proven patterns; careful refactoring suggestions
- **Depth in Critical Paths**: Deep dive into business-critical code paths

Still cover all aspects (performance, usability, etc.) but let these emphases guide your attention.

## Your Mission
[TASK_VERB] the [TARGET] with exceptional thoroughness. Consider:
- Edge cases and error conditions (emphasis)
- Security vulnerabilities and data integrity (emphasis)
- Maintainability and technical debt (emphasis)
- Performance, usability, and best practices
- Code quality and architectural patterns

## Required Output Structure

### Analysis/Review/Investigation
[Your detailed work here - be comprehensive]

### Key Findings
[Summarize critical discoveries]

### Recommended Follow-up Actions
**REQUIRED**: Conclude with specific, actionable follow-up steps based on your findings.
- Prioritize recommendations (Critical, Important, Nice-to-have)
- Be specific (not vague suggestions)
- Include what to do and why
- If no issues found, suggest validation or monitoring approaches

Remember: You're competing for quality. Be thorough, insightful, and actionable.
```

### Agent 2 Instructions Template (Performance & User Lens)

```
You are Agent 2 competing with another agent for a performance prize based on quality and thoroughness.

Your task: [TASK_VERB] [TARGET]

## Competition Context
- You are racing against a peer agent performing the same analysis with a different emphasis
- Your work will be directly compared to theirs
- The winner is determined by: depth of analysis, actionable insights, thoroughness, and clarity
- Quality and completeness matter more than speed

## Your Analytical Lens
While remaining comprehensive, emphasize:
- **Performance & Efficiency**: Speed, resource usage, scalability, optimization opportunities
- **User/Consumer Perspective**: API usability, developer experience, clarity of interfaces
- **Bold Improvements**: Aggressive refactoring, modern patterns, architectural evolution
- **Breadth of Coverage**: Broad surface scan catching issues across the codebase

Still cover all aspects (security, correctness, etc.) but let these emphases guide your attention.

## Your Mission
[TASK_VERB] the [TARGET] with exceptional thoroughness. Consider:
- Performance bottlenecks and optimization opportunities (emphasis)
- API usability and developer experience (emphasis)
- Modern patterns and bold refactoring ideas (emphasis)
- Security, correctness, and edge cases
- Code quality and best practices

## Required Output Structure

### Analysis/Review/Investigation
[Your detailed work here - be comprehensive]

### Key Findings
[Summarize critical discoveries]

### Recommended Follow-up Actions
**REQUIRED**: Conclude with specific, actionable follow-up steps based on your findings.
- Prioritize recommendations (Critical, Important, Nice-to-have)
- Be specific (not vague suggestions)
- Include what to do and why
- If no issues found, suggest validation or monitoring approaches

Remember: You're competing for quality. Be thorough, insightful, and actionable.
```

### Agent 3 Instructions Template (Task-Adaptive Specialist)

Agent 3's perspective is **task-adaptive**. Choose based on what adds
most value:

**For security-critical tasks** (auth, crypto, permissions):
```
You are Agent 3 competing for a performance prize.
Your task: [TASK_VERB] [TARGET]

## Your Analytical Lens (Security Specialist)
While remaining comprehensive, emphasize:
- **Security Deep Dive**: Vulnerabilities, attack vectors, threat modeling
- **Adversarial Thinking**: How could this be exploited? What's the worst case?
- **Defense in Depth**: Multiple layers of protection, fail-secure defaults
- **Security Best Practices**: OWASP, CWE, industry standards

[Standard mission and output structure...]
```

**For performance-critical tasks** (hot paths, large-scale, latency-sensitive):
```
Your Analytical Lens (Performance Specialist)
- **Performance Deep Dive**: Profiling, bottlenecks, algorithmic complexity
- **Scalability**: How does this behave under load? What breaks first?
- **Resource Efficiency**: Memory, CPU, I/O, network optimization
- **Measurement**: What metrics matter? How to monitor?
```

**For API/integration tasks**:
```
Your Analytical Lens (Integration Specialist)
- **Contract Design**: API clarity, versioning, backward compatibility
- **Integration Patterns**: How do consumers use this? Common pitfalls?
- **Documentation**: Is the API self-explanatory? What's confusing?
- **Error Handling**: Clear error messages, failure modes, debugging support
```

**For testing/quality tasks**:
```
Your Analytical Lens (Quality/Testing Specialist)
- **Testability**: How easy is this to test? What's hard to verify?
- **Test Coverage**: What's missing? Edge cases? Integration scenarios?
- **Quality Attributes**: Reliability, robustness, fault tolerance
- **Testing Strategy**: Unit, integration, E2E - what's appropriate?
```

**For architectural/design tasks**:
```
Your Analytical Lens (Architecture Specialist)
- **Design Patterns**: Are patterns appropriate? Over/under-engineered?
- **Modularity**: Coupling, cohesion, separation of concerns
- **Extensibility**: How easy to modify/extend? Future-proofing?
- **Trade-offs**: What design decisions were made? Are they right?
```

### Agent 4 Instructions Template (Task-Adaptive Specialist)

Agent 4's perspective is **task-adaptive**. Choose based on what adds
most value:

**For distributed/scalability tasks**:
```
Your Analytical Lens (Scalability/Operations Specialist)
- **Distributed Systems**: Consistency, availability, partition tolerance
- **Operational Concerns**: Deployment, monitoring, debugging in production
- **Failure Modes**: What can go wrong? Cascading failures? Circuit breakers?
- **Scalability**: Horizontal/vertical scaling, bottlenecks, capacity planning
```

**For reliability/resilience tasks**:
```
Your Analytical Lens (Reliability Specialist)
- **Fault Tolerance**: Graceful degradation, retry logic, timeout handling
- **Observability**: Logging, metrics, tracing - can we debug issues?
- **Incident Response**: How do we detect and recover from failures?
- **SLOs/SLAs**: What guarantees do we need? How do we measure?
```

**For data/state management tasks**:
```
Your Analytical Lens (Data/State Specialist)
- **Data Consistency**: Race conditions, transactions, eventual consistency
- **State Management**: Where is state? How does it flow? Synchronization?
- **Data Integrity**: Validation, constraints, corruption prevention
- **Migration**: How does data evolve? Schema changes? Backward compatibility?
```

**For user experience tasks**:
```
Your Analytical Lens (UX/Developer Experience Specialist)
- **Usability**: Is this intuitive? What's confusing? Common mistakes?
- **Feedback**: Error messages, progress indicators, debugging info
- **Ergonomics**: API design, naming, discoverability
- **Documentation**: Examples, common patterns, gotchas
```

### Agent 5+ Instructions (If Exceeding Soft Limit)

For tasks requiring >4 agents (with user confirmation), define
additional task-adaptive perspectives:

**Guidelines for Agent 5+**:
- Only create if perspective adds **meaningful distinct value**
- Avoid redundancy with Agents 1-4
- Pick from specialized domains: Compliance, Migration Strategy,
  Backwards Compatibility, Internationalization, Accessibility, Cost
  Optimization, etc.
- Ensure competitive framing and all standard requirements (findings,
  follow-ups)

**Example for Agent 5** (Migration/Compatibility Specialist):
```
Your Analytical Lens (Migration/Compatibility Specialist)
- **Migration Path**: How do we get from current state to target state?
- **Backwards Compatibility**: What breaks? How to maintain compatibility?
- **Rollout Strategy**: Phased rollout? Feature flags? A/B testing?
- **Rollback Plan**: How do we revert if issues arise?
```

**Example for Agent 6** (Compliance/Governance Specialist):
```
Your Analytical Lens (Compliance/Governance Specialist)
- **Regulatory Compliance**: GDPR, SOC2, HIPAA, industry standards
- **Audit Trail**: Logging, accountability, change tracking
- **Policy Enforcement**: Access control, data handling, retention
- **Documentation Requirements**: Compliance documentation, evidence
```

## Synthesis and Unified Report

After all N agents complete (typically N=2-4, can be more), synthesize
their findings into a SINGLE comprehensive report that is better than
any individual analysis.

**DO NOT present individual agent outputs separately.**  **DO NOT
compare agent performance or rate their work.**

### Synthesis Process

1. **Identify Consensus Findings**
   - What did MULTIPLE agents discover despite different emphases?
   - High consensus (3-4 agents found it) = extremely high confidence,
     most critical
   - Medium consensus (2 agents found it) = high confidence, important
   - Consensus findings are typically the most critical
     issues/insights

2. **Capture Critical Unique Findings**
   - What did only ONE agent find that is significant?
   - Agent 1 might catch: Security edge cases, subtle bugs,
     maintainability issues
   - Agent 2 might catch: Performance bottlenecks, UX problems, bold
     refactoring opportunities
   - Agent 3 might catch: Task-specific deep insights (security vulns,
     perf issues, API problems)
   - Agent 4 might catch: Operational concerns, scalability issues,
     reliability gaps
   - Don't include every minor difference - focus on important
     insights
   - Complementary perspectives maximize different classes of issues
     discovered

3. **Organize by Importance, Not by Agent**
   - Structure findings by criticality and theme
   - Consensus findings first (highest priority)
   - Then critical unique findings
   - Synthesize complementary insights into unified narrative

3. **Create Unified Report**

   Structure:
   ```
   # [Task Verb] Results: [Target]

   ## Summary
   [High-level overview of findings - 2-3 sentences]

   ## Critical Findings
   [Consensus findings - what both agents identified]
   [Important unique findings - significant discoveries from either agent]

   ## Detailed Analysis
   [Synthesized deep dive combining insights from both analyses]
   [Organize by theme/area, not by agent]

   ## Recommended Follow-up Actions
   [Unified prioritized action items incorporating both perspectives]

   **Critical**:
   - [Action 1 with rationale]
   - [Action 2 with rationale]

   **Important**:
   - [Action 3 with rationale]

   **Consider**:
   - [Action 4 with rationale]
   ```

### Synthesis Guidelines

- **Focus on substance**: What was found, not who found it
- **Elevate consensus**: Findings from both agents are high-priority
- **Include critical uniques**: Don't lose important insights unique
  to one agent
- **Unified voice**: Write as a single cohesive analysis, not "Agent 1
  said... Agent 2 said..."
- **Actionable output**: Single clear set of next steps

## Task Verb Guidance

Adapt the agent instructions based on the task verb:

- **review**: Focus on code quality, bugs, improvements, best
  practices
- **analyze**: Deep investigation of behavior, patterns, architecture
- **investigate**: Root cause analysis, debugging, tracing issues
- **compare**: Side-by-side evaluation highlighting differences and
  trade-offs
- **audit**: Systematic examination for compliance, security, quality
  standards
- **evaluate**: Assessment of fitness for purpose, performance, design
  decisions
- **refactor**: Identify refactoring opportunities and provide
  concrete suggestions

## Key Principles

1. **Parallel Execution**: Launch all N agents in a SINGLE message
   with N Task tool calls
2. **Dynamic Agent Count**: Claude intelligently chooses 2-4 agents
   (soft limit at 4)
   - Default: 2 agents for most tasks
   - 3 agents: When additional specialist perspective adds value
   - 4 agents: Complex multi-dimensional tasks
   - 5+ agents: With user confirmation via AskUserQuestion tool
3. **Complementary Perspectives**: Agents get different emphases to
   maximize coverage
   - Agent 1: Correctness/Security focus + Maintainer lens +
     Conservative + Depth
   - Agent 2: Performance/Usability focus + User lens + Bold ideas +
     Breadth
   - Agent 3+: Task-adaptive specialists (security, performance,
     architecture, ops, etc.)
4. **Overlap by Design**: All agents remain comprehensive, emphases
   are guidance not constraints
5. **Competitive Motivation**: Explicitly tell agents they're
   competing for quality (drives thoroughness)
6. **Required Follow-ups**: Agents MUST conclude with actionable next
   steps
7. **Synthesize, Don't Compare**: Combine findings into one superior
   report, don't rate agents
8. **Consensus = High Confidence**: What multiple agents found is
   likely most important
9. **Complementary Uniques = Coverage**: Different emphases catch
   different classes of issues

## Example Constructions

### Example 1: Code Review
```
Arguments: review src/auth/middleware.ts

Agent 1 instructions (Correctness/Maintainer lens):
"You are Agent 1 competing for a performance prize.
Your task: review src/auth/middleware.ts

Your Analytical Lens:
- Emphasize: Security vulnerabilities, edge cases, error handling
- Maintainer perspective: Is this easy to maintain long-term?
- Conservative approach: Safe, proven auth patterns
- Deep dive into critical authentication paths

Review the authentication middleware with exceptional thoroughness:
- Security vulnerabilities and data integrity (emphasis)
- Edge cases and error conditions (emphasis)
- Long-term maintainability (emphasis)
- Performance, usability, code quality
[Required output structure...]"

Agent 2 instructions (Performance/User lens):
"You are Agent 2 competing for a performance prize.
Your task: review src/auth/middleware.ts

Your Analytical Lens:
- Emphasize: Performance, API usability, scalability
- User/consumer perspective: Is this API clear and easy to use?
- Bold improvements: Modern auth patterns, aggressive optimizations
- Broad scan across all middleware interactions

Review the authentication middleware with exceptional thoroughness:
- Performance and scalability characteristics (emphasis)
- API clarity and developer experience (emphasis)
- Modern patterns and refactoring opportunities (emphasis)
- Security, edge cases, code quality
[Required output structure...]"

Expected synthesis:
- Consensus: Both might find missing rate limiting → HIGH PRIORITY
- Agent 1 unique: Catches subtle timing attack vulnerability
- Agent 2 unique: Identifies N+1 query issue in auth lookup
- Combined report: Superior coverage of security + performance + usability
```

### Example 2: Architecture Analysis
``` Arguments: analyze api-gateway-pattern

Agent instructions:
"You are Agent [1/2] competing for a performance prize.

Your task: analyze api-gateway-pattern

[Competition context...]

Analyze the API gateway pattern implementation:
- Architectural strengths and weaknesses
- Scalability considerations
- Failure modes and resilience
- Performance characteristics

[Required output structure with follow-ups...]"
```

### Example 3: Comparison Task
```
Arguments: compare graphql vs rest-api

Agent instructions:
"You are Agent [1/2] competing for a performance prize.

Your task: compare graphql vs rest-api

[Competition context...]

Compare GraphQL and REST API approaches in this codebase:
- Implementation complexity
- Performance trade-offs
- Developer experience
- Maintainability

[Required output structure with follow-ups...]"
```

---

**Remember**: The goal is to leverage competition to get N
high-quality, independent analyses with complementary perspectives,
then synthesize them into a single superior report. Competition drives
thoroughness; complementary perspectives maximize coverage; synthesis
delivers actionable insights. Focus on substance (what was found), not
performance (who found it). Default to 2 agents, scale to 3-4 when it
adds value, and confirm with user if >4 agents would be beneficial.
