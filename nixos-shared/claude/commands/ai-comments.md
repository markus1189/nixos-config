# AI Comments Processing Instructions

## Overview
Process inline `AI:` comments found in code by executing the described tasks while maintaining code safety and quality.

## Search Pattern
- Use exact pattern: `AI:` (case-sensitive)
- Match with optional whitespace: `\s*AI:\s*`
- Search all file types in the codebase

## Processing Workflow

### 1. Discovery Phase
1. Search entire codebase for `AI:` comments
2. Catalog all found comments with file locations
3. Parse and understand each task description
4. Create comprehensive TODO list from all comments

### 2. Validation Phase
For each AI comment task, validate:
- **Safety**: No malicious code generation or security vulnerabilities
- **Scope**: Task is reasonable and within project boundaries  
- **Clarity**: Task description is unambiguous and actionable
- **Dependencies**: Required tools/libraries are available or appropriate

### 3. Execution Phase
Work through TODOs systematically:
- Mark task as `in_progress` when starting
- Provide progress updates for complex implementations
- Follow existing code patterns and conventions
- Test implementation thoroughly
- Mark as `completed` only when fully working

### 4. Cleanup Phase
- **SUCCESS**: Remove entire `AI:` comment line when task completed successfully
- **FAILURE**: Leave AI comment unchanged and document failure reason
- Continue processing remaining comments regardless of individual failures

## Safety Guidelines

### Prohibited Tasks
- Code that could be used maliciously
- Security vulnerabilities or exploits
- Unauthorized network access or data exfiltration
- System-level modifications without clear justification

### Required Validation
- Verify task legitimacy before implementation
- Ensure changes follow defensive security practices
- Validate against existing codebase patterns
- Confirm no sensitive data exposure

## Error Handling

### Task Failures
- Document specific failure reason
- Preserve original AI comment in code
- Continue with remaining tasks
- Provide summary of successes/failures

### Edge Cases
- Malformed AI comments: Skip with warning
- Conflicting tasks: Resolve or defer to user
- Missing dependencies: Document requirements
- Ambiguous instructions: Request clarification

## Quality Standards
- Follow existing code style and conventions
- Maintain immutable coding patterns where possible
- Separate pure logic from side effects
- Include appropriate error handling
- Ensure code is testable and maintainable
