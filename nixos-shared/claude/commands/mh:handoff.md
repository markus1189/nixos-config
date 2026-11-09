---
description: Write a handoff plan of this conversation to .plan/handoffs/ so a fresh session can continue via /mh:pickup
argument-hint: <purpose of the next session>
---

Creates a detailed handoff plan of the conversation for continuing the
work in a new session.

The user specified purpose:

<purpose>$ARGUMENTS</purpose>

You are creating a summary specifically so that it can be continued by
another agent. If `<purpose>` is empty, ask the user for the next
session's purpose before writing anything; the plan is built around it.

## Goal

Your task is to create a detailed summary of the conversation so far,
paying close attention to the user's explicit purpose for the next
steps.  This handoff plan should be thorough in capturing technical
details, code patterns, and architectural decisions that will be
essential for continuing development work without losing context.

## Process

Before writing, check the plan for technical accuracy and
completeness. It must include the following sections:

1. **Primary Request and Intent**: Capture all of the user's explicit
   requests and intents in detail
2. **Key Technical Concepts**: List all important technical concepts,
   technologies, and frameworks discussed.
3. **Files and Code Sections**: Enumerate specific files and code
   sections examined, modified, or created. Pay special attention to
   the most recent messages and include full code snippets where
   applicable and include a summary of why this file read or edit is
   important.
4. **Problem Solving**: Document problems solved and any ongoing
   troubleshooting efforts.
5. **Pending Tasks**: Outline any pending tasks that you have
   explicitly been asked to work on.
6. **Current Work**: Describe in detail precisely what was being
   worked on immediately before this handoff request, paying special
   attention to the most recent messages from both user and
   assistant. Include file names and code snippets where applicable.
7. **Optional Next Step**: List the next step that you will take that
   is related to the most recent work you were doing. Keep
   this step in line with the user's explicit requests and the task in
   progress immediately before this handoff request. If your last task was concluded, then only list
   next steps if they are explicitly in line with the users
   request. Do not start on tangential requests without confirming
   with the user first.

Additionally create a "slug" for this handoff.  The "slug" is how we
will refer to it later in a few places.  Examples:

* current-user-api-handler
* implement-auth
* fix-issue-42

Together with the slug create a "Readable Summary".  Examples:

* Implement Current User API Handler
* Implement Authentication
* Fix Issue #42

## Output Structure

Here's an example of how your output should be structured:

````markdown
# Readable Summary

<plan>
# Session Handoff Plan

## 1. Primary Request and Intent
[Detailed description of all user requests and intents]

## 2. Key Technical Concepts
- [Concept 1]
- [Concept 2]
- [...]

## 3. Files and Code Sections
### [File Name 1]
- **Why important**: [Summary of why this file is important]
- **Changes made**: [Summary of the changes made to this file, if any]
- **Code snippet**:
```language
[Important Code Snippet]
```

### [File Name 2]
- **Code snippet**:
```language
[Important Code Snippet]
```

[...]

## 4. Problem Solving
[Description of solved problems and ongoing troubleshooting]

## 5. Pending Tasks
- [Task explicitly requested but not yet done]

## 6. Current Work
[Precisely what was being worked on right before this handoff, with
file names and code snippets]

## 7. Optional Next Step
[Next step, directly aligned with user's explicit handoff purpose]
</plan>
````

## Final Step

Write the handoff to a
markdown file at `.plan/handoffs/[timestamp]-[slug].md` where
[timestamp] is the current local time as YYYY-MM-DD-HHMM (`date
+%F-%H%M`) and the slug is what we defined before.  The file contains
only the `# Readable Summary` title line followed by the content inside
`<plan>...</plan>` (without the tags).

Then tell the user about this file (with an absolute path) and that
they can use `/mh:pickup FILENAME` to continue.
