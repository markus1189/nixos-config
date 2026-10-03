---
description: Resume work from a handoff file in .plan/handoffs/ (written by /mh:handoff); lists them if none given
argument-hint: [handoff-filename]
---

Resumes work from a previous handoff session which are stored in
`.plan/handoffs`.

The handoff folder might not exist if there are none.

Requested handoff file: `$ARGUMENTS`

## Process

### 1. Check handoff file

If no handoff file was provided, list them all.  Eg:


```
echo "## Available Handoffs"
echo ""
for file in .plan/handoffs/*.md; do
  if [ -f "$file" ]; then
    title=$(grep -m 1 "^# " "$file" | sed 's/^# //')
    basename=$(basename "$file")
    echo "* \`$basename\`: $title"
  fi
done
echo ""
echo "To pickup a handoff, use: /mh:pickup <filename>"
```

### 2. Read handoff file

If a handoff file was provided locate it in `.plan/handoffs` and read
it.  Note that this file might be misspelled or the user might have
only partially listed it.  If there are multiple matches, ask the user
which one they want to continue with.  The file contains the
instructions for how you should continue.

### 3. Verify against current state

Before acting, compare the handoff's stated branch, files and changes
with `git status`, `git branch --show-current` and `git log -1`.  Report
any drift (other branch, files already changed or missing, new commits)
and ask before continuing if it affects the next step.
