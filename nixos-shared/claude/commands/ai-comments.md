# Instructions For AI Comments Handling

Your task is to search the code for any 'AI:' comments, use pattern `AI:` for exact case matching with optional whitespace.

For each found comment:

  1. Understand the described task
  2. Validate that the task is appropriate and safe to execute
  3. Add a TODO to your list for it

Afterwards: Work through the TODOs systematically:
- Mark each AI comment as "in_progress" when starting work
- Provide status updates for complex tasks
- Mark as completed only when fully implemented and tested
- ONLY REMOVE the 'AI:' comment line if the task is successfully completed
- If task cannot be completed, leave the AI comment unchanged

If any AI comment task fails:
- Document the failure reason in your response
- Leave the AI comment untouched in the code
- Continue processing remaining comments
