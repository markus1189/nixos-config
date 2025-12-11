---
description: Capture and analyze tmux pane output for errors, progress, and status
argument-hint: <pane-number>
---

# tmux Pane Analysis

## Task
Capture output from specified tmux pane and analyze for:
- Error patterns (exit codes, exceptions, stack traces, build failures)
- Progress indicators (percentages, X/Y counts, ETAs)
- Completion status (process finished, still running, waiting)
- Recent activity patterns

## Steps
1. **Capture pane output**: `tmux capture-pane -p -t %$ARGUMENTS -S -100`
   - Start with last 100 lines
   - If analysis is inconclusive or output is truncated, capture more (200, 500, full buffer)

2. **Analyze captured output**:
   - **Errors**: Search for error/exception/failed/failure patterns
   - **Progress**: Look for completion percentages, counts, time estimates
   - **Status**: Determine if process is active, completed, or stuck
   - **Context**: Identify what's running (build tool, server, shell command)

3. **Present findings**:
   - Current status (running/completed/failed)
   - Key errors or warnings found
   - Progress indicators if available
   - Recommended next actions

## Error Pattern Examples
- Exit codes: `exit status 1`, `returned 127`
- Java/Gradle: `FAILED`, `BUILD FAILED`, stack traces
- npm/Vite: `ERROR`, `Failed to compile`
- MongoDB: `MongoServerError`, `E11000`
- General: `exception`, `fatal`, `panic`

## Progress Pattern Examples
- Percentages: `45%`, `[======>   ] 67%`
- Counts: `15/42`, `3 of 10 tests`
- Time: `ETA: 2m 30s`, `~5 minutes remaining`
- Build tools: `Task X of Y`, `Building N/M`

## Adaptive Capture Strategy
- **Start**: 100 lines (fast, covers most cases)
- **If insufficient**: Automatically capture more (200, 500, or full buffer with `-S -`)
- **Indicators for more lines**:
  - Output starts mid-sentence/mid-error
  - Stack trace is truncated
  - Can't find start of error or progress indicator
  - Need to see full build log

## Notes
- Pane numbers are tmux's internal IDs (visible in status line)
- Use `tmux list-panes -a` to see all panes across sessions
- If pane doesn't exist, tmux will return an error
