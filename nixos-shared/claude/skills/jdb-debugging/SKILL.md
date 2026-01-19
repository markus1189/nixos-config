---
name: jdb-debugging
description: "Debug local JVM applications using jdb via tmux. Use when user asks to debug a running Java/Scala/Kotlin application, attach to a debug port, set breakpoints, step through code, or inspect variables."
---

# JDB Debugging via tmux

Debug running JVM applications using jdb controlled through tmux panes.

## Setup

**With dedicated pane (preferred):** User provides pane ID (e.g., `%265`)
```bash
tmux send-keys -t %265 "jdb -attach PORT" Enter
sleep 2
tmux capture-pane -t %265 -p
```

**With new session:**
```bash
tmux new-session -d -s jdb "jdb -attach PORT"
sleep 2
tmux capture-pane -t jdb -p
```

## Commands

All commands follow this pattern:
```bash
tmux send-keys -t PANE "COMMAND" Enter
sleep 0.5
tmux capture-pane -t PANE -p
```

### Breakpoints

```bash
# Simple method (non-overloaded)
stop in com.example.MyClass.myMethod

# Overloaded method - first list methods to find signature
methods com.example.MyClass
# Then use exact signature
stop in com.example.MyClass.execute(java.lang.Object)

# Line breakpoint (often fails with Scala due to line mismatch)
stop at com.example.MyClass:42
```

### Navigation

| Command | Action |
|---------|--------|
| `cont` | Continue execution |
| `step` | Step into |
| `next` | Step over |
| `step up` | Step out of current method |

### Inspection

| Command | Action |
|---------|--------|
| `where` | Stack trace |
| `locals` | Local variables |
| `print varName` | Print variable value |
| `print obj.field` | Print object field |
| `threads` | List all threads |
| `thread THREAD_ID` | Switch to thread |

### Breakpoint Management

| Command | Action |
|---------|--------|
| `clear` | List breakpoints |
| `clear com.example.MyClass.myMethod` | Remove breakpoint |

## Scala/Kotlin Notes

- Line breakpoints often fail - use method breakpoints instead
- For overloaded methods, use `methods ClassName` to find exact JVM signature
- `list` command typically fails (source not found) but inspection commands work fine

## Cleanup

```bash
# Session approach
tmux kill-session -t jdb

# Pane approach - just exit jdb
tmux send-keys -t %265 "quit" Enter
```
