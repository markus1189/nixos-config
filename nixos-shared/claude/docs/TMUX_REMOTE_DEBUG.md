# tmux Remote Debugging Guide

## Overview

This guide documents techniques for using tmux as an interface for remote system debugging and administration. The approach involves sending commands to specific tmux panes on remote systems and capturing their output for analysis.

## Core Concepts

### tmux Session Architecture
- **Session**: Top-level container (e.g., `default`)
- **Window**: Numbered containers within sessions (e.g., `3`)
- **Pane**: Split sections within windows (e.g., `.1`, `.2`)

### Targeting Syntax
```
session:window.pane
```
Examples:
- `default:3.1` - Left pane in window 3 of default session
- `default:3.2` - Right pane in window 3 of default session

## Command Execution Patterns

### Basic Command Execution
```bash
# Send command to specific pane
tmux send-keys -t default:3.2 'command' Enter

# Capture output from pane
tmux capture-pane -t default:3.2 -p
```

### Handling Interactive Tools
```bash
# Exit pagers (less, more, journalctl output)
tmux send-keys -t default:3.2 'q' Enter

# Cancel running commands
tmux send-keys -t default:3.2 C-c

# Navigate through paged output
tmux send-keys -t default:3.2 Space    # Page down
tmux send-keys -t default:3.2 'b'      # Page up
```

## Remote Debugging Workflow

### Standard Debugging Cycle
1. **Send command** to target pane
2. **Capture output** immediately
3. **Analyze results** locally
4. **Send follow-up commands** based on findings
5. **Repeat** until issue is resolved

### Example Workflow
```bash
# 1. Check service status
tmux send-keys -t remote:1.1 'systemctl --user status service-name' Enter
tmux capture-pane -t remote:1.1 -p

# 2. Examine logs if service has issues
tmux send-keys -t remote:1.1 'journalctl --user -u service-name --since "1 hour ago"' Enter
tmux capture-pane -t remote:1.1 -p

# 3. Check configuration files
tmux send-keys -t remote:1.1 'cat ~/.config/service-name/config.conf' Enter
tmux capture-pane -t remote:1.1 -p

# 4. Make changes and restart
tmux send-keys -t remote:1.1 'systemctl --user restart service-name' Enter
tmux capture-pane -t remote:1.1 -p
```

## Command Categories and Examples

### Process Inspection
```bash
# Find running processes
tmux send-keys -t target:1.1 'ps aux | grep process-name' Enter

# Check process details
tmux send-keys -t target:1.1 'pgrep -l process-name' Enter

# Monitor process activity
tmux send-keys -t target:1.1 'top -p $(pgrep process-name)' Enter
```

### Log Analysis
```bash
# Service-specific logs
tmux send-keys -t target:1.1 'journalctl --user -u service-name --since "today"' Enter

# Real-time log monitoring
tmux send-keys -t target:1.1 'journalctl --user -f' Enter

# Filter logs
tmux send-keys -t target:1.1 'journalctl --user | grep -i error' Enter
```

### File Operations
```bash
# Read configuration files
tmux send-keys -t target:1.1 'cat ~/.config/app/config.conf' Enter

# Find configuration files
tmux send-keys -t target:1.1 'find ~/.config -name "*app*" 2>/dev/null' Enter

# Backup before editing
tmux send-keys -t target:1.1 'cp file.conf file.conf.backup' Enter
```

### Service Management
```bash
# Check service status
tmux send-keys -t target:1.1 'systemctl --user status service-name' Enter

# Restart services
tmux send-keys -t target:1.1 'systemctl --user restart service-name' Enter

# Check system inhibits
tmux send-keys -t target:1.1 'systemd-inhibit --list' Enter
```

## Best Practices

### Session Management
- Use descriptive session and window names
- Maintain consistent pane layouts
- Keep command history accessible via tmux scrollback

### Command Execution
- Always capture output immediately after sending commands
- Handle interactive tools explicitly (exit pagers, cancel long-running commands)
- Use timeouts for commands that might hang

### Error Handling
- Check command success through output analysis
- Have fallback commands ready for common failures
- Maintain ability to cancel or interrupt problematic commands

### Documentation
- Log command sequences for complex debugging sessions
- Document successful resolution patterns for future reference
- Keep track of configuration changes and their effects

## Technical Implementation Details

### Pane Addressing
- Panes are numbered starting from 0
- Common layouts: `.0` (single), `.1` (left), `.2` (right)
- Window splits create additional pane numbers

### Output Capture
- `capture-pane -p` prints to stdout immediately
- `capture-pane -S -` captures entire scrollback buffer
- Output may include terminal formatting characters

### Special Characters
- `Enter` sends carriage return
- `C-c` sends SIGINT (interrupt)
- `C-d` sends EOF
- Space and other keys send literal characters

### Limitations
- Commands requiring interactive input need special handling
- Terminal size affects output formatting
- Network latency impacts response times
- Some ncurses applications may not work properly

## Troubleshooting

### Common Issues
- **Command not executing**: Check pane targeting syntax
- **Output truncated**: Use scrollback capture options
- **Interactive tools hanging**: Send 'q' or C-c to exit
- **Pane not responding**: Verify session/window/pane exists

### Debugging Commands
```bash
# List tmux sessions
tmux list-sessions

# List windows in session
tmux list-windows -t session-name

# List panes in window
tmux list-panes -t session:window
```

This approach provides a powerful framework for remote system administration and debugging while maintaining local analysis capabilities.