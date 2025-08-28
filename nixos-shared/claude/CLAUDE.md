# Claude Code Statusline Configuration

This directory contains the Claude Code statusline script and related configuration.

## Files

- `claude-code-statusline.sh` - Main statusline script that displays session information
- `CLAUDE.md` - This documentation file

## Statusline Script Development

### Color Consistency

The statusline uses a standardized color palette defined as RGB values:

```bash
readonly RED="255;120;120"      # Model name
readonly ORANGE="255;180;100"   # Version
readonly GREEN="120;220;120"    # Git branch/status  
readonly BLUE="100;180;255"     # Project directory
readonly PURPLE="180;140;255"   # Cost
readonly CYAN="100;200;200"     # Agent metrics
readonly PINK="255;140;180"     # Transcript ID
```

**Context segment uses dynamic colors based on usage:**
- Green: Low usage (<100k tokens)
- Orange: Medium usage (100k-150k tokens)  
- Red: High usage (>150k tokens)

### Code Standards

**Always run shellcheck after making changes:**
```bash
shellcheck nixos-shared/claude/claude-code-statusline.sh
```

**Key requirements:**
- Use `printf` instead of `echo` for escape sequences
- Quote all variable expansions
- Use `readonly` for constants
- Follow existing function naming patterns

### Segment Structure

Each statusline segment follows this pattern:
```bash
echo -en "$(segment "$COLOR" "content")"
echo -en "$(separator "$COLOR1" "$COLOR2")"
```

**Current segment order:**
1. Model name (red)
2. Version (orange)
3. Git branch/status (green)
4. Project directory (blue)
5. Cost (purple)
6. Context with usage bar (dynamic color)
7. Agent metrics (cyan)
8. Transcript ID (pink)

### Token Formatting

Use consistent "kt" (kilotokens) suffix for large token counts:
- `<1000`: Display raw number (e.g., "500")
- `≥1000`: Display with kt suffix (e.g., "62.5kt")

### Adding New Metrics

When adding new metrics:
1. Create a `get_metric_name()` function
2. Add color constant if needed
3. Insert segment in logical position
4. Update separators for adjacent segments
5. Run shellcheck to verify
6. Test with sample input
7. Document the metric in this file

### Testing

Test the statusline script:
```bash
cat /tmp/input.json | ./nixos-shared/claude/claude-code-statusline.sh
```

The script expects JSON input with fields like `model.display_name`, `version`, `transcript_path`, etc.