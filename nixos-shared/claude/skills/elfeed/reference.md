# Elfeed Advanced Reference

Advanced patterns, troubleshooting, and optimization for elfeed queries.

## Table of Contents

- Advanced Multi-Tag Filtering Patterns
- Complex Regex Patterns for Text Matching
- Performance Optimization
- Troubleshooting Connection and Query Issues
- Integration with External Tools
- Regular Expression Reference

## Advanced Multi-Tag Filtering

### Complex Tag Combinations

**Find entries with multiple required tags:**
```bash
"+ai +llm +unread"          # Must have all three tags
"+programming +security -reddit -youtube"  # Include/exclude mix
```

**Available tags by category:**
- Content type: `reddit`, `youtube`, `github`, `newsletter`
- Topics: `programming`, `ai`, `llm`, `security`, `hacking`, `nixos`, `emacs`
- Categories: `sport`, `analog`, `electronics`, `reading`, `news`
- Special: `unread`, `starred`

### Time-Based Workflows

**Daily digest:**
```bash
"+unread @1-day-ago"
```

**Weekly review by category:**
```bash
"+programming @1-week-ago #50"
"+ai +llm @1-week-ago #30"
```

**Monthly archive search:**
```bash
"-unread @1-month-ago topic-keyword"  # Already read items
```

**Time expression variants:**
- `@1-day-ago` or `@1-days-ago`
- `@2-weeks-ago` or `@2-week-ago`
- `@3-months-ago` or `@3-month-ago`
- `@1-year-ago` or `@1-years-ago`

**Note:** Elfeed doesn't support "older than" directly. Fetch all and filter programmatically if needed.

### Feed-Specific Advanced Queries

**Single source deep dive:**
```bash
"=simonwillison @1-month-ago"
```

**Exclude aggregators, blogs only:**
```bash
"-reddit -youtube -github +programming"
```

**Compare coverage across sources:**
```bash
# Run separately to compare
"=source1 ai.*agents"
"=source2 ai.*agents"
```

## Complex Regex Patterns

### Regex Alternation

```bash
"(nixos|nix-darwin|home-manager)"      # Match any of these
"(claude|anthropic|sonnet|opus)"       # AI-related terms
```

### Case-Insensitive Matching

```bash
"[Cc]laude|CLAUDE"                     # Case variations
"[Nn]ix[Oo][Ss]"                       # NixOS/nixos/NIXOS
```

### Word Boundaries

```bash
"\bAI\b"          # Matches "AI" but not "AIr" or "mAIn"
"\bnix\b"         # Matches "nix" but not "nixos" or "unix"
```

### Negative Lookahead Patterns

```bash
"!advertisement"                       # Exclude this exact term
"!spam !promoted !sponsored"           # Multiple exclusions
```

### Feed URL/Title Matching

```bash
"=simonwillison"                       # Specific feed
"=anthropic|openai"                    # Multiple feeds (regex OR)
"~reddit"                              # Exclude all Reddit feeds
"~youtube.*shorts"                     # Exclude YouTube Shorts
```

## Performance Optimization

### Large Result Sets

**Problem:** Query takes too long or returns thousands of results.

**Solution:** Add constraints progressively:
```bash
# Slow: entire database
"programming"

# Better: time constraint
"programming @1-month-ago"

# Best: time + limit + tag filtering
"+programming +unread @1-week-ago #50"
```

### Optimization Strategies

1. **Always use time constraints** for performance:
   - Default to `@1-week-ago` for recent queries
   - Use `@1-month-ago` for comprehensive searches

2. **Limit results explicitly:**
   - Use `#20` for conversational queries
   - Use `#50` for comprehensive reviews
   - Avoid unlimited results on large databases

3. **Filter by tag before text matching:**
   ```bash
   # Slow: searches all entries
   "claude ai"

   # Fast: narrows scope first
   "+programming +ai claude"
   ```

4. **Use feed filtering for source-specific queries:**
   ```bash
   # Instead of searching everything
   "llm tutorial"

   # Search specific high-quality sources
   "=simonwillison llm"
   ```

## Troubleshooting

### No Results Returned

**Symptoms:** Query completes but returns empty results.

**Debug checklist:**
1. **Is Emacs running?** → `pgrep emacs` or start with `emacs --daemon`
2. **Is elfeed loaded?** → Should auto-load, check Emacs config
3. **Filters too restrictive?** → Simplify to one condition: `elfeed-cli query "+unread"`
4. **Database updated recently?** → Check `elfeed-cli stats` for `:last-update`

**Example debugging progression:**
```bash
# Start simple
elfeed-cli query "+unread"

# Add one filter at a time
elfeed-cli query "+unread +programming"
elfeed-cli query "+unread +programming @1-week-ago"
```

### Connection Failures

**Error:** `Cannot connect to Emacs` or `emacsclient: can't find socket`

**Solutions:**
1. Start Emacs daemon: `emacs --daemon`
2. Check if running: `emacsclient --eval "(+ 1 1)"` should return `2`
3. Verify `EMACS_SERVER_FILE` environment variable if using custom socket

### Incorrect or Unexpected Results

**Debug steps:**

1. **Check available tags:**
   ```bash
   elfeed-cli tags
   ```

2. **Verify feed sources:**
   ```bash
   elfeed-cli feeds | grep -i "keyword"
   ```

3. **Test regex separately:**
   ```bash
   # Does your regex work?
   echo "test string" | grep -E "your.*regex"
   ```

4. **Simplify filter progressively:**
   ```bash
   # Remove constraints one at a time
   "+ai +llm +programming @1-week-ago"
   "+ai +llm +programming"
   "+ai +llm"
   "+ai"
   ```

## Integration with External Tools

### JSON Processing with jq

**Filter by specific tags:**
```bash
elfeed-cli query "+unread @1-week-ago" json 100 | \
  jq '.[] | select(.tags | contains(["ai"]))'
```

**Extract just titles and links:**
```bash
elfeed-cli query "+programming" json 50 | \
  jq -r '.[] | "\(.title) - \(.link)"'
```

**Group by feed:**
```bash
elfeed-cli query "+unread" json | \
  jq 'group_by(.feed_title) | map({feed: .[0].feed_title, count: length})'
```

### Export for Analysis

**Generate reading list:**
```bash
elfeed-cli query "+starred" simple > reading-list.txt
```

**Export monthly data:**
```bash
elfeed-cli query "@1-month-ago" json > analysis-$(date +%Y-%m).json
```

**Create custom digest:**
```bash
{
  echo "# Daily Digest $(date)"
  echo ""
  echo "## Programming"
  elfeed-cli query "+programming +unread @1-day-ago" simple 10
  echo ""
  echo "## AI/LLM"
  elfeed-cli query "+ai +unread @1-day-ago" simple 10
} > daily-digest.md
```

### Pipe to Other Processing

**Count by feed:**
```bash
elfeed-cli query "+unread" simple | \
  cut -d'|' -f2 | \
  sort | uniq -c | sort -rn
```

**Find longest articles:**
```bash
elfeed-cli query "+unread" json | \
  jq -r '.[] | "\(.title | length) | \(.title)"' | \
  sort -rn | head -20
```

## Regular Expression Reference

Elfeed uses standard regex syntax (Emacs regex engine):

| Pattern | Meaning | Example |
|---------|---------|---------|
| `.` | Any character | `a.c` matches "abc", "a1c" |
| `*` | Zero or more | `ab*c` matches "ac", "abc", "abbc" |
| `+` | One or more | `ab+c` matches "abc", "abbc" (not "ac") |
| `?` | Zero or one | `ab?c` matches "ac", "abc" |
| `^` | Start of string | `^http` matches URLs starting with "http" |
| `$` | End of string | `\.pdf$` matches PDF files |
| `\|` | Alternation (OR) | `cat\|dog` matches "cat" or "dog" |
| `[abc]` | Character class | `[aeiou]` matches any vowel |
| `[^abc]` | Negated class | `[^0-9]` matches non-digits |
| `\b` | Word boundary | `\bthe\b` matches "the" not "there" |
| `\w` | Word character | `[a-zA-Z0-9_]` |
| `\s` | Whitespace | Space, tab, newline |
| `\d` | Digit | `[0-9]` |

### Escaping Special Characters

When matching literal special characters, escape with backslash:

```bash
# Match literal dot
"example\.com"

# Match literal question mark
"why\?"

# Match literal parentheses
"\(important\)"

# Match literal brackets
"\[tag\]"
```

### Common Patterns

**URLs:**
```bash
"https?://.*github\.com"                # GitHub URLs
".*\.pdf$"                              # PDF links
```

**Versions:**
```bash
"v[0-9]+\.[0-9]+\.[0-9]+"              # Semantic versions
"[0-9]{4}-[0-9]{2}-[0-9]{2}"           # Dates (YYYY-MM-DD)
```

**Code-related:**
```bash
"python.*3\.[0-9]+"                     # Python 3.x mentions
"(typescript|javascript)"               # TS or JS
```

## Advanced Integration Tips

### With Claude Code

When Claude invokes this skill:
- Start with broader queries, narrow if too many results
- Prefer `simple` format for conversation readability
- Use `json` format only when parsing/processing needed
- Always include time constraints (`@1-week-ago`) for performance
- Limit to 20-50 results unless comprehensive list requested
- If no results, try simpler filter before giving up

### Query Construction Strategy

1. **Start with category tags:** `+programming`
2. **Add read status:** `+programming +unread`
3. **Add time constraint:** `+programming +unread @1-week-ago`
4. **Add text filter if needed:** `+programming +unread @1-week-ago rust`
5. **Limit results:** `+programming +unread @1-week-ago rust #20`

This progressive approach ensures good performance and relevant results.
