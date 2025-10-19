# Elfeed Query Reference

Complete reference for advanced elfeed database queries.

## Filter Syntax

Elfeed uses a powerful filter language that combines multiple operators. Filters are composed of space-separated terms that are ANDed together.

### Tag Operators

**Include tag:** `+tag`
- Entry must have this tag
- Examples: `+unread`, `+programming`, `+reddit`, `+youtube`

**Exclude tag:** `-tag`
- Entry must NOT have this tag
- Examples: `-unread`, `-youtube`, `-spam`

**Common tags in the database:**
- Content type: `reddit`, `youtube`, `github`, `newsletter`
- Topics: `programming`, `ai`, `llm`, `security`, `hacking`, `nixos`, `emacs`
- Categories: `sport`, `analog`, `electronics`, `reading`, `news`
- Special: `unread`, `starred`

### Time-Based Filtering

**Newer than:** `@time-expression`

Supported time expressions:
- `@1-day-ago` or `@1-days-ago`
- `@2-weeks-ago` or `@2-week-ago`
- `@3-months-ago` or `@3-month-ago`
- `@1-year-ago` or `@1-years-ago`

Examples:
```bash
# Last 24 hours
@1-day-ago

# Last week
@1-week-ago

# Last 30 days
@1-month-ago
```

**Note:** Elfeed doesn't support "older than" directly. Use date ranges by fetching all and filtering programmatically if needed.

### Text Matching

**Entry title/link contains:** `text` or `regex`
- Matches against entry title and link URL
- Treated as regular expression
- Case-sensitive by default

Examples:
```bash
# Simple text match
nixos

# Regex pattern
claude.*ai

# Multiple words (must all match)
machine learning transformers
```

**Entry title/link doesn't contain:** `!regex`
```bash
# Exclude entries with "advertisement"
!advertisement

# Exclude multiple patterns
!spam !promoted
```

### Feed Filtering

**Feed URL/title matches:** `=regex`
- Matches against the feed's URL or title
- Useful for filtering to specific sources

Examples:
```bash
# All entries from Simon Willison's blog
=simonwillison

# All anthropic-related feeds
=anthropic
```

**Feed URL/title doesn't match:** `~regex`
```bash
# Exclude all Reddit feeds
~reddit

# Exclude YouTube
~youtube
```

### Result Limiting

**Limit results:** `#N`
- Returns maximum N entries
- Applied after other filters

Examples:
```bash
# Latest 10 entries
#10

# Top 100 unread
+unread #100
```

## Combining Filters

Filters are combined with AND logic. Space-separate multiple conditions.

### Examples by Use Case

**Recent unread programming content:**
```bash
"+programming +unread @1-week-ago #20"
```

**AI articles from last month, excluding Reddit:**
```bash
"+ai -reddit @1-month-ago"
```

**All GitHub releases, newest 50:**
```bash
"+github #50"
```

**Unread YouTube videos about NixOS:**
```bash
"+youtube +unread nixos"
```

**Everything from Simon Willison's blog:**
```bash
"=simonwillison"
```

**Recent unread, excluding certain topics:**
```bash
"+unread @3-days-ago -sport -youtube"
```

## CLI Command Reference

### Query Command

```bash
elfeed-cli query [filter] [format] [limit]
```

**Parameters:**
- `filter` - Filter expression (default: `+unread`)
- `format` - Output format: `simple`, `json`, or `sexp` (default: `simple`)
- `limit` - Maximum results (optional, overrides `#N` in filter)

**Examples:**
```bash
# Default: unread entries in simple format
elfeed-cli query

# Custom filter
elfeed-cli query "+programming @1-week-ago"

# JSON output for parsing
elfeed-cli query "+unread" json

# Limit results
elfeed-cli query "+unread" simple 20
```

### Stats Command

```bash
elfeed-cli stats
```

Returns database statistics in s-expression format:
```elisp
(:total-entries 15420
 :unread-entries 342
 :total-feeds 89
 :last-update 1729353600)
```

### Feeds Command

```bash
elfeed-cli feeds
```

Lists all feeds with their titles, URLs, and authors:
```elisp
((:title "Simon Willison's Weblog"
  :url "https://simonwillison.net/atom/everything/"
  :author nil)
 (:title "Hacker News: Front Page"
  :url "..."
  :author nil)
 ...)
```

### Tags Command

```bash
elfeed-cli tags
```

Returns list of all tags used in the database:
```elisp
(unread starred programming reddit youtube github ai llm security ...)
```

## Output Format Details

### Simple Format

Human-readable, one entry per line:
```
2025-10-19 14:30 | Feed Title | Entry Title | https://example.com/article
```

Best for: Direct reading, quick scanning

### JSON Format

Structured data for programmatic processing:
```json
[
  {
    "title": "Article Title",
    "link": "https://example.com/article",
    "date": 1729353600,
    "tags": ["unread", "programming"],
    "feed_title": "Blog Name",
    "feed_url": "https://example.com/feed.xml"
  }
]
```

Best for: Parsing, processing, filtering in other tools

### S-expression Format

Emacs Lisp data structure:
```elisp
((:title "Article Title"
  :link "https://example.com/article"
  :date 1729353600
  :tags (unread programming)
  :feed-title "Blog Name"
  :feed-url "https://example.com/feed.xml"))
```

Best for: Emacs integration, Lisp processing

## Advanced Patterns

### Multi-Tag Filtering

**Find entries with multiple required tags:**
```bash
"+ai +llm +unread"
```

**Exclude multiple tags:**
```bash
"+programming -youtube -reddit"
```

**Mix include/exclude:**
```bash
"+ai +unread -reddit -youtube"
```

### Complex Text Matching

**Regex alternation:**
```bash
"(nixos|nix-darwin|home-manager)"
```

**Case variations (requires regex):**
```bash
"[Cc]laude|CLAUDE"
```

**Word boundaries:**
```bash
"\bAI\b"  # Matches "AI" but not "AIr" or "mAIn"
```

### Time-Based Workflows

**Daily digest:**
```bash
"+unread @1-day-ago"
```

**Weekly review:**
```bash
"+programming @1-week-ago #50"
```

**Monthly archive search:**
```bash
"-unread @1-month-ago topic-keyword"
```

### Feed-Specific Queries

**Single feed deep dive:**
```bash
"=simonwillison @1-month-ago"
```

**All blogs (exclude aggregators):**
```bash
"-reddit -youtube -github"
```

**Compare sources:**
```bash
# Run separately to compare coverage
"=source1 topic"
"=source2 topic"
```

## Troubleshooting

### No Results Returned

**Check:**
1. Is Emacs running? (`emacsclient` must connect)
2. Is elfeed loaded? (Should auto-load in user's config)
3. Are filters too restrictive? Try simpler filter
4. Has elfeed updated recently? Check `elfeed-cli stats`

### Incorrect Results

**Debug steps:**
1. Simplify filter to one condition at a time
2. Check available tags: `elfeed-cli tags`
3. Verify feed sources: `elfeed-cli feeds`
4. Test regex patterns separately

### Performance Issues

**Large result sets:**
- Use `#N` to limit results
- Add time constraints: `@1-week-ago`
- Filter by specific tags to narrow scope

**Example optimization:**
```bash
# Slow: entire database
"programming"

# Fast: recent subset
"+programming @1-week-ago #50"
```

## Integration Tips

### With Claude Code

When Claude invokes this skill:
- Start with broader queries, narrow if too many results
- Prefer `simple` format for readability in conversation
- Use `json` format only when parsing/processing needed
- Always include time constraints for performance
- Limit to 20-50 results unless user needs comprehensive list

### With Other Tools

**Pipe to processing tools:**
```bash
elfeed-cli query "+unread" json | jq '.[] | select(.tags | contains(["ai"]))'
```

**Export for analysis:**
```bash
elfeed-cli query "+programming @1-month-ago" json > analysis.json
```

**Generate reading list:**
```bash
elfeed-cli query "+starred" simple > reading-list.txt
```

## Regular Expression Reference

Elfeed uses standard regex syntax:

| Pattern | Meaning |
|---------|---------|
| `.` | Any character |
| `*` | Zero or more of previous |
| `+` | One or more of previous |
| `?` | Zero or one of previous |
| `^` | Start of string |
| `$` | End of string |
| `\|` | Alternation (OR) |
| `[abc]` | Character class |
| `[^abc]` | Negated character class |
| `\b` | Word boundary |
| `\w` | Word character |
| `\s` | Whitespace |

**Escaping special characters:**
```bash
# Match literal dot
"example\.com"

# Match literal question mark
"why\?"
```
