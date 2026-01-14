---
name: elfeed
description: Search the user's Emacs elfeed RSS feed database containing curated feeds from Reddit, blogs, YouTube, GitHub releases, and newsletters. Use when the user asks about articles they've read, mentions RSS feeds or 'something I read', wants to research topics from their curated sources (programming, AI, security, NixOS, Emacs, etc.), or needs to triage unread items.
---

# Searching Elfeed RSS Database

Elfeed is the user's primary information aggregation system. This skill provides CLI access to query the database.

## Quick Start

The primary tool is `elfeed-cli` located at:
```
~/.claude/skills/elfeed/scripts/elfeed-cli
```

Or use the absolute path from this repo:
```
/home/markus/repos/nixos-config/nixos-shared/claude/skills/elfeed/scripts/elfeed-cli
```

**Most common usage:**
```bash
# Search recent unread items about a topic
elfeed-cli query "topic +unread @1-week-ago" simple 20

# Get database statistics
elfeed-cli stats

# List all feeds
elfeed-cli feeds

# List all tags
elfeed-cli tags
```

## Filter Syntax Quick Reference

Combine these operators to build queries:

| Operator | Meaning | Example |
|----------|---------|---------|
| `+tag` | Must have tag | `+unread`, `+programming`, `+reddit` |
| `-tag` | Must not have tag | `-unread`, `-youtube` |
| `@time` | Newer than time | `@1-week-ago`, `@3-days-ago`, `@1-month-ago` |
| `#N` | Limit to N entries | `#10`, `#50` |
| `text` | Title/link matches (regex) | `nixos`, `claude.*ai` |
| `!regex` | Title/link doesn't match | `!advertisement` |
| `=regex` | Feed URL/title matches | `=simonwillison` |
| `~regex` | Feed URL/title doesn't match | `~reddit` |

**Combine operators with spaces:**
```bash
"+programming +unread @1-week-ago #20"
"+reddit -unread ai.*agents"
"+github =anthropic"
```

## Common Workflows

### 1. Research a Topic
```bash
# Find recent articles about a topic
elfeed-cli query "topic-keyword +unread @2-weeks-ago" simple 30
```

### 2. Check Unread by Category
```bash
# Recent unread programming articles
elfeed-cli query "+programming +unread @1-week-ago" simple 20

# YouTube videos unwatched
elfeed-cli query "+youtube +unread" simple 10
```

### 3. Analyze Reading Patterns
```bash
# Get overall statistics
elfeed-cli stats

# See all feed sources
elfeed-cli feeds

# Check what tags are used
elfeed-cli tags
```

### 4. Find Specific Article
```bash
# Search by keyword across all entries
elfeed-cli query "specific-term" simple 50

# From specific source
elfeed-cli query "=simonwillison llm" simple 20
```

## Output Formats

The `query` command supports three formats:

1. **simple** (default, most readable):
   ```
   2025-10-19 14:30 | Blog Name | Article Title | https://example.com/article
   ```

2. **json** (for parsing):
   ```json
   [{"title":"...","link":"...","date":...,"tags":[...],"feed_title":"...","feed_url":"..."}]
   ```

3. **sexp** (Emacs s-expression):
   ```elisp
   ((:title "..." :link "..." :date ... :tags (...) :feed-title "..." :feed-url "..."))
   ```

## Important Notes

- **Requires running Emacs**: Uses `emacsclient` to query live database
- **Read-only**: Queries don't modify read/unread status
- **Tag system**: Feeds tagged by category (programming, reddit, youtube, github, etc.)
- **Real-time data**: Reflects current database state

## Advanced Usage

Read `reference.md` when:
- Building complex multi-tag filters or regex patterns
- Queries produce unexpected results requiring troubleshooting
- Need performance optimization for large result sets
- Integrating with other tools (jq, pipes, etc.)
