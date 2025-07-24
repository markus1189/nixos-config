# HackerNewsCli (`hn`) Command Overview

The `hn` tool is a command-line interface for browsing Hacker News content. Here's a comprehensive overview of its capabilities:

## Basic Structure
```
hn [OPTIONS] COMMAND [ARGS]...
```

## Core Commands

### Content Browsing Commands
All content commands accept an optional `[LIMIT]` parameter (defaults to 10):

- **`hn top [LIMIT]`** - Display top recent posts
- **`hn new [LIMIT]`** - Display latest posts
- **`hn best [LIMIT]`** - Display best posts from past few days
- **`hn ask [LIMIT]`** - Display "Ask HN" posts
- **`hn show [LIMIT]`** - Display "Show HN" posts
- **`hn jobs [LIMIT]`** - Display job posts
- **`hn onion [LIMIT]`** - Display onions

### Specialized Search Commands

#### `hn hiring [REGEX_QUERY]`
Search monthly "Who is Hiring" posts with regex patterns.
- **Options**: `-i, --id_post INTEGER` (search specific post ID)
- **Examples**:
  - `hn hiring "Python"`
  - `hn hiring "(?i)Python|JavaScript"` (case insensitive)
  - `hn hiring "(?i)Python" -i 8394339` (specific post)

#### `hn freelance [REGEX_QUERY]`
Search monthly "Seeking Freelancer" posts with regex patterns.
- **Options**: `-i, --id_post INTEGER`
- **Examples**: Similar to hiring command

### User Information
#### `hn user USER_ID`
Display user profile and submitted posts.
- **Options**: `-l, --limit INTEGER`
- **Examples**: `hn user tptacek`, `hn user patio11`

### Post Viewing
#### `hn view INDEX [REGEX_QUERY]`
View specific posts by index (from recent list) or post ID.

**IMPORTANT**: Index numbers refer to the **most recently executed** numbered list command. Running a new list command (like `hn top`, `hn best`, `hn ask`, etc.) invalidates previous index numbers. For example, if you run `hn top` and see item #3, then run `hn best`, the previous #3 is no longer valid - you must use the new numbered list.

**Key Options**:
- `-c, --comments` - Show comments
- `-cr, --comments_recent` - Show only recent comments (< 59 min)
- `-cu, --comments_unseen` - Show only unseen comments
- `-ch, --comments_hide_non_matching` - Hide non-matching comments
- `-b, --browser` - Open in browser
- `-cc, --clear_cache` - Clear comment cache
- `-cq, --comments_regex_query TEXT` - Filter comments with regex

**Examples**:
- `hn view 3` - View 3rd post from **most recent numbered list**
- `hn view 3 -c` - View with comments
- `hn view 10492086` - View specific post ID (always works)
- `hn view 3 "(?i)programmer" --comments` - Filter comments

**Best Practice**: Use specific post IDs for reliable access, or ensure you're referencing the correct recent list.

**IMPORTANT**: Always use `hn view [POST_ID]` instead of directly fetching URLs. The HN CLI is already optimized for console display and provides better formatting than direct web fetching.

## Usage Patterns

1. **Browse content**: Use `top`, `new`, `best`, `ask`, `show` for different feed types
2. **Job searching**: Use `jobs` for job posts, `hiring` with search terms for specific roles
3. **User research**: Use `user` to explore specific user's activity
4. **Deep reading**: Use `view` with various comment options for detailed post analysis
5. **Filtering**: Most commands support regex for precise content matching

## Advanced Analysis Techniques

### Topic Research & Trend Analysis
- **Combine list + grep**: `hn best 100 | grep -i -E "(topic1|topic2|topic3)" -A1` to find related stories
- **Time-based analysis**: Use `best` for past few days, `top` for recent hours, `new` for latest posts
- **Comment filtering**: Use `-cq "regex"` to find specific discussion themes within stories

### Content Discovery Patterns
- **High engagement indicators**: Stories with >500 points and >200 comments typically indicate major discussions
- **Controversial topics**: High comment-to-point ratios often signal debate-heavy topics
- **Show HN launches**: Use `show` command to track new tool/project announcements
- **Ask HN insights**: Use `ask` to find community questions and sentiment on topics

### Research Workflow Examples
```bash
# Find all AI/coding tool discussions from past month
hn best 100 | grep -i -E "(cursor|AI|coding|agent|LLM)" -A1

# Deep dive into specific high-engagement story comments
hn view 2 -c -cq "(?i)keyword1|keyword2"

# Track Show HN launches in specific domain
hn show 50 | grep -i "topic" -A1

# Analyze Ask HN community sentiment
hn ask 30 | grep -i "topic" -A1
```

### Best Practices for Analysis
- **Start broad, narrow down**: Begin with high limits (50-100), then focus on specific stories
- **Use specific post IDs** for reliable cross-session reference
- **Combine temporal views**: Compare `top` (recent) vs `best` (past week) for trend analysis
- **Filter comments strategically**: Use regex to find specific discussion themes rather than reading all comments
