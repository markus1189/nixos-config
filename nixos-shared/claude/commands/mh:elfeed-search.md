---
description: Search elfeed database without modifying Emacs state
argument-hint: <filter-expression>
---

Search the elfeed database using a read-only approach that doesn't modify the running Emacs instance state.

## Search Approach

We query elfeed using `emacsclient --eval` with Emacs Lisp code that:
1. Requires elfeed to ensure it's loaded
2. Uses `with-elfeed-db-visit` to iterate through all entries
3. Filters entries based on the provided search criteria
4. Returns formatted results without modifying any buffers or state

## Your Search Query

$ARGUMENTS

## Instructions

Execute a search on the elfeed database using the provided filter expression. The filter should support:

- **Tag filtering**: Check for specific tags (e.g., `mh/pocketed`, `unread`)
- **Time filtering**: Filter by date ranges (e.g., last 7 days, specific dates)
- **Text search**: Search in entry titles, feed titles, or links
- **Combined filters**: Support multiple criteria together

Format the results as a readable list showing:
- Date (YYYY-MM-DD format)
- Entry title
- Link

Use the emacsclient approach demonstrated in this session to query elfeed without modifying the running Emacs state.
