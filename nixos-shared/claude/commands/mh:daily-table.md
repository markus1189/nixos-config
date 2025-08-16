# Daily Table Extraction

## Description
Extract my assigned roles from sprint duty tables by analyzing the most recent screenshot and creating a comprehensive assignment table.

## Prompt
Find the most recent screenshot in ~/Screenshots using an efficient file discovery approach, extract all visible sprint duty tables, and systematically search for "Markus" assignments.

**File Discovery Strategy:**
Use `find ~/Screenshots -mtime -1 -type f | sort -r` to get the most recent files (within last 24 hours), sorted newest first.

**Analysis Requirements:**
1. **Thorough Search**: Examine EVERY column in ALL tables row by row
2. **Column Types**: Check all possible role columns including:
   - Daily, Spoc, Livegang
   - Daily Moderation order, Daily Moderation Discounter, Daily Moderation core
   - Any other role/duty columns present
3. **Data Extraction**: For each "Markus" entry found:
   - Identify the exact column header (this becomes the Role)
   - Extract the corresponding Day of Week and Date from that row
   - Verify no entries are missed by double-checking all tables

**Output Format:**
Create a markdown table with columns: Day of Week, Date, Role
- Use German day names as shown in source (Montag, Dienstag, etc.)
- Include date in DD.MM. format as shown in source
- Use exact column header names for Role
- Sort chronologically by date
- Ensure table is easily copyable for further use

**Quality Check:**
Before finalizing, re-examine the screenshot to confirm no "Markus" entries were overlooked in any table or column.
