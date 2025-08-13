# Daily Table Extraction

## Description
Extract my assigned roles from the most recent screenshot and create a table for the day + role I received.

## Prompt
Find the most recent screenshot in ~/Screenshots, extract two visible tables, and systematically search ALL columns in BOTH tables for any occurrence of "Markus".

For each "Markus" entry found:
1. Check every column: Daily, Spoc, Livegang, Daily Moderation order, Daily Moderation Discounter, Daily Moderation core
2. Go row by row through both tables
3. Note which column contains "Markus" - that column header becomes the Role
4. Extract the corresponding Day of Week and Date from that row

Create a table with columns: Day of Week, Date, Role
Format as a markdown table that can be easily copied and updated.
# Daily Table Extraction

## Description
Extract my assigned roles from the most recent screenshot and create a table for the day + role I received.

## Prompt
Find the most recent screenshot in ~/Screenshots, extract two visible tables, and create a table with columns: Day of Week, Date, Role

Format as a markdown table that can be easily copied and updated.
