# Restwerte Analysis

## Description
Extract Jira story information from the most recent screenshot and create a table for tracking remaining story points (Restwerte) in the current sprint.

## Prompt
Find the most recent screenshot in ~/Screenshots, extract all visible Jira stories, and create a table with columns: Nr, Story, Title, Estimated, Remaining. Leave the "Remaining" column empty for manual input.

The table should include:
- Sequential number (Nr) for easy reference
- Story numbers (e.g., FT1-29241)
- Story titles/descriptions
- Estimated story points (visible on the right side)
- Empty "Remaining" column for Restwerte input

Format as a markdown table that can be easily copied and updated.