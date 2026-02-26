---
description: Sprint Restwerte — extract Jira stories from latest screenshot into tracking table, copy as HTML to clipboard for Teams
---

Find the most recent screenshot in ~/Stuff/Today, extract all visible active Jira stories (skip greyed-out/parked items), and build a markdown table with columns: Nr, Story, Title, Estimated, Remaining (empty), Notizen. Include a Total row.

Once the user fills in Remaining values, update the table and copy it to clipboard as HTML for Teams pasting:

```bash
echo '...table...' | pandoc -f markdown -t html > /tmp/restwerte.html
nohup bash -c 'DISPLAY=:0 xclip -selection clipboard -t text/html -i /tmp/restwerte.html' >/dev/null 2>&1 &
```

Do NOT pipe directly into xclip — it hangs. The nohup+DISPLAY pattern is the only reliable approach.
