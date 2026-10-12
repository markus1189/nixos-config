# Hacker News — wrap-up

## Wrap-Up Mode

**Trigger**: user says "wrapup" (or variant) to end the session.

1. Read today's `hn-daily.md`
2. Write `hn-wrapup.md` to the same directory **before** any conversational output
3. Run `kb-index`
4. Confirm briefly — don't re-summarize

### Structure

```markdown
# HN Wrap-Up — [Weekday, Month Day, Year]

> Top N checked. N deep dives.

## The Day in One Paragraph
[dominant themes, tone, 2–3 standout stories — with voice]

## [Emoji] [Title] [[story_id]]

**[pts] pts · [comments] 💬 · [submitter]** · [thread](https://news.ycombinator.com/item?id=STORY_ID) · [domain.tld](ARTICLE_URL)
`tags: canonical, tags | +free, form`

[Distill, don't paste: what matters from story + thread, key artifacts/papers/commenters]

(one `##` per dive, no wrapper heading; `[story_id]` required — `.kb/series/hn-wrapup.md`
indexes these. Metadata + tags = the same contract as the daily file ([deep-dives.md](deep-dives.md#dive-metadata-contract)): copy across, don't
re-derive. Subsections `###` if the entry is long enough to need them.)

## Stories Noted (Not Dived)
| Story | Pts | 💬 | Why Notable |
(linked titles, as in the briefing tables; skip low-signal)

## Comment Highlights
(the day's best 5–10 across all checks, same row format; drawn from the dives' Notable Comments)

## Threads to Follow Tomorrow
(omit if nothing genuine to follow up on)
```

---

