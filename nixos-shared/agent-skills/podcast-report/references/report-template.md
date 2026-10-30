# Report template

Language: German episode → report in German. Any other language → English.
Quotes stay in the original language. Keep quotes short (one or two sentences); never reproduce
long transcript passages.

Target length: 1,300–2,000 words total. Readers open this on a phone: short lines, no wide tables
except where stated.

Links: print one only if you opened it; else mark it "(search result)". Unresolved → "not found".
openai.com and sec.gov answer 403: say so, don't guess the content.

Timestamps are `HH:MM:SS` from the transcript. If `transcript` reported `"timestamps": false`,
write `~HH:MM:SS` where the chunk or section start gives an estimate, and omit them otherwise.

```
# <Episode title>
<Show> · <publish date> · <duration> · [episode page](url) · [Doc](claude doc url)
Transcript: <source> · Speakers: <how attributed, one line>

## TL;DR
3 bullets.

## Who's talking
Table: name | role (host/guest) | label in transcript | evidence for the mapping.
One line on overall attribution reliability. Statements whose speaker can't be established are
"speaker unclear" everywhere in the report — never guessed.

## Guest profiles
Per person on the show (guests first; regular hosts only briefly, one line each):
role, affiliation, why they're on now, with a source URL.
🔴 Conflict of interest on its own line directly under the person, for anyone (host or guest)
speaking about their employer's product, their own company, a funder or a sponsor.
None found → say so in the same line as the profile.

## Summary
~250 words of prose. What was covered and argued, not whether it's true.

## Highlights
5–8 items: `HH:MM:SS` · speaker · the point in one line · optional short quote.
Pick what is surprising, quotable or decision-relevant, not merely "covered".
If a sourced fact contradicts what was said, add it to that highlight in one line with the link
(e.g. "says 10 October; OpenAI's post says 1 October"). No separate claim-check section.
Every entry in `bookmarks` of episode.json (moments the user marked while listening) gets a
highlight on top of the 5–8, marked 🔖 with the bookmark's title if it has one.

## Things to try
- [ ] checkbox list, up to 8 concrete actions (a tool, a habit, an experiment, a setting).
If the episode has none, omit the whole section.

## Leads
Books, papers, tools, companies, people worth following up, as a list (not a table):
- **item** (type) — why it came up `HH:MM:SS` — link
Max 12; drop the trivial ones (the show's own conference, generic homepages).

## Blind spots
Only for opinion-heavy or one-sided segments: 3–5 one-liners on what was not asked, not
mentioned, or assumed. Not a rebuttal. Pure news/recap episode → omit the section.

## Related notes
`rg -il --glob '*.md' --glob '!<this report's filename>' TERM ~/Stuff/20*` for 4–6 distinctive
terms. List up to 6 hits as `path` — one line why related. Lead with any earlier note on this
same episode. Skip daily digests (`hn-daily.md`, `hn-wrapup.md`) unless the hit is
specifically about the topic. Only hits with a real connection; none → omit the section.
```
