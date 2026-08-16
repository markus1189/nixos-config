# Domain ledger

What Kagi's crawlers returned per domain. Observations from real calls only.

**The script parses this file.** Entries under `## Never works` are refused
before the request, costing nothing. The other sections are advisory.

Strict format, domain in backticks, one per line:

    - `example.com` — note, date observed

A bare domain matches subdomains: `reuters.com` covers `www.reuters.com`.

## Append protocol

Record every call — this is the only memory across sessions.

- Content returned → **Works**.
- `No data returned from crawlers!` → **Never works**. Deterministic; a
  retry costs money and fails identically.
- `bad status code: NNN` → **Mixed**, with the status. May be per-path.
- Content on some paths, `No data returned` on others → **Mixed**, naming which
  paths. A domain is not one behaviour.
- A **Never works** entry that suddenly yields content → move to **Works**,
  note the date.

Keep the note explaining why an entry sits where it does.

## Works

- `x.com` — profiles and posts, 2026-08
- `twitter.com` — same infrastructure as `x.com`, 2026-08
- `linkedin.com` — company pages, 34 KB, no login, 2026-08
- `news.ycombinator.com` — item pages, comment tree preserved, 2026-08.
  Extracts fine, but use the free `hackernews` skill instead.
- `wikipedia.org` — full articles, 74 KB on a long one, 2026-08
- `bitcoin.org` — remote PDF converted to markdown, 2026-08

Remote PDFs are converted rather than returned as binary, provided the
domain lets Kagi fetch them.

## Mixed

Worth one attempt; expect failure on some paths.

- `arxiv.org` — `/pdf/1706.03762` gave `bad status code: 500`, 2026-08.
  Abstract pages untested.
- `amazon.de` — `/dp/` product pages work: 126 KB, title, "About this item"
  bullets, rating and review count, review text, related products. **No price
  and no buy box** — do not use it for pricing. `/s?k=` search listings return
  `No data returned from crawlers!`. Both observed 2026-08.

## Unknown

Anything absent here. Try once, then record the outcome. Do not
pre-populate with guesses.

## Never works

Refused by the script. All returned `No data returned from crawlers!`.

⚠️ **Keep this section last in the file.** The script reads from this heading to
EOF, so any section appended below it gets absorbed into the blocklist.

- `reuters.com` — 2026-08, confirmed on retry
- `bloomberg.com` — 2026-08
- `g2.com` — 2026-08
- `crunchbase.com` — 2026-08
- `example.com` — 2026-08, too little content rather than blocked
- `example.org` — 2026-08, same
