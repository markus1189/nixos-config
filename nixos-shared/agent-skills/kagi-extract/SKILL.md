---
name: kagi-extract
description: "PAID last resort (~$0.004/URL): extracts a blocked or paywalled page, or a remote PDF, as markdown via Kagi. Only after WebFetch/curl fail and no dedicated skill covers the site. Not for private instances (Confluence, Jira, GitLab): no credentials, fails and still bills."
---

# kagi-extract

Kagi's crawlers reach what WebFetch/curl cannot, and return markdown, not HTML.
API: <https://kagi.com/api/playground/extract>

## Precedence — this one costs money

1. dedicated skill for the site: `reddit`, `hackernews`,
   `codecentric-confluence`, `agent-browser` (real, logged-in browser)
2. WebFetch, or `curl -sL "$URL" | pandoc -f html -t gfm-raw_html`
3. this skill, once 1 and 2 are out or have failed

**Works** in the ledger ≠ prefer this. `news.ycombinator.com` extracts fine;
the free `hackernews` skill still wins.

**Private/self-hosted — never.** Confluence, Jira, GitLab, internal wikis: no
credentials, no VPN → nothing back, billed anyway.

## Usage

```bash
./scripts/kagi-extract.sh <url> [url ...]        # 1-10 https:// URLs per call
./scripts/kagi-extract.sh -o notes/ <url>        # choose output dir
./scripts/kagi-extract.sh --stdout <url>         # skip files, print markdown
./scripts/kagi-extract.sh -t 60 <url>            # longer server-side budget
```

One `.md` per URL into a temp dir (or `-o DIR`), one status line each:

```
OK   5129   /tmp/claude-code.kagi.21KFj2/01-x.com-AnthropicAI.md   https://x.com/AnthropicAI
FAIL No data returned from crawlers! url=https://...                https://...
```

- run from the skill dir; Nix shebangs, no setup
- `https://` only
- one call per batch, ≤10 URLs, fetched concurrently
- `--stdout` for short pages only — 70k+ chars is normal; else read/grep the file
- exit 0 = ≥1 OK, 1 = all failed, 2 = request-level (bad key, >10 URLs,
  non-https, network)
- $4/1000 pages, billed per URL **attempted**, failures included. Never in a
  loop without user consent.

## Domain ledger

[references/domains.md](references/domains.md) — **Works** / **Mixed** /
**Never works**.

Script-enforced: never-works domains and in-batch duplicates are dropped
pre-request (`SKIP`, free); the rest proceeds. `--force` overrides.

MUST append every outcome, following that file's *Append protocol*. Only
memory across sessions.

## Failure modes

- `No data returned from crawlers!` — per-URL, **arrives with HTTP 200**.
  Never read a 200 as success without checking status lines.
- `empty extraction (no markdown, no error)` — neither content nor reason. Billed.
- `bad status code: NNN` — the origin's answer to Kagi. 404 = wrong URL;
  re-check before spending again.
- `general.invalid_token` — key malformed/missing. `$KAGI_API_KEY`, else
  `pass api/kagi/search`.
- rate/credit → <https://kagi.com/api/billing>

## Fixes

<!-- Add only for failures actually observed in use. -->
