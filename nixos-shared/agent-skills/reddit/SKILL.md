---
name: reddit
description: "Searches Reddit posts and comments, reads comment threads, browses subreddits, and reads the user's own frontpage, subscriptions, and saved history via the official Reddit OAuth API. Use when the user mentions Reddit or a subreddit, asks what Reddit thinks about a topic, wants Reddit opinions or discussion, pastes a reddit.com link, or asks about their Reddit frontpage, feed, subscriptions, or saved posts. Use instead of WebFetch for reddit.com URLs, which return 403 anonymously."
---

# Reddit

Read-only, via the official OAuth API. Credentials come from `pass` unaided — no
`env` prefix, no setup. Re-linking the account: [references/auth.md](references/auth.md).

## Commands

For any reddit.com link use `url` — it routes by shape (thread, comment
permalink, subreddit, user, `/s/` share link, `redd.it` shortlink) and focuses
the single comment when given a comment permalink.

```bash
./scripts/reddit.py url URL
./scripts/reddit.py search QUERY [--sub S] [--sort relevance|hot|top|new|comments] [--time all|year|month|week|day]
./scripts/reddit.py search-comments QUERY [--sub S] [--threads N]     # read the caveat
./scripts/reddit.py comments POST [--sort top] [--depth 4]            # id, t3_id, or thread URL
./scripts/reddit.py sub SUB [--sort hot|new|top|rising] [--time T]
./scripts/reddit.py user NAME [overview|submitted|comments]

./scripts/reddit.py frontpage [--sort best|hot|new|top]   # the user's OWN feed
./scripts/reddit.py subs
./scripts/reddit.py history saved|upvoted|submitted|comments|downvoted|hidden
```

All but `whoami` take `--limit N`, `--json` (raw API) and `--jsonl` (one flat
record per post/comment: permalink, ISO date, depth, parent, full body; ~5x
smaller than `--json`), before or after the subcommand. Prefer `--jsonl` for
anything you will parse or quote. `--body-chars 0` prints full bodies in text
mode. `frontpage`, `subs` and `history` need user context and fail loudly
without it, rather than passing generic popular posts off as the feed.

## Narrowing search

`--sub` takes several subreddits joined by `+` (`--sub ClaudeAI+ClaudeCode`).
The query accepts Reddit's operators: `title:"exact phrase"`, `selftext:word`,
`author:name`, `NOT word`. An ambiguous term (a tool named after a cartoon
character) drowns in off-topic hits site-wide: scope to subreddits and use
`title:"…"` before raising `--limit`.

## Comment search is a heuristic

**Reddit's API has no comment index.** `type=comment` on `/search` is silently
ignored and returns posts. Any tool claiming true comment search is scraping.

`search-comments` instead finds relevant *threads*, then ranks their comments by
query-term overlap. So it cannot find a comment in a thread whose *title* does
not match the query. Raise `--threads` for coverage (one API call each). When the
thread is already known, `comments` on it directly is exact.

## Notes

- `… replies continue deeper → URL` marks a branch cut by `--depth`;
  `… 14 more replies → URL` marks siblings withheld by `--limit`. Raise the
  flag, or pass the URL to `url`.
- API reference: <https://www.reddit.com/dev/api>

**Script Execution:** Scripts should be executed from the skill directory.
All scripts use Nix shebangs so no manual dependency installation is required.
