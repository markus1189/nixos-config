---
name: nzb-search
description: "Search and download NZB files from Usenet indexers (Treasure Maps, NZBgeek, NZBFinder, NZBPlanet, DrunkenSlug) for movies, TV shows, books, and other media. Use when the user wants to find or download content from Usenet, mentions NZBs, asks for movies/TV/books with download intent, or wants to manage their cart."
---

# NZB Search

Search Newznab-compatible Usenet indexers, rank the releases, and download the
NZB (or add it to the Treasure Maps cart).

**Script Execution:** Scripts should be executed from the skill directory.

## Indexers

| Indexer                 | Prefix          | Notes                                       |
| ----------------------- | --------------- | ------------------------------------------- |
| Treasure Maps (default) | `@treasuremaps` | Only one with a cart; German/Spanish categories; reports no grabs |
| NZBgeek                 | `@nzbgeek`      |                                             |
| NZBFinder               | `@nzbfinder`    | **15 calls/24h** — only when asked or others came up empty |
| NZBPlanet               | `@nzbplanet`    |                                             |
| DrunkenSlug             | `@drunkenslug`  | No `book` search, use `search` + `&cat=7000` |

API keys live in `pass` at `api/<indexer>`. Capability matrix, per-indexer
quirks and the full category tree: [references/indexers.md](references/indexers.md).

## Workflow

1. **Search** with the most specific command (`movie`, `tvsearch`, `book`, else `search`)
2. **Rank** by piping into `results` (flat records, sorted by grabs)
3. **Present** the top 3–5, numbered
4. **Act** on the user's pick: download, or `cartadd` on Treasure Maps
5. **Offer** to hand a downloaded NZB to the premiumize skill — offer only, never do it unasked

## Searching

```bash
./scripts/nzb-api.sh [@indexer] search   "QUERY"            ["&cat=2000&limit=50&extended=1"]
./scripts/nzb-api.sh [@indexer] movie    "TITLE or tt1375666" ["&extended=1"]
./scripts/nzb-api.sh [@indexer] tvsearch "SHOW" [SEASON [EP]] ["&extended=1"]
./scripts/nzb-api.sh [@indexer] book     "author:tolkien"   ["&limit=10"]
./scripts/nzb-api.sh search_all [--include nzbfinder] "QUERY" ["&cat=2000"]
```

- Trailing `&key=value` arguments are passed to the indexer verbatim; always
  add `&extended=1`, without it there are no grabs, resolution or subtitles.
- `search_all` queries every indexer in parallel (NZBFinder only with
  `--include`), reports failing ones on stderr and merges the rest.
- Indexer errors (bad key, rate limit) exit non-zero with the reason on stderr.
  Empty output with exit 0 really means no results.

Common parameters: `&cat=N`, `&limit=N` (max 100), `&maxage=DAYS`,
`&extended=1`.

Common categories: `2000` Movies, `5000` TV, `7000` Books, `3030` Audiobooks.
For **German** content use the DE categories, the plain ones are mostly
English: `2100` Movies DE, `5100` TV DE, **`7120` German ebooks**, `3130`
Audiobook DE (Treasure Maps).

## Ranking with `results`

Pipe any search output into `results`. Do not hand-roll jq over the raw response.

```bash
./scripts/nzb-api.sh search "inception" "&cat=2000&limit=50&extended=1" \
  | ./scripts/nzb-api.sh results --table
```

```
1. Inception.2010.1080p.BluRay.x264-GRP
   8.2 GB · grabs 340 · 1080p · subs: English · treasuremaps
   guid bbb222 · Tue, 02 Sep 2026 10:00:00 +0000
```

- `--sort grabs` (default) | `size` | `none` (indexer order, usually newest first).
  Treasure Maps reports no grabs (`grabs ?`), so its results keep indexer
  order: rank them by resolution, size and subs instead
- Without `--table`: a JSON array of `{indexer, title, guid, size_gb, grabs,
  resolution, subs, category, pubDate}`. Use it for filtering, e.g.
  `| jq 'map(select(.resolution != "2160p"))'`
- `resolution` is `1080p`-style: from the title, else from the metadata's
  `WxH`; `null` if neither has it

**What to prefer:**

1. 1080p, then 720p; skip 2160p/4K unless asked (unnecessarily large)
2. More grabs among equal quality
3. `subs` containing English when subtitles were requested
4. Plausible size: movie 720p 1–4 GB, 1080p 2–8 GB; TV episode 720p 0.5–1.5 GB, 1080p 1–3 GB

**Presenting:** title, size, grabs, subs when relevant, and the indexer when
results came from more than one.

## Download and Cart

```bash
./scripts/nzb-api.sh [@indexer] download GUID ["path/name.nzb"]   # prints the saved path
./scripts/nzb-api.sh cartadd GUID                                 # Treasure Maps only
./scripts/nzb-api.sh cartdel GUID
```

- Use the `@indexer` the result came from: a GUID is only valid on its own indexer.
- Without a path the NZB lands in `$TMPDIR/nzb-search/GUID.nzb` (`/tmp` if
  unset); give a readable name when the user will see the file.
- `download` checks the payload is an NZB, so an error page (rate limit,
  premium-only UHD) fails instead of being saved as `.nzb`.
- **Handoff:** after a download, offer to start it on Premiumize via the
  premiumize skill (`transfer-create-file PATH`). Run it only if the user says yes.

## Other Commands

```bash
./scripts/nzb-api.sh [@indexer] details GUID   # full metadata for one release
./scripts/nzb-api.sh [@indexer] nfo GUID       # release NFO as text
./scripts/nzb-api.sh [@indexer] caps           # categories and supported searches
./scripts/nzb-api.sh list_indexers
./scripts/nzb-api.sh [@indexer] api "t=...&..."  # raw Newznab call, normalized
```

## Examples

**"Find Inception with English subtitles":**
`movie "inception" "&cat=2000&limit=50&extended=1" | results --table`, keep
1080p/720p with English `subs`, present the top 3–5, download the pick, offer Premiumize.

**"Latest episode of show X":**
`tvsearch "show x" S E "&extended=1" | results --table`, prefer 1080p by grabs.

**"German J.D. Robb ebooks":**
`search "j.d. robb" "&cat=7120&limit=50&extended=1" | results --sort none --table`,
then order by book number from the titles.

**"Rare 1985 movie, try everywhere":**
`search_all "obscure movie 1985" "&cat=2000&extended=1" | results --table`; still
nothing → ask before spending NZBFinder quota with `--include nzbfinder`.
