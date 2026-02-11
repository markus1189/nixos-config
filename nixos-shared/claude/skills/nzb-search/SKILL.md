---
name: nzb-search
description: "Search and download NZB files from Usenet indexers (SceneNZBs, NZBgeek, NZBFinder, NZBPlanet) for movies, TV shows, books, and other media. Use when the user wants to find or download content from Usenet, mentions NZBs, asks for movies/TV/books with download intent, or wants to manage their cart."
---

# NZB Search

Search Newznab-compatible Usenet indexers for movies, TV shows, books, and other media. Find quality releases and add them to cart or download NZB files.

## Supported Indexers

| Indexer | Prefix | API Key Location | Cart Support |
|---------|--------|------------------|--------------|
| SceneNZBs (default) | `@scenenzbs` | `pass api/scenenzbs` | ✅ API |
| NZBgeek | `@nzbgeek` | `pass api/nzbgeek` | ❌ Web-only |
| NZBFinder | `@nzbfinder` | `pass api/nzbfinder` | ❌ Not supported |
| NZBPlanet | `@nzbplanet` | `pass api/nzbplanet` | ❌ Not supported |

## Workflow

1. **Search** - Query for content (movie, TV, book, etc.)
2. **Filter & Rank** - Prioritize by quality indicators
3. **Present** - Show top results with key details
4. **Action** - Download NZB (or add to cart for SceneNZBs)

## Quick Start - Most Common Search Pattern

**Use this pattern for any search** (works reliably across all indexers):

```bash
cd ~/.claude/skills/nzb-search && \
./scripts/nzb-api.sh [@indexer] search "QUERY" "&cat=CATEGORY&limit=50&extended=1" | \
jq -r '[.channel.item] | flatten | .[]? | {
  title: .title,
  guid: .guid,
  size: (.size // (.attr[]? | select(."@attributes".name == "size") | ."@attributes".value) // .enclosure."@attributes".length),
  grabs: ((.attr[]? | select(."@attributes".name == "grabs") | ."@attributes".value) // "N/A"),
  pubDate: .pubDate
} | "\n\(.title)\n  Size: \(if .size then ((.size | tonumber) / 1073741824 * 100 | floor / 100 | tostring) + " GB" else "Unknown" end)\n  Grabs: \(.grabs)\n  Published: \(.pubDate)\n  GUID: \(.guid)"'
```

Common categories: `2000` (Movies), `5000` (TV), `6000` (XXX), `7000` (Books), `7120` (German ebooks)

## Search Operations

All searches use `scripts/nzb-api.sh`. Prefix with `@indexer` to select indexer (default: scenenzbs).

**IMPORTANT - JQ Parsing:** The API returns different JSON structures depending on result count:
- **Multiple results**: `.channel.item` is an array
- **Single result**: `.channel.item` is an object (not an array)

Always use this pattern to handle both cases:
```bash
jq '[.channel.item] | flatten | .[]? | ...'
```

Do NOT use `.channel.item[]?` directly as it will fail on single results.

### Indexer Selection

```bash
# Use default indexer (scenenzbs)
./scripts/nzb-api.sh search "inception"

# Use specific indexer
./scripts/nzb-api.sh @nzbgeek search "inception"
./scripts/nzb-api.sh @scenenzbs search "inception"
./scripts/nzb-api.sh @nzbfinder search "inception"
./scripts/nzb-api.sh @nzbplanet search "inception"

# Search ALL indexers at once (results include .indexer field)
./scripts/nzb-api.sh search_all "inception" "&cat=2000"

# List available indexers
./scripts/nzb-api.sh list_indexers

# Get indexer capabilities
./scripts/nzb-api.sh @nzbgeek caps
```

### Reliable jq Parsing Patterns

**ALWAYS use these tested patterns** instead of improvising:

```bash
# 1. SIMPLE LIST - Just show titles and basic info
./scripts/nzb-api.sh search "query" "&cat=6000&limit=50&extended=1" | \
  jq -r '[.channel.item] | flatten | .[]? | {
    title: .title,
    guid: .guid,
    size: (.size // (.attr[]? | select(."@attributes".name == "size") | ."@attributes".value) // .enclosure."@attributes".length),
    grabs: ((.attr[]? | select(."@attributes".name == "grabs") | ."@attributes".value) // "N/A"),
    pubDate: .pubDate
  } | "\n\(.title)\n  Size: \(if .size then ((.size | tonumber) / 1073741824 * 100 | floor / 100 | tostring) + " GB" else "Unknown" end)\n  Grabs: \(.grabs)\n  Published: \(.pubDate)\n  GUID: \(.guid)"'

# 2. JSON OUTPUT - For further processing/sorting
./scripts/nzb-api.sh search "query" "&cat=6000&limit=50&extended=1" | \
  jq '[.channel.item] | flatten | .[]? | {
    title: .title,
    guid: .guid,
    size: (.size // (.attr[]? | select(."@attributes".name == "size") | ."@attributes".value) // .enclosure."@attributes".length | tonumber),
    grabs: ((.attr[]? | select(."@attributes".name == "grabs") | ."@attributes".value) // "0" | tonumber),
    pubDate: .pubDate
  }'

# 3. SORTED BY GRABS (most popular first)
./scripts/nzb-api.sh search "query" "&cat=6000&limit=50&extended=1" | \
  jq '[.channel.item] | flatten | .[]? | {
    title: .title,
    guid: .guid,
    size: (.size // (.attr[]? | select(."@attributes".name == "size") | ."@attributes".value) // .enclosure."@attributes".length | tonumber),
    grabs: ((.attr[]? | select(."@attributes".name == "grabs") | ."@attributes".value) // "0" | tonumber),
    pubDate: .pubDate
  }' | jq -s 'sort_by(-.grabs)'
```

**Key points:**
- Always use `[.channel.item] | flatten | .[]?` to handle single/multiple results
- Use `// "default"` for fallback values (grabs, size)
- Size can be in `.size`, `.attr[]`, or `.enclosure."@attributes".length` - try all three
- Convert to GB: `| tonumber / 1073741824 * 100 | floor / 100`
- GUID is usually in `.guid` directly (NZBgeek) or `.attr[]` (SceneNZBs)

### Movies

```bash
# By title
./scripts/nzb-api.sh movie "inception" "&cat=2000&limit=20&extended=1"

# By IMDb ID
./scripts/nzb-api.sh movie "0468569" "&extended=1"

# With quality filters and extended metadata
./scripts/nzb-api.sh search "inception" "&cat=2000&limit=20&extended=1"

# Search NZBgeek for movies
./scripts/nzb-api.sh @nzbgeek movie "inception"
```

### TV Shows

```bash
# Specific episode
./scripts/nzb-api.sh tvsearch "spartacus" 1 3

# Season pack
./scripts/nzb-api.sh tvsearch "breaking bad" 2

# General TV search
./scripts/nzb-api.sh search "the wire" "&cat=5000&limit=15"
```

### Books

```bash
# By author or title
./scripts/nzb-api.sh book "author:tolkien" "&limit=10"

# Direct search
./scripts/nzb-api.sh search "fantasy novels" "&cat=7000"
```

## Quality Filtering

**Priority indicators:**
1. **Grabs/stats** - Higher = more popular/reliable
2. **Resolution** - Prefer 720p or 1080p (avoid 2160p/4K - unnecessarily large)
3. **Subtitles** - Check `subs` attribute for "english" when requested
4. **Size** - Reasonable for quality level

### Finding Quality Releases

```bash
# Get extended metadata including grabs, resolution, subs
./scripts/nzb-api.sh search "movie name" "&cat=2000&limit=30&extended=1&sort=stats_desc" | \
  jq '[.channel.item] | flatten | .[]? | {
    title: .title,
    guid: .guid,
    size: (.size // (.attr[]? | select(."@attributes".name == "size") | ."@attributes".value) | tonumber),
    grabs: ((.attr[]? | select(."@attributes".name == "grabs") | ."@attributes".value) // "0"),
    resolution: ((.attr[]? | select(."@attributes".name == "resolution") | ."@attributes".value) // "unknown"),
    subs: ((.attr[]? | select(."@attributes".name == "subs") | ."@attributes".value) // "none")
  }'
```

**Filter logic:**
- Extract resolution from title if not in metadata (look for "720p", "1080p", "2160p")
- Prioritize 1080p, accept 720p
- Exclude 2160p/4K unless specifically requested
- When subtitles requested, filter by `subs` containing "english" or "en"
- Sort by grabs (popularity) among quality matches

### Size Ranges (approximate)

- **Movies 720p**: 1-4 GB
- **Movies 1080p**: 2-8 GB
- **TV episode 720p**: 500 MB - 1.5 GB
- **TV episode 1080p**: 1-3 GB

## Presenting Results

Show 3-5 top results with:
- Title (include resolution/quality indicators visible in title)
- Size (in GB, formatted)
- Grabs/popularity if available
- Subtitles info if relevant
- Indexer name (when using search_all or comparing across indexers)

Number results clearly for user selection.

## Cart & Download Operations

### Indexer Support Matrix

| Operation | SceneNZBs | NZBgeek | NZBFinder | NZBPlanet |
|-----------|-----------|---------|-----------|-----------|
| Search | ✅ Newznab API | ✅ Newznab API | ✅ Newznab API | ✅ Newznab API |
| Movie search | ✅ | ✅ | ✅ | ✅ |
| TV search | ✅ | ✅ | ✅ | ✅ |
| Book search | ✅ | ✅ | ❌ Not supported | ✅ |
| Download NZB | ✅ `t=get&id=GUID` | ✅ `t=get&id=GUID` | ✅ `t=get&id=GUID` | ✅ `t=get&id=GUID` |
| Add to cart | ✅ `t=cartadd&id=GUID` | ❌ Web-only (session cookie) | ❌ Not supported | ❌ Not supported |
| Remove from cart | ✅ `t=cartdel&id=GUID` | ❌ Web-only | ❌ Not supported | ❌ Not supported |
| View cart | ❌ Not implemented | ❌ Web-only | ❌ Not supported | ❌ Not supported |

**NZBgeek Note:** Cart operations require web session authentication with internal release IDs that aren't exposed via the Newznab API. For NZBgeek, use direct download instead of cart.

**NZBFinder Notes:**
- **Rate limit:** Free tier is limited to 15 API calls per 24 hours. Use sparingly — prefer other indexers for exploratory searches.
- **UHD downloads:** Require a premium account. Non-UHD downloads work on free tier.
- **No book search:** `t=book` endpoint is not supported. Use general `search` with `&cat=7000` as fallback.
- **No cart:** Use direct download (`t=get`) instead.

### Download NZB (Works on All Indexers)

```bash
# Download to file (recommended for NZBgeek)
./scripts/nzb-api.sh download "guid-hash-here" "movie-name.nzb"

# From specific indexer
./scripts/nzb-api.sh @nzbgeek download "guid-hash-here" "movie-name.nzb"

# Returns: filename
```

### Add to Cart (SceneNZBs Only)

```bash
# Add by GUID (uses default indexer - scenenzbs)
./scripts/nzb-api.sh cartadd "guid-hash-here"

# Response: {"@attributes": {"id": "internal-cart-id"}}
```

### Remove from Cart (SceneNZBs Only)

```bash
./scripts/nzb-api.sh cartdel "guid-hash-here"
```

## Examples

### "Find Inception with English subtitles"

1. Search movies: `./scripts/nzb-api.sh search "inception" "&cat=2000&limit=20&extended=1&sort=stats_desc"`
2. Filter for English subs, 720p/1080p, sort by grabs
3. Present top 3-5 results
4. Ask user which to add to cart or download

### "Find The Hobbit book by Tolkien"

1. Search books: `./scripts/nzb-api.sh book "hobbit tolkien" "&limit=15"`
2. Filter by author/title match
3. Present top 5 results with size/format
4. Add selected to cart

### "Find latest episode of show X"

1. TV search: `./scripts/nzb-api.sh tvsearch "show x" [season] [ep]`
2. Filter 720p/1080p by grabs
3. Present top 3
4. Download selected NZB

### "Find German J.D. Robb ebooks"

1. Search with German category: `./scripts/nzb-api.sh search "j.d. robb" "&cat=7120&limit=50&extended=1"`
2. Parse results and sort by book number or publication date
3. Present top results with German titles
4. Add selected to cart or download

### "Search both indexers for rare content"

1. Search all: `./scripts/nzb-api.sh search_all "obscure movie 1985" "&cat=2000&limit=20"`
2. Results include `.indexer` field showing source
3. Compare availability and quality across indexers
4. Use appropriate `@indexer` prefix for cart/download

## Categories

**Note:** Categories are standardized by Newznab, but availability varies by indexer. Use `caps` to check what an indexer supports.

### English Content
- **2000** = Movies
  - 2060 = 3D
  - 2050 = BluRay
  - 2040 = HD
  - 2030 = SD
  - 2045 = UHD
- **5000** = TV
  - 5040 = HD
  - 5030 = SD
  - 5045 = UHD
  - 5070 = Anime
  - 5080 = Documentary
  - 5060 = Sport
- **7000** = Books
  - 7020 = Ebook
  - 7030 = Comics
  - 7010 = Mags
- **3000** = Audio
  - 3030 = Audiobook
  - 3010 = MP3
  - 3040 = Lossless
  - 3020 = Video

### German Content (DE) - SceneNZBs specific
- **7100** = Books - DE
  - **7120** = Ebook (use this for German ebooks!)
  - 7130 = Comics
  - 7110 = Mags
- **2100** = Movies - DE
  - 2140 = HD
  - 2150 = BluRay
  - 2145 = UHD
- **5100** = TV - DE
  - 5140 = HD
  - 5145 = UHD
  - 5170 = Anime
  - 5180 = Documentary
  - 5160 = Sport
- **3130** = Audiobook - DE

### Spanish Content (ES) - SceneNZBs specific
- **2200** = Movies - ES
- **5200** = TV - ES
- **3230** = Audiobook - ES

### Other Categories
- **1000** = Console (PS4, PS5, Xbox, Switch, etc.)
- **4000** = PC (Games, Software, Mobile)
- **6000** = XXX
- **8000** = Other

**Important:** When searching for German content, always use the DE-specific categories (7120 for ebooks, 2100 for movies, 5100 for TV, etc.) as regular categories (7000, 2000, 5000) contain primarily English content. Check indexer capabilities with `caps` as not all indexers have localized categories.

## Additional Search Parameters

Append to search commands:
- `&limit=N` - max results (default varies, max 100)
- `&maxage=N` - posted within N days
- `&minsize=1GB` / `&maxsize=10GB` - size filters
- `&sort=stats_desc` - sort by popularity/grabs
- `&extended=1` - include all metadata (grabs, resolution, subs, imdb, etc.)

## Technical Notes

- **URL Encoding**: The script automatically URL-encodes all search queries, so spaces and special characters are handled correctly
- **Single vs Multiple Results**: Always use `[.channel.item] | flatten | .[]?` pattern in jq to handle both cases
- **Newznab Compatibility**: Both indexers use the standard Newznab API, so all commands work identically
- **API Keys**: Stored in `pass` - ensure keys exist at `api/scenenzbs`, `api/nzbgeek`, `api/nzbfinder`, and `api/nzbplanet`
- **Rate Limits**: NZBFinder free tier: 15 calls/24h. Other indexers have more generous limits.

## Adding New Indexers

To add a new Newznab-compatible indexer, edit the `INDEXERS` array in `scripts/nzb-api.sh`:

```bash
declare -A INDEXERS=(
    ["scenenzbs"]="https://scenenzbs.com/api|api/scenenzbs"
    ["nzbgeek"]="https://api.nzbgeek.info/api|api/nzbgeek"
    ["nzbfinder"]="https://nzbfinder.ws/api|api/nzbfinder"
    ["nzbplanet"]="https://api.nzbplanet.net/api|api/nzbplanet"
    ["newindexer"]="https://newindexer.com/api|api/newindexer"
)
```

Format: `["name"]="BASE_URL|PASS_PATH"`

The script normalizes response differences between indexers automatically (e.g., `newznab:attr` vs `attr`, different GUID structures). Non-JSON error responses (XML rate limit errors etc.) are handled gracefully in `search_all`.
