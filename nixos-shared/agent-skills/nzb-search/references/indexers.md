# Indexer Reference

Load this when you need a category ID beyond the common ones, a per-indexer
quirk, or are adding an indexer.

## Capability Matrix

| Operation        | Treasure Maps          | NZBgeek                      | NZBFinder          | NZBPlanet                    | DrunkenSlug        |
| ---------------- | ---------------------- | ---------------------------- | ------------------ | ---------------------------- | ------------------ |
| Search           | ✅                     | ✅                           | ✅                 | ✅                           | ✅                 |
| Movie / TV search| ✅                     | ✅                           | ✅                 | ✅                           | ✅                 |
| Book search      | ✅                     | ✅                           | ❌ use `search` + `&cat=7000` | ✅               | ❌ use `search` + `&cat=7000` |
| Download NZB     | ✅                     | ✅                           | ✅                 | ✅                           | ✅                 |
| Add/remove cart  | ✅ `cartadd`/`cartdel` | ❌ web-only (session cookie) | ❌                 | ❌ web-only (session cookie) | ❌                 |
| View cart        | ❌                     | ❌                           | ❌                 | ❌                           | ❌                 |

## Per-Indexer Notes

**NZBgeek:** cart operations need a web session and internal release IDs the
Newznab API does not expose. Download instead.

**NZBPlanet:** cart needs a web session (POST `/cart?add=ID` with PHPSESSID).
The Newznab `t=cartadd` endpoint is documented but answers error 300.
Download instead.

**NZBFinder:**

- **Rate limit:** free tier allows 15 API calls per 24h. `search_all` skips it
  unless given `--include nzbfinder`.
- **UHD downloads** need a premium account; non-UHD works on the free tier.

**DrunkenSlug:**

- `t=caps&o=json` returns literal `null`; `caps` falls back to the XML document.
- `t=get` answers 302 to `/getnzb/<guid>.nzb`; `download` follows redirects.
- Omits the `.channel` wrapper and uses `{_name,_value}` / `{_url,_length,_type}`
  / `guid.text`; see Response Normalization.

## Categories

Categories are standardized by Newznab, but availability varies by indexer.
`caps` lists what an indexer actually has.

### English

- **2000** Movies: 2030 SD, 2040 HD, 2045 UHD, 2050 BluRay, 2060 3D
- **5000** TV: 5030 SD, 5040 HD, 5045 UHD, 5060 Sport, 5070 Anime, 5080 Documentary
- **7000** Books: 7010 Mags, 7020 Ebook, 7030 Comics
- **3000** Audio: 3010 MP3, 3020 Video, 3030 Audiobook, 3040 Lossless

### German (Treasure Maps)

- **2100** Movies DE: 2140 HD, 2145 UHD, 2150 BluRay
- **5100** TV DE: 5140 HD, 5145 UHD, 5160 Sport, 5170 Anime, 5180 Documentary
- **7100** Books DE: 7110 Mags, **7120 Ebook**, 7130 Comics
- **3130** Audiobook DE

### Spanish (Treasure Maps)

- **2200** Movies ES, **5200** TV ES, **3230** Audiobook ES

### Other

- **1000** Console, **4000** PC (games, software, mobile), **6000** XXX, **8000** Other

## Response Normalization

Every command that returns search results runs the indexer's JSON through
`normalize_response`, so all indexers come out in one shape:
`{channel: {item: [...]}}` with `attr` entries as `{"@attributes": {name, value}}`
and every item tagged with `.indexer`. A single result stays a bare object
under `.channel.item`; `results` flattens both cases. What gets rewritten:

- Top-level `.item` (DrunkenSlug) → wrapped under `.channel.item`
- `newznab:attr` (NZBFinder, DrunkenSlug) → `attr`
- `{_name,_value}` (DrunkenSlug) → `{"@attributes":{name,value}}`
- `guid."@content"` (NZBFinder) / `guid.text` (DrunkenSlug) → GUID taken from the
  URL, set as a flat string and added to `attr`
- `{_url,_length,_type}` enclosure (DrunkenSlug) → `{"@attributes":{url,length,type}}`

Indexer errors (Newznab XML `<error description=…>` even under `o=json`, or a
JSON `error` object) exit non-zero with the description on stderr.

## Adding an Indexer

Add a line to `INDEXERS` in `scripts/nzb-api.sh`, format
`["name"]="BASE_URL|PASS_PATH"`, and put its API key in `pass` at that path.
If its JSON deviates from the canonical shape, extend `normalize_response`
and add a fixture plus test to `tests/nzb-search.bats` (repo-side, next to
the skill directory).
