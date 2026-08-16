# Weather reference

## Input

Coordinates only (lat/lng) — geocode an address first if needed.

## Units

`METRIC` (default) or `IMPERIAL`. Any other value returns HTTP 400 with an opaque error.

## Pagination

- Hourly: up to 240h, paginated internally (24 per page).
- Daily: up to 10d, paginated internally (5 per page).

The script accumulates pages and truncates to the requested count.

## Timezone handling

All times in `*-pretty` output are converted to local time using the response's `timeZone.id`. Internally the jq helpers strip the fractional-seconds `Z` suffix, `strptime` into epoch, then `strflocaltime` under `TZ=<id>` — so local hours render correctly without pulling in a tz library.
