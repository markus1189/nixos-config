---
name: google-maps
description: "Queries Google Maps Platform and Google Weather APIs for geocoding, reverse-geocoding, directions with travel times, distance matrix, places search, place details, static map images, and current/hourly/daily weather forecasts. Use when the user mentions an address or lat/lng coordinates; asks how to get somewhere, travel time, ETA, or a route (driving, walking, bicycling, transit); asks for nearby places, restaurants, cafes, bars, or points of interest (including \"near me\" or \"closest\"); wants a map image; or asks about weather, forecast, temperature, rain, or conditions at a location."
---

# Google Maps

`./scripts/maps-api.sh` wraps Google Maps Platform + Google Weather APIs. API key from `pass api/claude-maps`.

Run `./scripts/maps-api.sh help` for the full command list.

Places endpoints use the **Places API (New)** at `places.googleapis.com/v1` — the legacy `/maps/api/place/*` endpoints are EOL. The Places API (New) must be enabled on the GCP project.

## Examples

```bash
./scripts/maps-api.sh geocode-pretty "Reeperbahn, Hamburg"
./scripts/maps-api.sh directions-pretty "Hamburg" "Berlin" driving
./scripts/maps-api.sh places-nearby-pretty "53.5488,9.9872" 500 restaurant
./scripts/maps-api.sh place-details-pretty "PLACE_ID"
./scripts/maps-api.sh weather-hourly-pretty 53.5495 9.9626 24
./scripts/maps-api.sh static-map "Hamburg, Germany" 13 600x400 "" /tmp/map.png
```

## Error handling

Commands validate responses and exit non-zero on real errors (printing the API message to stderr). `ZERO_RESULTS` is treated as success with an empty result. `static-map` verifies `image/*` content-type; a non-image payload (e.g. over-quota HTML) is a failure.

## Cost control

- `place-details` omits `reviews` by default (cheaper SKU). Set `MAPS_WITH_REVIEWS=1` to include up to 3 recent reviews.
- Every call is billed against the GCP project; avoid tight retry loops.

## Limits & truncation (pretty output)

- `reverse-geocode-pretty`: 3 results
- `places-search-pretty` / `places-nearby-pretty`: 10 results
- `place-details-pretty`: 3 reviews × 150 chars each
- `weather-hourly`: ≤ 240h (paginated internally)
- `weather-daily`: ≤ 10d (paginated internally)

Raw (non-`-pretty`) variants return the full response.

## Warnings

- Key must be server-side (no HTTP-referrer restriction) with Geocoding + Directions + Distance Matrix + Static + Places (New) + Weather enabled — opaque `REQUEST_DENIED` otherwise.
- `alternatives` is a magic 4th arg to `directions`; only the literal string `alternatives` enables it.
- `units` accepts only `METRIC` or `IMPERIAL`; other values return 400.
- `429` / `RESOURCE_EXHAUSTED` → back off; don't retry immediately.

## References

- `references/static-maps.md` — marker syntax, URL-length caveats
- `references/weather.md` — timezone handling, units, pagination
