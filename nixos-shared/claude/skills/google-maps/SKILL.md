---
name: google-maps
description: "Queries Google Maps Platform and Google Weather APIs for geocoding, reverse-geocoding, directions with travel times, distance matrix, places search, place details, static map images, and current/hourly/daily weather forecasts. Use when the user mentions an address or lat/lng coordinates; asks how to get somewhere, travel time, ETA, or a route (driving, walking, bicycling, transit); asks for nearby places, restaurants, cafes, bars, or points of interest (including \"near me\" or \"closest\"); wants a map image; or asks about weather, forecast, temperature, rain, or conditions at a location."
---

# Google Maps

## Script

`./scripts/maps-api.sh` wraps the Google Maps Platform APIs. API key stored in `pass api/claude-maps`.

Places endpoints use the **Places API (New)** at `places.googleapis.com/v1` — the legacy `/maps/api/place/*` endpoints are end-of-life. The Places API (New) must be enabled on the GCP project for the key.

```bash
# Quick reference
./scripts/maps-api.sh geocode-pretty "Reeperbahn, Hamburg"
./scripts/maps-api.sh reverse-geocode-pretty 53.5488 9.9872
./scripts/maps-api.sh directions-pretty "Hamburg" "Berlin" driving
./scripts/maps-api.sh directions-pretty "Hamburg Hbf" "Altona" transit
./scripts/maps-api.sh directions-waypoints-pretty "Hamburg" "Berlin" "Hannover" driving
./scripts/maps-api.sh distance-matrix-pretty "Hamburg|Berlin" "Munich|Frankfurt" driving
./scripts/maps-api.sh places-search-pretty "coffee shops" "53.5488,9.9872" 1000
./scripts/maps-api.sh places-nearby-pretty "53.5488,9.9872" 500 restaurant
./scripts/maps-api.sh place-details-pretty "PLACE_ID"
./scripts/maps-api.sh static-map "Hamburg, Germany" 13 600x400 "" /tmp/map.png
./scripts/maps-api.sh weather-current-pretty 53.5495 9.9626
./scripts/maps-api.sh weather-hourly-pretty 53.5495 9.9626 24
./scripts/maps-api.sh weather-daily-pretty 53.5495 9.9626 10
```

## Command Reference

| Command | Description |
|---------|-------------|
| `geocode <address>` | Address to lat/lng (raw JSON) |
| `geocode-pretty <address>` | Address to lat/lng (formatted) |
| `reverse-geocode <lat> <lng>` | Lat/lng to address (raw JSON) |
| `reverse-geocode-pretty <lat> <lng>` | Lat/lng to address (formatted) |
| `directions <origin> <dest> [mode] [alternatives]` | Route directions (raw JSON) |
| `directions-pretty <origin> <dest> [mode] [alternatives]` | Route directions (formatted) |
| `directions-waypoints <origin> <dest> <waypoints> [mode]` | Route with stops (raw JSON) |
| `directions-waypoints-pretty <origin> <dest> <waypoints> [mode]` | Route with stops (formatted) |
| `distance-matrix <origins> <dests> [mode]` | Travel time/distance matrix (raw JSON) |
| `distance-matrix-pretty <origins> <dests> [mode]` | Travel time/distance matrix (formatted) |
| `places-search <query> [lat,lng] [radius]` | Text search for places (raw JSON) |
| `places-search-pretty <query> [lat,lng] [radius]` | Text search (formatted) |
| `places-nearby <lat,lng> [radius] [type]` | Nearby search (raw JSON) |
| `places-nearby-pretty <lat,lng> [radius] [type]` | Nearby search (formatted) |
| `place-details <place_id>` | Full place details (raw JSON) |
| `place-details-pretty <place_id>` | Full place details (formatted) |
| `static-map <center> [zoom] [size] [markers] [outfile]` | Download map image (default: /tmp/map.png) |
| `weather-current <lat> <lng> [units]` | Current conditions (raw JSON) |
| `weather-current-pretty <lat> <lng> [units]` | Current conditions (formatted) |
| `weather-hourly <lat> <lng> [hours] [units]` | Hourly forecast, 1–240h (raw JSON) |
| `weather-hourly-pretty <lat> <lng> [hours] [units]` | Hourly forecast (formatted) |
| `weather-daily <lat> <lng> [days] [units]` | Daily forecast, 1–10d (raw JSON) |
| `weather-daily-pretty <lat> <lng> [days] [units]` | Daily forecast (formatted) |

## Modes

`driving` (default), `walking`, `bicycling`, `transit`

## Weather

Coordinates only (lat/lng) — geocode an address first if needed. Units: `METRIC` (default) or `IMPERIAL`. Hourly paginates internally up to 240h; daily up to 10d. All times in pretty output are converted to local time using the response's `timeZone.id` via `TZ=` + jq's `strflocaltime`.

## Multi-origin/destination

For distance-matrix, separate multiple origins/destinations with `|`:
```bash
./scripts/maps-api.sh distance-matrix-pretty "Hamburg|Berlin" "Munich|Cologne"
```

## Place Types (for nearby search)

Common types: `restaurant`, `cafe`, `bar`, `bakery`, `supermarket`, `pharmacy`, `hospital`, `gas_station`, `parking`, `gym`, `park`, `museum`, `hotel`, `airport`, `train_station`, `bus_station`

## Static Map Markers

Marker spec: `color:red|label:A|Hamburg`. Multiple marker groups: separate with `;`:
```bash
./scripts/maps-api.sh static-map "Germany" 6 800x600 \
  "color:red|label:A|Hamburg;color:blue|label:B|Berlin" /tmp/map.png
```

## Error handling

All commands validate the API response and exit non-zero on real errors (printing the API's error message to stderr). `ZERO_RESULTS` is treated as success with an empty result. `static-map` verifies the response is `image/*`; a non-image payload (e.g. over-quota error page) is treated as failure.

## Cost control

- `place-details` omits `reviews` by default (cheaper SKU). Set `MAPS_WITH_REVIEWS=1` to include up to 3 recent reviews.
- Every call is billed against the GCP project; avoid tight retry loops.

## Workflows

### "How do I get from A to B?"
1. `directions-pretty "A" "B" [mode]` — get route with steps and duration
2. Optionally pass `alternatives` as 4th arg to get multiple routes

### "Find X near Y"
1. `geocode "Y"` to get lat/lng (if not already known)
2. `places-search-pretty "X" "lat,lng" radius` or `places-nearby-pretty "lat,lng" radius type`
3. `place-details-pretty PLACE_ID` for more info on a specific result

### "How far is it between multiple places?"
1. `distance-matrix-pretty "A|B" "C|D" mode` — get all pairwise distances/durations

### "What's the weather in X?"
1. `geocode "X"` to get lat/lng
2. `weather-current-pretty LAT LNG` for now, `weather-hourly-pretty LAT LNG N` for next N hours, or `weather-daily-pretty LAT LNG N` for next N days

**Script Execution:** Scripts should be executed from the skill directory. All scripts use Nix shebangs so no manual dependency installation is required.
