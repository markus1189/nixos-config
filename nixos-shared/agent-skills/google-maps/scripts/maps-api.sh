#!/usr/bin/env nix
#! nix shell nixpkgs#bash nixpkgs#curl nixpkgs#jq nixpkgs#coreutils --command bash
# shellcheck shell=bash
set -euo pipefail

readonly BASE="https://maps.googleapis.com/maps/api"
readonly PLACES_BASE="https://places.googleapis.com/v1"
readonly ROUTES_BASE="https://routes.googleapis.com"
readonly WEATHER_BASE="https://weather.googleapis.com/v1"

# Places API (New) field masks. Required — absent mask = billed-at-full-tier response.
# Default details mask excludes `reviews` (Atmosphere SKU, most expensive tier).
# Set MAPS_WITH_REVIEWS=1 to include reviews.
readonly PLACES_SEARCH_MASK="places.id,places.displayName,places.formattedAddress,places.location,places.rating,places.userRatingCount,places.priceLevel,places.regularOpeningHours.openNow"
readonly PLACE_DETAILS_MASK_BASE="id,displayName,formattedAddress,internationalPhoneNumber,websiteUri,googleMapsUri,regularOpeningHours,rating,userRatingCount,priceLevel,location,types"

# Routes API field masks. All requested fields are in the "Basic" SKU tier.
readonly ROUTES_FIELD_MASK="routes.description,routes.distanceMeters,routes.duration,routes.staticDuration,routes.polyline.encodedPolyline,routes.localizedValues,routes.legs.distanceMeters,routes.legs.duration,routes.legs.staticDuration,routes.legs.localizedValues,routes.legs.steps.navigationInstruction.instructions,routes.legs.steps.distanceMeters,routes.legs.steps.staticDuration,routes.legs.steps.localizedValues"
readonly ROUTE_MATRIX_FIELD_MASK="originIndex,destinationIndex,distanceMeters,duration,condition,localizedValues"

place_details_mask() {
    if [ "${MAPS_WITH_REVIEWS:-0}" = "1" ]; then
        printf '%s,reviews' "$PLACE_DETAILS_MASK_BASE"
    else
        printf '%s' "$PLACE_DETAILS_MASK_BASE"
    fi
}

# API key is loaded lazily so `help` / `--help` don't trigger a GPG prompt.
require_api_key() {
    if [ -z "${API_KEY:-}" ]; then
        API_KEY="$(pass api/claude-maps)"
        readonly API_KEY
    fi
}

# Emit an ISO-8601 UTC timestamp for the DRIVE `departureTime` field.
# Default: now + 5 seconds (API requires a future timestamp).
# Override with MAPS_DEPARTURE_TIME — any `date -d` compatible string,
# e.g. "2026-04-21 17:45 CEST", "today 18:00", "2026-04-21T17:45:00+02:00".
_departure_time_iso() {
    if [ -n "${MAPS_DEPARTURE_TIME:-}" ]; then
        if ! date -u -d "$MAPS_DEPARTURE_TIME" +"%Y-%m-%dT%H:%M:%SZ" 2>/dev/null; then
            echo "Invalid MAPS_DEPARTURE_TIME: $MAPS_DEPARTURE_TIME" >&2
            return 1
        fi
    else
        date -u -d '+5 seconds' +"%Y-%m-%dT%H:%M:%SZ"
    fi
}

# --- Helpers ---

urlencode() {
    jq -rn --arg v "$1" '$v|@uri'
}

# Map legacy mode names (driving/walking/bicycling/transit) to Routes API travelMode enum.
travel_mode_enum() {
    case "$1" in
        driving)   printf 'DRIVE' ;;
        walking)   printf 'WALK' ;;
        bicycling) printf 'BICYCLE' ;;
        transit)   printf 'TRANSIT' ;;
        *) echo "Unknown travel mode: $1 (expected driving|walking|bicycling|transit)" >&2; return 1 ;;
    esac
}

# Core HTTP wrapper used by all JSON endpoints. Distinguishes transport errors,
# HTTP 4xx/5xx (body captured via --fail-with-body), and non-JSON responses
# (e.g. HTML 503 pages that would otherwise crash downstream jq).
_curl_json() {
    local body rc=0
    body=$(curl -sS --fail-with-body "$@") || rc=$?
    if [ "$rc" -ne 0 ]; then
        local err=""
        if [ -n "$body" ]; then
            err=$(jq -r '.error.message // .error_message // empty' <<<"$body" 2>/dev/null || true)
        fi
        if [ -n "$err" ]; then
            echo "API error: $err" >&2
        else
            echo "API error (curl exit $rc): ${body:0:200}" >&2
        fi
        return 1
    fi
    if ! jq -e . <<<"$body" >/dev/null 2>&1; then
        echo "API error: non-JSON response" >&2
        printf '%s\n' "${body:0:200}" >&2
        return 1
    fi
    printf '%s\n' "$body"
}

api_get() {
    _curl_json "$1"
}

# Validate a legacy Maps Platform JSON response — catches HTTP-200-but-status=REQUEST_DENIED.
# ZERO_RESULTS is allowed so callers can render an empty result set.
check_status() {
    local body="$1"
    local status
    status=$(jq -r '.status // empty' <<<"$body")
    case "$status" in
        ""|OK|ZERO_RESULTS) return 0 ;;
    esac
    local msg
    msg=$(jq -r '.error_message // ""' <<<"$body")
    echo "API error: $status${msg:+ — $msg}" >&2
    return 1
}

# Backup check for Places API (New); most errors already caught by _curl_json's --fail-with-body.
check_places_status() {
    local body="$1"
    local err
    err=$(jq -r '.error.message // empty' <<<"$body")
    if [ -n "$err" ]; then
        local code
        code=$(jq -r '.error.status // .error.code // ""' <<<"$body")
        echo "Places API error: ${code:+$code — }$err" >&2
        return 1
    fi
}

# --- Geocoding ---

geocode() {
    local address body
    address=$(urlencode "$1")
    body=$(api_get "${BASE}/geocode/json?address=${address}&key=${API_KEY}")
    check_status "$body"
    printf '%s\n' "$body"
}

geocode_pretty() {
    geocode "$1" | jq -r '.results[] | "\(.formatted_address)\n  lat: \(.geometry.location.lat), lng: \(.geometry.location.lng)\n  place_id: \(.place_id)\n"'
}

reverse_geocode() {
    local lat="$1" lng="$2" body
    body=$(api_get "${BASE}/geocode/json?latlng=${lat},${lng}&key=${API_KEY}")
    check_status "$body"
    printf '%s\n' "$body"
}

reverse_geocode_pretty() {
    reverse_geocode "$1" "$2" | jq -r '.results[:3][] | "\(.formatted_address)\n  types: \(.types | join(", "))\n"'
}

# --- Directions (Routes API v2) ---
# Legacy /maps/api/directions + /distancematrix became end-of-life 2025-03-01.
# All routing now uses routes.googleapis.com with POST + X-Goog-FieldMask.
# DRIVE travel mode gets TRAFFIC_AWARE routing + current departure time, so
# `duration` reflects live traffic and `staticDuration` is the baseline.
# Non-DRIVE modes must not set routingPreference or departureTime.

directions() {
    local origin="$1" destination="$2" mode="${3:-driving}" alternatives="${4:-}"
    local travel_mode body response alt_flag=false dep
    travel_mode=$(travel_mode_enum "$mode") || return 1
    [ "$alternatives" = "alternatives" ] && alt_flag=true
    dep=$(_departure_time_iso) || return 1
    body=$(jq -nc \
        --arg o "$origin" \
        --arg d "$destination" \
        --arg m "$travel_mode" \
        --arg dep "$dep" \
        --argjson alt "$alt_flag" \
        '{origin: {address: $o}, destination: {address: $d}, travelMode: $m, computeAlternativeRoutes: $alt} +
         (if $m == "DRIVE" then {routingPreference: "TRAFFIC_AWARE", departureTime: $dep} else {} end)')
    response=$(_curl_json -X POST "${ROUTES_BASE}/directions/v2:computeRoutes" \
        -H "Content-Type: application/json" \
        -H "X-Goog-Api-Key: ${API_KEY}" \
        -H "X-Goog-FieldMask: ${ROUTES_FIELD_MASK}" \
        -d "$body")
    check_places_status "$response"
    printf '%s\n' "$response"
}

# Render Routes API v2 response. `duration` is traffic-aware for DRIVE with
# TRAFFIC_AWARE; `staticDuration` is the free-flow baseline. Shown only when
# they differ. Step `navigationInstruction.instructions` is plain text — no
# HTML-strip needed.
format_routes() {
    jq -r '
        .routes[] |
        ([.legs[].localizedValues.distance.text] | join(" + ")) as $dist |
        ([.legs[].localizedValues.duration.text] | join(" + ")) as $dur |
        (.localizedValues.duration.text // "") as $rdur |
        (.localizedValues.staticDuration.text // "") as $sdur |
        "Route: \(.description // "unnamed")  [\($dist), \($dur)]" +
        (if $sdur != "" and $sdur != $rdur then "  (no traffic: \($sdur))" else "" end) +
        "\n" +
        ([.legs[].steps[]? |
            "  \(.navigationInstruction.instructions // "(no instruction)")  " +
            "[\(.localizedValues.distance.text // "?"), \(.localizedValues.staticDuration.text // "?")]"
        ] | join("\n")) +
        "\n"
    '
}

directions_pretty() {
    directions "$@" | format_routes
}

# --- Directions with waypoints ---

directions_waypoints() {
    local origin="$1" destination="$2" waypoints_raw="$3" mode="${4:-driving}"
    local travel_mode body response dep
    travel_mode=$(travel_mode_enum "$mode") || return 1
    dep=$(_departure_time_iso) || return 1
    body=$(jq -nc \
        --arg o "$origin" \
        --arg d "$destination" \
        --arg w "$waypoints_raw" \
        --arg m "$travel_mode" \
        --arg dep "$dep" \
        '{
            origin: {address: $o},
            destination: {address: $d},
            intermediates: ($w | split("|") | map(select(. != "") | {address: .})),
            travelMode: $m
         } +
         (if $m == "DRIVE" then {routingPreference: "TRAFFIC_AWARE", departureTime: $dep} else {} end)')
    response=$(_curl_json -X POST "${ROUTES_BASE}/directions/v2:computeRoutes" \
        -H "Content-Type: application/json" \
        -H "X-Goog-Api-Key: ${API_KEY}" \
        -H "X-Goog-FieldMask: ${ROUTES_FIELD_MASK}" \
        -d "$body")
    check_places_status "$response"
    printf '%s\n' "$response"
}

directions_waypoints_pretty() {
    directions_waypoints "$@" | format_routes
}

# --- Distance Matrix (Routes API v2) ---
# Returns a JSON array of elements keyed by originIndex/destinationIndex.
# No labels are echoed back, so pretty-output must be passed the input lists.

distance_matrix() {
    local origins_raw="$1" destinations_raw="$2" mode="${3:-driving}"
    local travel_mode body response dep
    travel_mode=$(travel_mode_enum "$mode") || return 1
    dep=$(_departure_time_iso) || return 1
    body=$(jq -nc \
        --arg origins "$origins_raw" \
        --arg dests "$destinations_raw" \
        --arg m "$travel_mode" \
        --arg dep "$dep" \
        '{
            origins: ($origins | split("|") | map({waypoint: {address: .}})),
            destinations: ($dests | split("|") | map({waypoint: {address: .}})),
            travelMode: $m
         } +
         (if $m == "DRIVE" then {routingPreference: "TRAFFIC_AWARE", departureTime: $dep} else {} end)')
    response=$(_curl_json -X POST "${ROUTES_BASE}/distanceMatrix/v2:computeRouteMatrix" \
        -H "Content-Type: application/json" \
        -H "X-Goog-Api-Key: ${API_KEY}" \
        -H "X-Goog-FieldMask: ${ROUTE_MATRIX_FIELD_MASK}" \
        -d "$body")
    printf '%s\n' "$response"
}

distance_matrix_pretty() {
    local origins_raw="$1" destinations_raw="$2" mode="${3:-driving}"
    local response
    response=$(distance_matrix "$origins_raw" "$destinations_raw" "$mode")
    jq -r \
        --arg origins "$origins_raw" \
        --arg dests "$destinations_raw" \
        '
        ($origins | split("|")) as $oarr |
        ($dests   | split("|")) as $darr |
        .[] |
        if .condition == "ROUTE_EXISTS" then
            "\($oarr[.originIndex]) → \($darr[.destinationIndex]): \(.localizedValues.distance.text // "?"), \(.localizedValues.duration.text // "?")"
        else
            "\($oarr[.originIndex]) → \($darr[.destinationIndex]): \(.condition // "UNKNOWN")"
        end
        ' <<<"$response"
}

# --- Places API (New) ---
# Note: legacy /place/textsearch, /place/nearbysearch, /place/details are end-of-life.
# These use the v1 endpoints with field masks (required, controls billing tier).

places_search() {
    local query="$1" location="${2:-}" radius="${3:-5000}" body response
    body=$(jq -nc \
        --arg q "$query" \
        --arg loc "$location" \
        --argjson r "$radius" \
        '{textQuery: $q} +
         (if $loc != "" then
            ($loc | split(",") | {
                locationBias: {
                    circle: {
                        center: {latitude: (.[0]|tonumber), longitude: (.[1]|tonumber)},
                        radius: $r
                    }
                }
            })
          else {} end)')
    response=$(_curl_json -X POST "${PLACES_BASE}/places:searchText" \
        -H "Content-Type: application/json" \
        -H "X-Goog-Api-Key: ${API_KEY}" \
        -H "X-Goog-FieldMask: ${PLACES_SEARCH_MASK}" \
        -d "$body")
    check_places_status "$response"
    printf '%s\n' "$response"
}

# Shared jq snippets for Places (New) pretty output.
read -r -d '' PLACES_PRICE_DEF <<'JQ' || true
def price: {"PRICE_LEVEL_FREE":"free","PRICE_LEVEL_INEXPENSIVE":"$","PRICE_LEVEL_MODERATE":"$$","PRICE_LEVEL_EXPENSIVE":"$$$","PRICE_LEVEL_VERY_EXPENSIVE":"$$$$"}[.] // .;
JQ

places_search_pretty() {
    places_search "$@" | jq -r "${PLACES_PRICE_DEF}"'
        .places[:10][]? |
        "\(.displayName.text)  [\(.rating // "n/a")★, \(.userRatingCount // 0) reviews]" +
        "\n  \(.formattedAddress)" +
        (if .regularOpeningHours.openNow != null then "\n  Open now: \(.regularOpeningHours.openNow)" else "" end) +
        (if .priceLevel then "\n  Price: \(.priceLevel | price)" else "" end) +
        "\n  place_id: \(.id)\n"'
}

places_nearby() {
    local location="$1" radius="${2:-1000}" type="${3:-}" body response
    body=$(jq -nc \
        --arg loc "$location" \
        --argjson r "$radius" \
        --arg t "$type" \
        '($loc | split(",")) as $ll |
         {
            locationRestriction: {
                circle: {
                    center: {latitude: ($ll[0]|tonumber), longitude: ($ll[1]|tonumber)},
                    radius: $r
                }
            }
         } +
         (if $t != "" then {includedTypes: [$t]} else {} end)')
    response=$(_curl_json -X POST "${PLACES_BASE}/places:searchNearby" \
        -H "Content-Type: application/json" \
        -H "X-Goog-Api-Key: ${API_KEY}" \
        -H "X-Goog-FieldMask: ${PLACES_SEARCH_MASK}" \
        -d "$body")
    check_places_status "$response"
    printf '%s\n' "$response"
}

places_nearby_pretty() {
    places_nearby "$@" | jq -r "${PLACES_PRICE_DEF}"'
        .places[:10][]? |
        "\(.displayName.text)  [\(.rating // "n/a")★, \(.userRatingCount // 0) reviews]" +
        "\n  \(.formattedAddress)" +
        (if .regularOpeningHours.openNow != null then "\n  Open now: \(.regularOpeningHours.openNow)" else "" end) +
        (if .priceLevel then "\n  Price: \(.priceLevel | price)" else "" end) +
        "\n  place_id: \(.id)\n"'
}

place_details() {
    local place_id response mask
    place_id=$(urlencode "$1")
    mask=$(place_details_mask)
    response=$(_curl_json "${PLACES_BASE}/places/${place_id}" \
        -H "X-Goog-Api-Key: ${API_KEY}" \
        -H "X-Goog-FieldMask: ${mask}")
    check_places_status "$response"
    printf '%s\n' "$response"
}

place_details_pretty() {
    place_details "$1" | jq -r "${PLACES_PRICE_DEF}"'
        "\(.displayName.text)\n  \(.formattedAddress)" +
        (if .internationalPhoneNumber then "\n  Phone: \(.internationalPhoneNumber)" else "" end) +
        (if .websiteUri then "\n  Web: \(.websiteUri)" else "" end) +
        (if .googleMapsUri then "\n  Maps: \(.googleMapsUri)" else "" end) +
        "\n  Rating: \(.rating // "n/a")★ (\(.userRatingCount // 0) reviews)" +
        (if .priceLevel then "\n  Price: \(.priceLevel | price)" else "" end) +
        (if .regularOpeningHours.weekdayDescriptions then "\n  Hours:\n" + (.regularOpeningHours.weekdayDescriptions | map("    \(.)") | join("\n")) else "" end) +
        (if .reviews then "\n  Recent reviews:" + (.reviews[:3] | map("\n    \(.rating)★ \(.authorAttribution.displayName // "anon"): \((.text.text // "(no text)")[:150])...") | join("")) else "" end)'
}

# --- Weather API ---
# Uses Google Weather API v1. Same v1 error shape as Places (New) — reuse check_places_status.
# Coordinates are passed as separate query params (location.latitude, location.longitude).
# Default units: METRIC.

weather_current() {
    local lat="$1" lng="$2" units="${3:-METRIC}" response
    response=$(api_get "${WEATHER_BASE}/currentConditions:lookup?key=${API_KEY}&location.latitude=${lat}&location.longitude=${lng}&unitsSystem=${units}")
    check_places_status "$response"
    printf '%s\n' "$response"
}

# Shared jq helpers for weather pretty output.
read -r -d '' WEATHER_HELPERS <<'JQ' || true
def pad: tostring | if length == 1 then "0" + . else . end;
def temp: if . == null then "n/a" else "\(.degrees | round)°\(if .unit == "FAHRENHEIT" then "F" else "C" end)" end;
def speed: if . == null then "n/a" else "\(.value | round) \(if .unit == "MILES_PER_HOUR" then "mph" else "km/h" end)" end;
def hr: if . == null then "??:??" else "\(.hours // 0 | pad):\(.minutes // 0 | pad)" end;
# Convert a UTC ISO timestamp to local HH:MM via jq's strflocaltime (honors $TZ env var,
# which the caller sets from the response's timeZone.id).
def local_hm: if . == null then "??:??" else sub("\\.[0-9]+Z$"; "Z") | sub("Z$"; "") | strptime("%Y-%m-%dT%H:%M:%S") | mktime | strflocaltime("%H:%M") end;
def local_dt: if . == null then "?" else sub("\\.[0-9]+Z$"; "Z") | sub("Z$"; "") | strptime("%Y-%m-%dT%H:%M:%S") | mktime | strflocaltime("%Y-%m-%d %H:%M") end;
def qpf_unit: {"MILLIMETERS":"mm","INCHES":"in"}[.] // (. | ascii_downcase);
JQ

weather_current_pretty() {
    local data tz
    data=$(weather_current "$@")
    tz=$(jq -r '.timeZone.id // "UTC"' <<<"$data")
    TZ="$tz" jq -r "${WEATHER_HELPERS}"'
        "Current weather \(.currentTime | local_dt)  (\(.timeZone.id // "UTC"))" +
        "\n  \(.weatherCondition.description.text // "Unknown")  \(.temperature | temp) (feels \(.feelsLikeTemperature | temp))" +
        "\n  Humidity: \(.relativeHumidity)%   UV: \(.uvIndex // "n/a")   Pressure: \(.airPressure.meanSeaLevelMillibars | round) mb" +
        "\n  Wind: \(.wind.direction.cardinal // "?") \(.wind.speed | speed)" +
        (if .wind.gust then " (gusts \(.wind.gust | speed))" else "" end) +
        "\n  Precipitation: \(.precipitation.probability.percent)% \(.precipitation.probability.type | ascii_downcase)" +
        (if (.precipitation.qpf.quantity // 0) > 0 then " (\(.precipitation.qpf.quantity) \(.precipitation.qpf.unit | qpf_unit))" else "" end) +
        "\n  Cloud cover: \(.cloudCover // 0)%   Visibility: \(.visibility.distance // "?") \(.visibility.unit // "" | ascii_downcase)"
    ' <<<"$data"
}

# Generic paginator for the v1 forecast endpoints. Accumulates `forecastHours` or `forecastDays`
# arrays across pages until the requested count is reached or the API stops returning a token.
_weather_forecast() {
    local endpoint="$1" array_field="$2" count_param="$3" count="$4" page_size="$5"
    local lat="$6" lng="$7" units="${8:-METRIC}"
    local pages_needed=$(( (count + page_size - 1) / page_size ))
    local accumulated="{\"${array_field}\": []}"
    local token='' pages=0 response url
    while [ "$pages" -lt "$pages_needed" ]; do
        url="${WEATHER_BASE}/${endpoint}?key=${API_KEY}&location.latitude=${lat}&location.longitude=${lng}&unitsSystem=${units}&${count_param}=${count}&pageSize=${page_size}"
        if [ -n "$token" ]; then
            url+="&pageToken=$(urlencode "$token")"
        fi
        response=$(api_get "$url")
        check_places_status "$response"
        accumulated=$(jq -c --argjson acc "$accumulated" --arg field "$array_field" '
            ($acc[$field] + .[$field]) as $merged |
            . + {($field): $merged}
        ' <<<"$response")
        token=$(jq -r '.nextPageToken // empty' <<<"$response")
        pages=$((pages + 1))
        [ -z "$token" ] && break
    done
    jq -c --arg field "$array_field" --argjson n "$count" '.[$field] |= .[:$n]' <<<"$accumulated"
}

weather_hourly() {
    local lat="$1" lng="$2" hours="${3:-24}" units="${4:-METRIC}"
    _weather_forecast "forecast/hours:lookup" "forecastHours" "hours" "$hours" 24 "$lat" "$lng" "$units"
}

weather_hourly_pretty() {
    local data tz
    data=$(weather_hourly "$@")
    tz=$(jq -r '.timeZone.id // "UTC"' <<<"$data")
    TZ="$tz" jq -r "${WEATHER_HELPERS}"'
        "Hourly forecast  (\(.timeZone.id // "UTC"))",
        (.forecastHours[] |
            "  \(.displayDateTime | hr)  " +
            "\(.weatherCondition.description.text // "?")  " +
            "\(.temperature | temp)  " +
            "Wind \(.wind.direction.cardinal // "?") \(.wind.speed | speed)  " +
            "Rain \(.precipitation.probability.percent // 0)%" +
            (if (.precipitation.qpf.quantity // 0) > 0 then " (\(.precipitation.qpf.quantity)\(.precipitation.qpf.unit | qpf_unit))" else "" end)
        )
    ' <<<"$data"
}

weather_daily() {
    local lat="$1" lng="$2" days="${3:-5}" units="${4:-METRIC}"
    _weather_forecast "forecast/days:lookup" "forecastDays" "days" "$days" 5 "$lat" "$lng" "$units"
}

weather_daily_pretty() {
    local data tz
    data=$(weather_daily "$@")
    tz=$(jq -r '.timeZone.id // "UTC"' <<<"$data")
    TZ="$tz" jq -r "${WEATHER_HELPERS}"'
        "Daily forecast  (\(.timeZone.id // "UTC"))",
        (.forecastDays[] |
            "  \(.displayDate.year)-\(.displayDate.month | pad)-\(.displayDate.day | pad)  " +
            "\(.daytimeForecast.weatherCondition.description.text // "?")  " +
            "\(.minTemperature | temp)–\(.maxTemperature | temp)  " +
            "Sun \(.sunEvents.sunriseTime | local_hm)–\(.sunEvents.sunsetTime | local_hm)  " +
            "Rain \(.daytimeForecast.precipitation.probability.percent // 0)%"
        )
    ' <<<"$data"
}

# --- Static Maps ---

static_map() {
    local center="$1" zoom="${2:-13}" size="${3:-600x400}" markers="${4:-}" outfile="${5:-/tmp/map.png}"
    local encoded_center url
    encoded_center=$(urlencode "$center")
    url="${BASE}/staticmap?center=${encoded_center}&zoom=${zoom}&size=${size}&key=${API_KEY}"
    if [ -n "$markers" ]; then
        local marker
        local -a marker_arr
        IFS=';' read -r -a marker_arr <<<"$markers"
        for marker in "${marker_arr[@]}"; do
            url+="&markers=$(urlencode "$marker")"
        done
    fi
    local curl_out http_code content_type rc=0
    curl_out=$(curl -sS -o "$outfile" -w '%{http_code}|%{content_type}' "$url") || rc=$?
    if [ "$rc" -ne 0 ]; then
        echo "static-map: curl failed (exit $rc)" >&2
        rm -f "$outfile"
        return 1
    fi
    http_code="${curl_out%%|*}"
    content_type="${curl_out#*|}"
    if [ "$http_code" -ge 400 ]; then
        echo "static-map: HTTP $http_code" >&2
        [ -s "$outfile" ] && head -c 300 "$outfile" >&2 && echo >&2
        rm -f "$outfile"
        return 1
    fi
    case "$content_type" in
        image/*) ;;
        *)
            echo "static-map: expected image, got '${content_type}' (likely an API error)" >&2
            [ -s "$outfile" ] && head -c 300 "$outfile" >&2 && echo >&2
            rm -f "$outfile"
            return 1 ;;
    esac
    echo "$outfile"
}

# --- Main dispatch ---

usage() {
    cat <<'EOF'
Usage: maps-api.sh <command> [args...]

Geocoding:
  geocode <address>                          Geocode an address (raw JSON)
  geocode-pretty <address>                   Geocode (formatted)
  reverse-geocode <lat> <lng>                Reverse geocode (raw JSON)
  reverse-geocode-pretty <lat> <lng>         Reverse geocode (formatted)

Directions:
  directions <origin> <dest> [mode] [alternatives]   Get directions (raw JSON)
  directions-pretty <origin> <dest> [mode] [alternatives]
                                                     Get directions (formatted)
  directions-waypoints <orig> <dest> <waypoints> [mode]
                                                     Directions with waypoints (raw JSON)
  directions-waypoints-pretty <orig> <dest> <waypoints> [mode]
                                                     Directions with waypoints (formatted)

Distance Matrix:
  distance-matrix <origins> <destinations> [mode]
  distance-matrix-pretty <origins> <destinations> [mode]
    Origins/destinations: pipe-separated for multiple (A|B|C)

Places (Places API New):
  places-search <query> [lat,lng] [radius]           Text search
  places-search-pretty <query> [lat,lng] [radius]    Text search (formatted)
  places-nearby <lat,lng> [radius] [type]            Nearby search
  places-nearby-pretty <lat,lng> [radius] [type]     Nearby search (formatted)
  place-details <place_id>                           Place details (raw JSON)
  place-details-pretty <place_id>                    Place details (formatted)

Static Maps:
  static-map <center> [zoom] [size] [markers] [outfile]
    Downloads a static map image. Default outfile: /tmp/map.png
    Multiple marker groups: separate with ';' (e.g. "color:red|Hamburg;color:blue|Berlin")

Weather:
  weather-current <lat> <lng> [units]               Current conditions (raw JSON)
  weather-current-pretty <lat> <lng> [units]        Current conditions (formatted)
  weather-hourly <lat> <lng> [hours] [units]        Hourly forecast, 1-240h (raw JSON)
  weather-hourly-pretty <lat> <lng> [hours] [units] Hourly forecast (formatted)
  weather-daily <lat> <lng> [days] [units]          Daily forecast, 1-10d (raw JSON)
  weather-daily-pretty <lat> <lng> [days] [units]   Daily forecast (formatted)
    Units: METRIC (default), IMPERIAL

Modes: driving (default), walking, bicycling, transit
EOF
}

cmd="${1:-}"
case "$cmd" in
    help|--help|-h|"") usage; exit 0 ;;
esac

require_api_key

case "$cmd" in
    geocode)                       shift; geocode "$@" ;;
    geocode-pretty)                shift; geocode_pretty "$@" ;;
    reverse-geocode)               shift; reverse_geocode "$@" ;;
    reverse-geocode-pretty)        shift; reverse_geocode_pretty "$@" ;;
    directions)                    shift; directions "$@" ;;
    directions-pretty)             shift; directions_pretty "$@" ;;
    directions-waypoints)          shift; directions_waypoints "$@" ;;
    directions-waypoints-pretty)   shift; directions_waypoints_pretty "$@" ;;
    distance-matrix)               shift; distance_matrix "$@" ;;
    distance-matrix-pretty)        shift; distance_matrix_pretty "$@" ;;
    places-search)                 shift; places_search "$@" ;;
    places-search-pretty)          shift; places_search_pretty "$@" ;;
    places-nearby)                 shift; places_nearby "$@" ;;
    places-nearby-pretty)          shift; places_nearby_pretty "$@" ;;
    place-details)                 shift; place_details "$@" ;;
    place-details-pretty)          shift; place_details_pretty "$@" ;;
    static-map)                    shift; static_map "$@" ;;
    weather-current)               shift; weather_current "$@" ;;
    weather-current-pretty)        shift; weather_current_pretty "$@" ;;
    weather-hourly)                shift; weather_hourly "$@" ;;
    weather-hourly-pretty)         shift; weather_hourly_pretty "$@" ;;
    weather-daily)                 shift; weather_daily "$@" ;;
    weather-daily-pretty)          shift; weather_daily_pretty "$@" ;;
    *)                             echo "Unknown command: $cmd" >&2; usage; exit 1 ;;
esac
