#!/usr/bin/env nix
#! nix shell nixpkgs#bash nixpkgs#curl nixpkgs#jq --command bash
# shellcheck shell=bash
set -euo pipefail

API_KEY="$(pass api/claude-maps)"
readonly API_KEY
readonly BASE="https://maps.googleapis.com/maps/api"
readonly PLACES_BASE="https://places.googleapis.com/v1"
readonly WEATHER_BASE="https://weather.googleapis.com/v1"

# Field masks for Places API (New). Required — without them the API returns SKU-billed
# full results and rejects pretty parsers expecting a slim shape.
readonly PLACES_SEARCH_MASK="places.id,places.displayName,places.formattedAddress,places.rating,places.userRatingCount,places.priceLevel,places.regularOpeningHours.openNow"
readonly PLACE_DETAILS_MASK="id,displayName,formattedAddress,internationalPhoneNumber,websiteUri,googleMapsUri,regularOpeningHours,rating,userRatingCount,priceLevel,reviews,location,types"

# --- Helpers ---

urlencode() {
    jq -rn --arg v "$1" '$v|@uri'
}

api_get() {
    curl -sS "$1"
}

# Validate a legacy Maps Platform JSON response. Exits non-zero on real errors;
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

# Validate a Places API (New) JSON response (HTTP errors come back as a JSON .error object).
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

# --- Directions ---

directions() {
    local origin destination mode extra body
    origin=$(urlencode "$1")
    destination=$(urlencode "$2")
    mode="${3:-driving}"
    extra=""
    if [ "${4:-}" = "alternatives" ]; then
        extra="&alternatives=true"
    fi
    body=$(api_get "${BASE}/directions/json?origin=${origin}&destination=${destination}&mode=${mode}${extra}&key=${API_KEY}")
    check_status "$body"
    printf '%s\n' "$body"
}

format_routes() {
    jq -r '
        .routes[] |
        ([.legs[].distance.text] | join(" + ")) as $dist |
        ([.legs[].duration.text] | join(" + ")) as $dur |
        "Route: \(.summary // "unnamed")  [\($dist), \($dur)]" +
        (if .legs[0].duration_in_traffic then "  (in traffic: \(.legs[0].duration_in_traffic.text))" else "" end) +
        "\n" +
        ([.legs[].steps[] | "  \(.html_instructions | gsub("<[^>]*>"; ""))  [\(.distance.text), \(.duration.text)]"] | join("\n")) +
        "\n"
    '
}

directions_pretty() {
    local origin="$1" destination="$2" mode="${3:-driving}"
    directions "$origin" "$destination" "$mode" "${4:-}" | format_routes
}

# --- Directions with waypoints ---

directions_waypoints() {
    local origin destination waypoints mode body
    origin=$(urlencode "$1")
    destination=$(urlencode "$2")
    waypoints=$(urlencode "$3")
    mode="${4:-driving}"
    body=$(api_get "${BASE}/directions/json?origin=${origin}&destination=${destination}&waypoints=${waypoints}&mode=${mode}&key=${API_KEY}")
    check_status "$body"
    printf '%s\n' "$body"
}

directions_waypoints_pretty() {
    directions_waypoints "$@" | format_routes
}

# --- Distance Matrix ---

distance_matrix() {
    local origins destinations mode body
    origins=$(urlencode "$1")
    destinations=$(urlencode "$2")
    mode="${3:-driving}"
    body=$(api_get "${BASE}/distancematrix/json?origins=${origins}&destinations=${destinations}&mode=${mode}&key=${API_KEY}")
    check_status "$body"
    printf '%s\n' "$body"
}

distance_matrix_pretty() {
    distance_matrix "$1" "$2" "${3:-driving}" | jq -r '
        . as $root |
        range(.rows | length) as $i |
        range(.rows[$i].elements | length) as $j |
        "\($root.origin_addresses[$i]) → \($root.destination_addresses[$j]): \(.rows[$i].elements[$j].distance.text), \(.rows[$i].elements[$j].duration.text)"'
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
    response=$(curl -sS -X POST "${PLACES_BASE}/places:searchText" \
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
    response=$(curl -sS -X POST "${PLACES_BASE}/places:searchNearby" \
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
    local place_id="$1" response
    response=$(curl -sS "${PLACES_BASE}/places/${place_id}" \
        -H "X-Goog-Api-Key: ${API_KEY}" \
        -H "X-Goog-FieldMask: ${PLACE_DETAILS_MASK}")
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
    curl -sS -o "$outfile" "$url"
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

case "${1:-}" in
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
    help|--help|-h|"")             usage ;;
    *)                             echo "Unknown command: $1" >&2; usage; exit 1 ;;
esac
