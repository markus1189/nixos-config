#!/usr/bin/env nix
#! nix shell nixpkgs#bash nixpkgs#coreutils nixpkgs#curl nixpkgs#jq nixpkgs#util-linux --command bash
set -euo pipefail

# Query the Lieferando / Just Eat Takeaway discovery API for a location.
#
# The API's own `textSearch` parameter is IGNORED server-side (verified: a nonsense
# term returns the identical result set), so it returns every restaurant delivering
# to the given coordinates and QUERY filtering happens locally, in jq.

readonly VALID_COUNTRIES="de at"
readonly VALID_SORTS="rating eta distance min name"

LAT=""
LON=""
COUNTRY=de
SERVICE=delivery
SORT=rating
OPEN_ONLY=0
FORMAT=table
QUERY=""
QUERY_SET=0

usage() {
  cat <<'EOF'
lieferando.sh — list restaurants delivering to a location, with ratings, fees and deals.

USAGE
  ./scripts/lieferando.sh --lat FLOAT --lon FLOAT [QUERY] [options]

  QUERY   Optional. Filters by restaurant name OR cuisine tag; case- and
          umlaut-insensitive substring match ("doner" matches "Döner").
          Omit it to list everything. This filtering is done locally.

OPTIONS
  --lat FLOAT         Latitude.  REQUIRED, no default.
  --lon FLOAT         Longitude. REQUIRED, no default.
  --country CC        de | at   (default: de). MUST match the coordinates —
                      a German lat/lon under --country at returns nothing.
  --collection        Pickup instead of delivery (default: delivery).
  --open              Only places open right now.
  --sort FIELD        rating | eta | distance | min | name   (default: rating)
                      rating=best first; eta/distance/min=lowest first.
  --json              Emit the filtered results as a JSON array (for scripting).
                      Includes fields not shown in the table: cuisines, slug.
  --raw               Dump the untouched ~44KB API response and exit.
  -h, --help          This text.

EXAMPLES
  ./scripts/lieferando.sh --lat 50.1109 --lon 8.6821 döner --sort rating
  ./scripts/lieferando.sh --lat 50.1109 --lon 8.6821 --open --sort distance
  ./scripts/lieferando.sh --lat 50.1109 --lon 8.6821 pizza --sort min --json | jq '.[0].name'
  ./scripts/lieferando.sh --country at --lat 48.2082 --lon 16.3738

NOTES
  Prices (MIN, FEE) come from the API in cents and are shown in euros. FEE is the
  cheapest delivery band; some places charge less above a higher order value.
  ETA is a meaningless placeholder while a restaurant is closed, so the table
  shows the opening time instead. No authentication is required by this API.

EXIT CODES
  0  success (including "no restaurants matched QUERY" — table prints
     "No matches.", --json prints [])
  1  usage error (bad flag, bad value, missing argument)
  2  network or API error (request failed, or response was not usable JSON)
EOF
  exit 0
}

die() {
  echo "lieferando.sh: $1" >&2
  [[ $# -gt 1 ]] && echo "  $2" >&2
  echo "  try: ./scripts/lieferando.sh --help" >&2
  exit "${3:-1}"
}

# Guard against `--lat` with no value (and against it swallowing the next flag).
need_arg() {
  [[ $# -ge 2 && -n "$2" && "$2" != -* ]] || die "option $1 requires a value"
  echo "$2"
}

in_list() { [[ " $2 " == *" $1 "* ]]; }

while [[ $# -gt 0 ]]; do
  case "$1" in
    --lat)        LAT=$(need_arg "$@"); shift 2;;
    --lon)        LON=$(need_arg "$@"); shift 2;;
    --country)    COUNTRY=$(need_arg "$@"); shift 2;;
    --sort)       SORT=$(need_arg "$@"); shift 2;;
    --collection) SERVICE=collection; shift;;
    --open)       OPEN_ONLY=1; shift;;
    --json)       FORMAT=json; shift;;
    --raw)        FORMAT=raw; shift;;
    -h|--help)    usage;;
    -*)           die "unknown option: $1";;
    *)
      [[ $QUERY_SET -eq 1 ]] && die "unexpected extra argument: $1" \
        "only one QUERY is accepted; quote it if it contains spaces"
      QUERY="$1"; QUERY_SET=1; shift;;
  esac
done

COUNTRY=$(echo "$COUNTRY" | tr '[:upper:]' '[:lower:]')
in_list "$COUNTRY" "$VALID_COUNTRIES" \
  || die "invalid --country: '$COUNTRY'" "valid: ${VALID_COUNTRIES// /, }"
in_list "$SORT" "$VALID_SORTS" \
  || die "invalid --sort: '$SORT'" "valid: ${VALID_SORTS// /, }"

[[ -n "$LAT" && -n "$LON" ]] \
  || die "--lat and --lon are required (there is no default location)" \
         "geocode the address first, e.g. --lat 50.1109 --lon 8.6821"

[[ "$LAT" =~ ^-?[0-9]+(\.[0-9]+)?$ ]] || die "invalid --lat: '$LAT'" "expected a number, e.g. 50.1109"
[[ "$LON" =~ ^-?[0-9]+(\.[0-9]+)?$ ]] || die "invalid --lon: '$LON'" "expected a number, e.g. 8.6821"
awk -v v="$LAT" 'BEGIN{exit !(v>=-90 && v<=90)}'   || die "--lat out of range: '$LAT'" "must be between -90 and 90"
awk -v v="$LON" 'BEGIN{exit !(v>=-180 && v<=180)}' || die "--lon out of range: '$LON'" "must be between -180 and 180"

SITE="https://www.lieferando.${COUNTRY}"
URL="https://rest.api.eu-central-1.production.jet-external.com/discovery/${COUNTRY}/restaurants/enriched"
URL+="?latitude=${LAT}&longitude=${LON}&serviceType=${SERVICE}"
URL+="&ratingsOutOfFive=true&je-tgl-ops_include_closed=true&vertical=all"

BODY_FILE=$(mktemp -t claude-code.XXXXXX.json)
trap 'rm -f "$BODY_FILE"' EXIT

HTTP=$(curl -sS --compressed --max-time 30 "$URL" \
  -H 'Accept: application/json;v=3' \
  -H "Accept-Language: de-${COUNTRY^^}" \
  -H "Referer: ${SITE}/" \
  -H "Origin: ${SITE}" \
  -H 'x-jet-application: OneWeb' \
  -H 'User-Agent: Mozilla/5.0 (X11; Linux x86_64; rv:152.0) Gecko/20100101 Firefox/152.0' \
  -o "$BODY_FILE" -w '%{http_code}') \
  || die "request to the discovery API failed" "network down, or the endpoint moved" 2

[[ "$HTTP" == 200 ]] || die "API returned HTTP $HTTP" \
  "the endpoint may have changed, or the coordinates/country are rejected" 2

BODY=$(cat "$BODY_FILE")

jq -e 'has("restaurants")' >/dev/null 2>&1 <<<"$BODY" \
  || die "API response was not the expected JSON (no 'restaurants' key)" \
         "run with --raw to inspect what came back" 2

if [[ $FORMAT == raw ]]; then echo "$BODY"; exit 0; fi

# A location with zero coverage is a different thing from a query with zero hits —
# say so, or the caller silently blames their QUERY.
if [[ $(jq '.restaurants | length' <<<"$BODY") -eq 0 ]]; then
  echo "lieferando.sh: no restaurants deliver to ${LAT},${LON} (country=${COUNTRY})." >&2
  echo "  check that --country matches the coordinates." >&2
fi

ROWS=$(jq \
  --arg q "$QUERY" --arg service "$SERVICE" --arg sort "$SORT" --argjson openonly "$OPEN_ONLY" '

def norm: ascii_downcase
  | gsub("ä";"a") | gsub("ö";"o") | gsub("ü";"u") | gsub("ß";"ss");

. as $root
| ($q | norm) as $needle
| ($service == "delivery") as $isDelivery
| [ .restaurants[]
    | . as $r
    | ($root.deliveryFees.restaurants[$r.id] // {}) as $fee
    | {
        name: .name,
        rating: (.rating.starRating // 0),
        ratingCount: (.rating.count // 0),
        etaMinutes: (if $isDelivery then .deliveryEtaMinutes.rangeUpper else null end),
        opensAt: ((if $isDelivery then .deliveryOpeningTimeLocal else .openingTimeLocal end) // "" | split("T") | last[0:5]),
        km: ((.driveDistanceMeters // 0) / 1000 * 10 | round / 10),
        minOrderCents: $fee.minimumOrderValue,
        feeCents: ($fee.bands // [] | if length > 0 then (map(.fee) | min) else null end),
        isOpen: (if $isDelivery then .isOpenNowForDelivery else .isOpenNowForCollection end),
        deals: [.deals[]? | if (.description // "") == "" then .offerType else .description end],
        cuisines: [.cuisines[].name],
        slug: .uniqueName
      }
    | select($needle == "" or (([.name] + .cuisines | join(" ") | norm) | contains($needle)))
    | select($openonly == 0 or .isOpen)
  ]
| sort_by(
    if   $sort == "rating"   then -.rating
    elif $sort == "eta"      then (.etaMinutes // 9999)
    elif $sort == "distance" then .km
    elif $sort == "min"      then (.minOrderCents // 999999)
    else .name end)
' <<<"$BODY")

if [[ $FORMAT == json ]]; then echo "$ROWS"; exit 0; fi

jq -r '
def money($cents): if $cents == null then "-" else "\($cents / 100)€" end;

if length == 0 then "No matches." else
  (["NAME","RATING","ETA/OPENS","DIST","MIN","FEE","DEALS"] | @tsv),
  (.[] | [
    (if .isOpen then .name else .name + " (closed)" end),
    (if .ratingCount > 0 then "\(.rating)★ (\(.ratingCount))" else "-" end),
    (if .isOpen then (if .etaMinutes then "\(.etaMinutes)min" else "-" end)
     elif .opensAt != "" then "opens \(.opensAt)"
     else "-" end),
    "\(.km)km",
    money(.minOrderCents),
    money(.feeCents),
    (.deals | if length == 0 then "-" else join("; ") end)
  ] | @tsv)
end' <<<"$ROWS" | column -t -s $'\t'
