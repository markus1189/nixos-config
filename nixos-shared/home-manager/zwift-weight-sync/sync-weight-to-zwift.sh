CACERT="@cacert@/etc/ssl/certs/ca-bundle.crt"

hc_ping() {
  if [[ -n "${HC_PING_URL:-}" ]]; then
    curl -fsS -m 10 --retry 5 -o /dev/null --cacert "$CACERT" "$@" || true
  fi
}

# Capture all output for healthcheck log
LOG_FILE=$(mktemp -t sync-weight.XXXXXX.log)
exec > >(tee -a "$LOG_FILE") 2>&1
trap 'EXIT_CODE=$?; if [[ -n "${HC_PING_URL:-}" ]]; then curl -fsS -m 10 --retry 5 --cacert "$CACERT" --data-raw "$(cat "$LOG_FILE")" "${HC_PING_URL}/${EXIT_CODE}" -o /dev/null || true; fi; rm -f "$LOG_FILE"' EXIT

hc_ping "${HC_PING_URL}/start"

: "${BEEMINDER_USER:?Set BEEMINDER_USER}"
: "${BEEMINDER_TOKEN:?Set BEEMINDER_TOKEN}"
: "${ZWIFT_USER:?Set ZWIFT_USER}"
: "${ZWIFT_PASS:?Set ZWIFT_PASS}"

ZWIFT_AUTH_URL="https://secure.zwift.com/auth/realms/zwift/protocol/openid-connect/token"
ZWIFT_API_URL="https://us-or-rly101.zwift.com"

log() { echo "[sync-weight] $(date +%H:%M:%S) $*"; }
die() { log "ERROR: $*" >&2; exit 1; }

# --- 1. Fetch last 7 datapoints from Beeminder ---
log "Fetching last 7 weight datapoints from Beeminder..."
DATAPOINTS=$(curl --silent --fail --cacert "$CACERT" \
  "https://www.beeminder.com/api/v1/users/${BEEMINDER_USER}/goals/weight/datapoints.json?auth_token=${BEEMINDER_TOKEN}&count=7&sort=daystamp") \
  || die "Failed to fetch from Beeminder"

NUM_POINTS=$(echo "$DATAPOINTS" | jq 'length')
[[ "$NUM_POINTS" -eq 0 ]] && die "No datapoints found"

LATEST_DAYSTAMP=$(echo "$DATAPOINTS" | jq -r '.[0].daystamp')
log "Got ${NUM_POINTS} datapoints, latest from ${LATEST_DAYSTAMP}"
echo "$DATAPOINTS" | jq -r '.[] | "  \(.daystamp): \(.value) kg"'

# --- 2. Staleness check: most recent must be within last 2 days ---
YESTERDAY=$(date -d 'yesterday' +%Y%m%d)
TODAY=$(date +%Y%m%d)
DAYSTAMP_CLEAN=$(echo "$LATEST_DAYSTAMP" | tr -d '-')

if [[ "$DAYSTAMP_CLEAN" != "$YESTERDAY" && "$DAYSTAMP_CLEAN" != "$TODAY" ]]; then
  die "Stale data: most recent datapoint is from ${LATEST_DAYSTAMP}, expected ${YESTERDAY} or ${TODAY}"
fi

# --- 3. Compute 7-day rolling average and convert to Zwift grams ---
WEIGHT_KG=$(echo "$DATAPOINTS" | jq '[.[].value] | add / length | . * 100 | round / 100')
WEIGHT_GRAMS=$(echo "$DATAPOINTS" | jq '[.[].value] | add / length * 1000 | round')
log "7-day avg weight: ${WEIGHT_KG} kg (${WEIGHT_GRAMS} g)"

# --- 4. Authenticate with Zwift ---
log "Authenticating with Zwift..."
AUTH_RESP=$(curl --silent --fail --cacert "$CACERT" \
  -X POST "$ZWIFT_AUTH_URL" \
  -d "client_id=Zwift Game Client" \
  -d "grant_type=password" \
  -d "username=${ZWIFT_USER}" \
  -d "password=${ZWIFT_PASS}") \
  || die "Zwift authentication failed"

ACCESS_TOKEN=$(echo "$AUTH_RESP" | jq -r '.access_token')
[[ "$ACCESS_TOKEN" == "null" || -z "$ACCESS_TOKEN" ]] && die "No access token received"
log "Zwift auth successful"

zwift_api() {
  curl --silent --fail --cacert "$CACERT" \
    -H "Authorization: Bearer ${ACCESS_TOKEN}" \
    -H "Accept: application/json" \
    -H "User-Agent: Zwift/115 CFNetwork/758.0.2 Darwin/15.0.0" \
    "$@"
}

# --- 5. Get current profile ---
log "Fetching Zwift profile..."
PROFILE=$(zwift_api "${ZWIFT_API_URL}/api/profiles/me") \
  || die "Failed to fetch Zwift profile"

PLAYER_ID=$(echo "$PROFILE" | jq -r '.id')
OLD_WEIGHT=$(echo "$PROFILE" | jq -r '.weight')
OLD_WEIGHT_KG=$(echo "$OLD_WEIGHT" | jq -r '. / 1000')

log "Player ID: ${PLAYER_ID}"
log "Current Zwift weight: ${OLD_WEIGHT_KG} kg (${OLD_WEIGHT} g)"

if [[ "$OLD_WEIGHT" == "$WEIGHT_GRAMS" ]]; then
  log "Weight already up to date, nothing to do"
  exit 0
fi

# --- 6. Update weight ---
log "Updating weight: ${OLD_WEIGHT_KG} kg -> ${WEIGHT_KG} kg"
UPDATED_PROFILE=$(echo "$PROFILE" | jq ".weight = ${WEIGHT_GRAMS}")

UPDATE_RESP=$(zwift_api \
  -X PUT \
  -H "Content-Type: application/json" \
  -d "$UPDATED_PROFILE" \
  "${ZWIFT_API_URL}/api/profiles/me/${PLAYER_ID}") \
  || die "Failed to update Zwift profile"

# --- 7. Verify ---
log "Verifying update..."
VERIFY=$(zwift_api "${ZWIFT_API_URL}/api/profiles/me") \
  || die "Failed to verify Zwift profile"

NEW_WEIGHT=$(echo "$VERIFY" | jq -r '.weight')

if [[ "$NEW_WEIGHT" == "$WEIGHT_GRAMS" ]]; then
  log "Success: weight updated to ${WEIGHT_KG} kg (${WEIGHT_GRAMS} g)"
else
  log "ERROR: Verification failed: expected ${WEIGHT_GRAMS} but got ${NEW_WEIGHT}" >&2
  exit 1
fi
