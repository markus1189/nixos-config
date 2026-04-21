CACERT="@cacert@/etc/ssl/certs/ca-bundle.crt"

: "${BEEMINDER_USER:?Set BEEMINDER_USER}"
: "${BEEMINDER_TOKEN:?Set BEEMINDER_TOKEN}"
: "${ZWIFT_USER:?Set ZWIFT_USER}"
: "${ZWIFT_PASS:?Set ZWIFT_PASS}"

ZWIFT_AUTH_URL="https://secure.zwift.com/auth/realms/zwift/protocol/openid-connect/token"
ZWIFT_API_URL="https://us-or-rly101.zwift.com"

log() { echo "[sync-weight] $(date +%H:%M:%S) $*"; }
die() { log "ERROR: $*" >&2; exit 1; }

# --- 1. Fetch latest weight from Beeminder ---
log "Fetching latest weight from Beeminder..."
DATAPOINT=$(curl --silent --fail --cacert "$CACERT" \
  "https://www.beeminder.com/api/v1/users/${BEEMINDER_USER}/goals/weight/datapoints.json?auth_token=${BEEMINDER_TOKEN}&count=1&sort=daystamp") \
  || die "Failed to fetch from Beeminder"

DAYSTAMP=$(echo "$DATAPOINT" | jq -r '.[0].daystamp')
WEIGHT_KG=$(echo "$DATAPOINT" | jq -r '.[0].value')

[[ "$DAYSTAMP" == "null" || "$WEIGHT_KG" == "null" ]] && die "No datapoint found"

log "Beeminder datapoint: ${WEIGHT_KG} kg on ${DAYSTAMP}"

# --- 2. Staleness check: must be from yesterday ---
YESTERDAY=$(date -d 'yesterday' +%Y%m%d)
DAYSTAMP_CLEAN=$(echo "$DAYSTAMP" | tr -d '-')

if [[ "$DAYSTAMP_CLEAN" != "$YESTERDAY" ]]; then
  die "Stale data: datapoint is from ${DAYSTAMP}, expected ${YESTERDAY}"
fi

# --- 3. Convert to Zwift grams ---
WEIGHT_GRAMS=$(echo "$WEIGHT_KG" | jq -r '. * 1000 | round')
log "Weight in grams for Zwift: ${WEIGHT_GRAMS}"

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
  die "Verification failed: expected ${WEIGHT_GRAMS} but got ${NEW_WEIGHT}"
fi
