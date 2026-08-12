#!/usr/bin/env nix
#! nix shell nixpkgs#bash nixpkgs#curl nixpkgs#jq nixpkgs#coreutils nixpkgs#gnused nixpkgs#pass nixpkgs#gnupg --command bash
set -euo pipefail

usage() {
  # Requested help is output; an unusable invocation is a diagnostic.
  local code="${1:-1}" fd=2
  if [[ "$code" -eq 0 ]]; then fd=1; fi
  cat >&"$fd" <<EOF
Usage: $0 [opts] <url> [url ...]

Extracts page content as markdown via the Kagi Extract API, for URLs that
WebFetch/curl cannot read (403/401, JS-only shells, login walls).

  -o, --out DIR    write results into DIR (default: a fresh mktemp dir)
      --stdout     print markdown to stdout instead of writing files
  -t, --timeout S  server-side budget for the whole batch, seconds (default 30)
  -f, --force      extract even domains listed as never working

Domains recorded under "Never works" in references/domains.md are refused
before the request, costing nothing. Record every outcome in that file.
URLs repeated within one batch are deduplicated, for the same reason.

Max 10 URLs per call (API limit). URLs must be https://.
Cost: \$0.004 per URL (\$4 / 1000 pages), billed per URL attempted, failures
included. The estimated charge is printed to stderr before the request and
the final charge after it.

API key: \$KAGI_API_KEY, else \`pass api/kagi/search\`.

Output: one status line per URL on stdout —
  OK   <bytes>  <file>  <url>
  FAIL <error>          <url>
Exit 0 if any URL succeeded, 1 if all failed, 2 on request-level error.
EOF
  exit "$code"
}

# $4 per 1000 pages => 4 thousandths of a dollar per URL. Integer math only:
# track thousandths, print as dollars.
PRICE_PER_URL_MILLI=4
fmt_cost() { printf '$%d.%03d' "$(( $1 / 1000 ))" "$(( $1 % 1000 ))"; }

OUTDIR="" TO=30 TO_STDOUT=0 FORCE=0 URLS=()
while [[ $# -gt 0 ]]; do
  case "$1" in
    -o|--out)     OUTDIR="${2:?--out needs a directory}"; shift 2 ;;
    -t|--timeout) TO="${2:?--timeout needs seconds}"; shift 2 ;;
    --stdout)     TO_STDOUT=1; shift ;;
    -f|--force)   FORCE=1; shift ;;
    -h|--help)    usage 0 ;;
    -*)           echo "Unknown option: $1" >&2; usage ;;
    *)            URLS+=("$1"); shift ;;
  esac
done

[[ ${#URLS[@]} -eq 0 ]] && usage
for u in "${URLS[@]}"; do
  [[ "$u" == https://* ]] || { echo "Error: not an https:// URL: $u" >&2; exit 2; }
done
# Validated here rather than at first use: jq and curl both take it, and their
# native complaints ("invalid JSON text passed to --argjson") say nothing useful.
[[ "$TO" =~ ^[0-9]+$ ]] || { echo "Error: --timeout must be a whole number of seconds, got: $TO" >&2; exit 2; }

# Domains under "## Never works" in the ledger are known-dead. Refusing them
# here is the difference between $0.000 and $0.004 per pointless URL.
LEDGER="$(dirname "$0")/../references/domains.md"
NEVER=""
if [[ -f "$LEDGER" ]]; then
  NEVER="$(sed -n '/^## Never works/,$p' "$LEDGER" | sed -n 's/^- `\([^`]*\)`.*/\1/p')"
fi

# Authority only: strip path, query, userinfo and port. A leftover ":443" or
# "user@" makes a blocked domain miss the match and cost $0.004 to confirm.
host_of() {
  local h="${1#https://}"
  h="${h%%/*}"; h="${h%%\?*}"; h="${h##*@}"; h="${h%%:*}"
  printf '%s' "${h,,}"
}

# Blocked URLs are dropped, not fatal: one dead domain must not cost the rest
# of the batch a round trip.
KEEP=()
SEEN=()
for u in "${URLS[@]}"; do
  # A URL repeated in one batch is charged twice for one page.
  dup=0
  for s in ${SEEN[@]+"${SEEN[@]}"}; do [[ "$s" == "$u" ]] && { dup=1; break; }; done
  if [[ "$dup" -eq 1 ]]; then
    printf 'SKIP duplicate URL in batch (would be billed twice)\t%s\n' "$u"
    continue
  fi
  SEEN+=("$u")
  hit=""
  if [[ "$FORCE" -eq 0 && -n "$NEVER" ]]; then
    h="$(host_of "$u")"
    while IFS= read -r d; do
      [[ -z "$d" ]] && continue
      if [[ "$h" == "$d" || "$h" == *".$d" ]]; then hit="$d"; break; fi
    done <<<"$NEVER"
  fi
  if [[ -n "$hit" ]]; then
    printf 'SKIP never-works domain `%s` (references/domains.md)\t%s\n' "$hit" "$u"
  else
    KEEP+=("$u")
  fi
done

if [[ ${#KEEP[@]} -eq 0 ]]; then
  echo "Error: every URL is on the never-works list; Kagi returns no content. --force to spend anyway." >&2
  exit 2
fi
# Checked after filtering: the API limit applies to what is actually sent, so
# duplicates and blocked domains must not push a viable batch over the line.
if [[ ${#KEEP[@]} -gt 10 ]]; then
  echo "Error: ${#KEEP[@]} URLs to send, API accepts at most 10 per call. Split into batches." >&2
  exit 2
fi
URLS=("${KEEP[@]}")

KEY="${KAGI_API_KEY:-}"
if [[ -z "$KEY" ]]; then
  KEY="$(pass api/kagi/search 2>/dev/null | head -1 || true)"
fi
if [[ -z "$KEY" ]]; then
  echo "Error: no API key. Set \$KAGI_API_KEY or add it to \`pass api/kagi/search\`." >&2
  exit 2
fi

COST=$(( ${#URLS[@]} * PRICE_PER_URL_MILLI ))
printf 'Requesting %d URL(s) — est. cost %s at $0.004/URL, failures billed.\n' \
  "${#URLS[@]}" "$(fmt_cost "$COST")" >&2

BODY="$(jq -n --argjson to "$TO" '$ARGS.positional | {pages: map({url: .}), timeout: $to}' \
  --args "${URLS[@]}")"

RESP="$(mktemp -t claude-code.kagi.XXXXXX.json)"
trap 'rm -f "$RESP"' EXIT

# The key goes in via --config on stdin, never as an argument: argv is world-
# readable through /proc/PID/cmdline and lands in process accounting. Config
# syntax is `header = "..."`, so backslashes and quotes need escaping.
KEY_ESC="${KEY//\\/\\\\}"; KEY_ESC="${KEY_ESC//\"/\\\"}"

HTTP="$(curl -sS -o "$RESP" -w '%{http_code}' \
  --max-time $((TO + 30)) \
  -X POST https://kagi.com/api/v1/extract \
  -H 'Content-Type: application/json' \
  -d "$BODY" -K - <<EOF
header = "Authorization: Bearer $KEY_ESC"
EOF
)" || { echo "Error: request to Kagi failed (network/timeout)." >&2; exit 2; }

if [[ "$HTTP" != 200 ]]; then
  echo "Error: Kagi returned HTTP $HTTP" >&2
  jq -re '.errors[]? | "  \(.code): \(.message // "no message")"' "$RESP" >&2 2>/dev/null \
    || head -c 500 "$RESP" >&2
  case "$(jq -r '.errors[0].code // ""' "$RESP" 2>/dev/null)" in
    general.invalid_token) echo "  -> The stored key is malformed. Regenerate at https://kagi.com/api/keys" >&2 ;;
    *insufficient*|*balance*|*limit*) echo "  -> Top up or raise the cap at https://kagi.com/api/billing" >&2 ;;
  esac
  exit 2
fi

# A 200 with no .data array is a request-level failure, not "every URL failed":
# without this, jq dies inside the process substitution below, set -e never sees
# it, and the caller gets exit 1 plus a $0.000 receipt.
jq -e '.data | type == "array"' "$RESP" >/dev/null 2>&1 \
  || { echo "Error: Kagi returned HTTP 200 with no .data array." >&2
       head -c 500 "$RESP" >&2; exit 2; }

# Billing follows the request, not the response: every URL sent is charged
# whether or not it came back. Report both numbers so they can't silently drift.
BILLED=${#URLS[@]}
RETURNED=$(jq '.data | length' "$RESP")
report_cost() {
  printf 'Cost: %s — %d of %d URL(s) returned content, %d billed.\n' \
    "$(fmt_cost "$(( BILLED * PRICE_PER_URL_MILLI ))")" "$1" "$RETURNED" "$BILLED"
}

if [[ "$TO_STDOUT" -eq 1 ]]; then
  jq -r '.data[] | "\n## \(.url)\n\n\(if (.markdown // "") == "" then
      "EXTRACTION FAILED: \(.error // "empty extraction (no markdown, no error)")"
    else .markdown end)"' "$RESP"
  GOT=$(jq '[.data[] | select((.markdown // "") != "")] | length' "$RESP")
  report_cost "$GOT" >&2
  [[ "$GOT" -gt 0 ]] || exit 1
  exit 0
fi

if [[ -z "$OUTDIR" ]]; then
  OUTDIR="$(mktemp -d -t claude-code.kagi.XXXXXX)"
else
  mkdir -p "$OUTDIR"
fi

OK=0
n=0
while IFS=$'\t' read -r url err has_md; do
  if [[ "$err" != "null" ]]; then
    printf 'FAIL %s\t%s\n' "$err" "$url"
    continue
  fi
  # No error and no markdown still means nothing was extracted. Writing it out
  # would put the literal string "null" in the file and call it a 5-byte OK.
  if [[ "$has_md" != "md" ]]; then
    printf 'FAIL empty extraction (no markdown, no error)\t%s\n' "$url"
    continue
  fi
  n=$((n + 1))
  slug="$(printf '%s' "$url" | sed -e 's#^https\?://##' -e 's#[^A-Za-z0-9._-]#-#g' | cut -c1-60)"
  f="$OUTDIR/$(printf '%02d' "$n")-$slug.md"
  # first(): a URL repeated in one batch must not concatenate both bodies.
  jq -r --arg u "$url" 'first(.data[] | select(.url == $u)) | .markdown' "$RESP" > "$f"
  printf 'OK   %s\t%s\t%s\n' "$(wc -c <"$f" | tr -d ' ')" "$f" "$url"
  OK=$((OK + 1))
done < <(jq -r '.data[] | [.url, (.error // "null"),
                           (if (.markdown // "") == "" then "nomd" else "md" end)] | @tsv' "$RESP")

report_cost "$OK"

[[ "$OK" -gt 0 ]] || exit 1
