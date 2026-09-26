# kb-retro-scan — ledger miner for /mh:retro-week
#
# SCOPE — deliberately narrow. This script reads ONLY the two structured ledger files:
#   YYYY-MM/DD-*/wrap-up-log.md   append-only event log, written by /mh:retro
#   YYYY-MM/DD-*/retro-week.md    decisions projection, written by /mh:retro-week
#
# It never looks inside day-dir prose, and that is a design decision, not a TODO.
# Threads and activity are not scannable, as demonstrated on the Schornstein chain
# (2026-06/30 → 2026-07/14 → 2026-07/17, one thread, three day dirs):
#   - freeform headings   the live state of that thread sat under `## Verdict`;
#                         a `Nächste Schritte` pattern misses the newest file entirely
#   - stale checkboxes    June's unchecked `- [ ]` items were resolved in the July file,
#                         so counting open boxes reports a number that is simply false
#   - no chaining         nothing in path, filename or heading links the three files;
#                         only the prose does
# Fixing that needs a reader, not a parser. Hence: wrap-up-log.md is a database,
# day dirs are prose. This parses the database. /mh:retro-week reads the prose.
#
# Emits facts, never verdicts: LEDGER (open + in-window findings), RECURRENCE
# (all-time slug frequency), COUNTS (follow-through rate).
#
# Usage:  kb-retro-scan [--days N] [--since YYYY-MM-DD]
#   --days N            window is the last N days (default 7)
#   --since YYYY-MM-DD  explicit window start; overrides --days (reproducible runs)
#
# Open findings are reported regardless of age: a finding stays open until it is
# APPLIED or KILLED, so a CARRY from three weeks ago still surfaces every Friday.

ROOT="${KB_ROOT:-$HOME/Stuff}"
cd "$ROOT"

DAYS=7
SINCE=""

# Not `sed -n '2,26p' "$0"`: under writeShellApplication $0 carries a preamble.
usage() {
  cat <<'USAGE'
Usage:  kb-retro-scan [--days N] [--since YYYY-MM-DD]
  --days N            window is the last N days (default 7)
  --since YYYY-MM-DD  explicit window start; overrides --days (reproducible runs)

Reads only YYYY-MM/DD-*/wrap-up-log.md and YYYY-MM/DD-*/retro-week.md under
$KB_ROOT (default ~/Stuff). Emits LEDGER, RECURRENCE and COUNTS.
USAGE
}

while [ $# -gt 0 ]; do
  case "$1" in
    --days)  DAYS="${2:?--days needs a number}"; shift 2 ;;
    --since) SINCE="${2:?--since needs YYYY-MM-DD}"; shift 2 ;;
    -h|--help) usage; exit 0 ;;
    *) printf 'kb-retro-scan: unknown argument: %s\n' "$1" >&2; exit 2 ;;
  esac
done

[ -n "$SINCE" ] || SINCE="$(date -d "-$DAYS days" +%F)"
UNTIL="$(date +%F)"

shopt -s globstar nullglob
LOGS=( **/wrap-up-log.md )
RETROS=( **/retro-week.md )

if [ ${#LOGS[@]} -eq 0 ]; then
  printf 'kb-retro-scan: no wrap-up-log.md found under %s — nothing to scan.\n' "$ROOT" >&2
  exit 1
fi

TSV="$(mktemp -t claude-code.retroscan.XXXXXX)"
trap 'rm -f "$TSV"' EXIT

# --- Parse -------------------------------------------------------------------------
# Field layout is anchored from BOTH ends, never by field count, so a title containing
# a stray " · " cannot shift the schema:
#   $1=date  $2=session  …  $(NF-1)=target  $NF=applied?
# The tier field (🟢/🟡) is located by scanning, and everything between tier and target
# is the title. Field 3 is the slug unless it is a known category — that is what makes
# both schemas readable: the 7-field pre-slug form (2026-07-16 and earlier) and the
# 8-field slugged form. Unslugged lines get slug "-" and cannot join to decisions;
# they are reported so they can be backfilled, not silently dropped.

awk -v since="$SINCE" '
  BEGIN {
    FS = " · "
    split("friction skill-gap missing-knowledge knowledge automation", c, " ")
    for (i in c) CAT[c[i]] = 1
  }

  function norm(a) {
    if (a ~ /^APPLIED/) return "APPLIED"
    if (a ~ /^KILLED/)  return "KILLED"
    if (a ~ /^CARRY/)   return a
    if (a == "no")      return "open"
    return "open"                       # unrecognised → open; never lose a finding
  }

  # --- retro-week.md decisions table: | slug | first seen | hits | decision | -------
  FILENAME ~ /retro-week\.md$/ {
    if ($0 !~ /^[ \t]*\|/) next
    n = split($0, p, /[ \t]*\|[ \t]*/)   # leading empty field from the opening pipe
    if (n < 5) next
    s = p[2]; d = p[5]
    if (s == "" || s == "slug" || s ~ /^-+$/) next
    if (FILENAME > DECSRC[s]) { DEC[s] = d; DECSRC[s] = FILENAME }   # newest retro wins
    next
  }

  # --- wrap-up-log.md findings ------------------------------------------------------
  FILENAME ~ /wrap-up-log\.md$/ && /^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9] · / {
    date = $1; sess = $2

    tier_i = 0
    for (i = 3; i <= NF; i++) if ($i ~ /🟢|🟡/) { tier_i = i; break }
    if (tier_i == 0 || tier_i > NF - 2) { bad++; next }

    if (CAT[$3]) { slug = "-";  cat = $3 } else { slug = $3; cat = $4 }
    tier  = ($tier_i ~ /🟢/) ? "green" : "gated"
    recur = ($tier_i ~ /🔁/) ? "Y" : "-"

    title = ""
    for (i = tier_i + 1; i <= NF - 2; i++) title = title (title == "" ? "" : " · ") $i
    target = $(NF-1)
    state  = norm($NF)

    key = (slug != "-") ? slug : "«unslugged»:" date ":" substr(title, 1, 24)

    HITS[key]++
    if (!(key in FIRST) || date < FIRST[key]) FIRST[key] = date
    if (!(key in LAST)  || date >= LAST[key]) {
      LAST[key] = date; TITLE[key] = title; TARGET[key] = target
      CATOF[key] = cat; TIER[key] = tier; SLUG[key] = slug
    }
    if (recur == "Y") RECUR[key] = "Y"
    if (state == "APPLIED") APPLIED[key] = 1        # any APPLIED wins over later "no"
    if (state == "KILLED")  KILLED[key]  = 1
    if (date >= since) { INWIN[key] = 1; WINROWS++ }
    if (state == "APPLIED" && date >= since) WINAPPLIED++
    if (state == "KILLED"  && date >= since) WINKILLED++
    next
  }

  END {
    for (k in HITS) {
      # Effective state: a retro decision (newest) outranks the log field, except that
      # an APPLIED in the log is never downgraded — the fix is on disk either way.
      st = "open"
      if (k in DEC && DEC[k] != "") {
        d = DEC[k]
        if (d ~ /^APPLIED/) st = "APPLIED"
        else if (d ~ /^KILLED/) st = "KILLED"
        else if (d ~ /^CARRY/) { sub(/[ \t]+$/, "", d); st = d }
      }
      if (APPLIED[k]) st = "APPLIED"
      else if (KILLED[k] && st == "open") st = "KILLED"

      open = (st ~ /^(open|CARRY)/) ? 1 : 0
      if (!open && !INWIN[k]) continue          # closed and out of window → not our problem

      printf "%d\t%s\t%d\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n",
             (open ? 0 : 1), st, HITS[k], FIRST[k], LAST[k],
             TIER[k], (RECUR[k] ? "Y" : "-"), CATOF[k],
             (SLUG[k] != "-" ? SLUG[k] : k), TARGET[k], TITLE[k],
             (INWIN[k] ? "win" : "old")
    }
    printf "@\t%d\t%d\t%d\t%d\n", WINROWS, WINAPPLIED, WINKILLED, bad > "/dev/stderr"
  }
' "${RETROS[@]}" "${LOGS[@]}" 2> >(grep '^@' > "$TSV.meta") | sort -t$'\t' -k1,1 -k3,3nr -k4,4 > "$TSV"

read -r _ WINROWS WINAPPLIED WINKILLED BAD < "$TSV.meta" 2>/dev/null || { WINROWS=0; WINAPPLIED=0; WINKILLED=0; BAD=0; }
rm -f "$TSV.meta"

# --- Report ------------------------------------------------------------------------
printf '# kb-retro-scan · ledger only\n'
printf '# window   %s .. %s\n' "$SINCE" "$UNTIL"
printf '# sources  %d wrap-up-log.md · %d retro-week.md\n' "${#LOGS[@]}" "${#RETROS[@]}"
printf '# note     day-dir prose is NOT scanned — read it. See /mh:retro-week.\n'
[ "${BAD:-0}" -gt 0 ] && printf '# warn     %d unparseable line(s) skipped\n' "$BAD"
printf '\n'

printf '## LEDGER — every open finding (any age) + everything decided in window\n\n'
if [ -s "$TSV" ]; then
  printf '%-10s %-4s %-11s %-6s %-5s %-16s %-30s %s\n' \
         STATE HITS FIRST TIER RECUR CATEGORY SLUG TITLE
  awk -F'\t' '{
    t = $11; if (length(t) > 62) t = substr(t, 1, 59) "…"
    printf "%-10s %-4d %-11s %-6s %-5s %-16s %-30s %s\n", $2, $3, $4, $6, $7, $8, $9, t
  }' "$TSV"
else
  printf '(no open findings, nothing decided in window — suspicious, or a good week)\n'
fi
printf '\n'

printf '## RECURRENCE — all time, >=2 hits, still open\n\n'
RECUR_OUT="$(awk -F'\t' '$1 == 0 && $3 >= 2 {
  esc = ($3 >= 3) ? "ESCALATE — >=3 hits is a rule change, not a note" : "watch"
  printf "%-30s %d hits  %s\n     first %s · last %s · target: %s\n", $9, $3, esc, $4, $5, $10
}' "$TSV")"
if [ -n "$RECUR_OUT" ]; then printf '%s\n' "$RECUR_OUT"; else printf '(none — no slug is repeating)\n'; fi
printf '\n'

ESCALATIONS="$(awk -F'\t' '$1 == 0 && $3 >= 3' "$TSV" | grep -c . || true)"
CARRIED="$(awk -F'\t' '$2 ~ /^CARRY/' "$TSV" | grep -c . || true)"
OPEN_NOW="$(awk -F'\t' '$1 == 0' "$TSV" | grep -c . || true)"
UNSLUGGED="$(awk -F'\t' '$9 ~ /^«unslugged»/' "$TSV" | grep -c . || true)"

RATE=0
[ "${WINROWS:-0}" -gt 0 ] && RATE=$(( (WINAPPLIED + WINKILLED) * 100 / WINROWS ))

printf '## COUNTS\n\n'
printf 'window findings    %s\n' "${WINROWS:-0}"
printf '  applied          %s\n' "${WINAPPLIED:-0}"
printf '  killed           %s\n' "${WINKILLED:-0}"
printf 'follow-through     %s%% — (applied+killed)/window. A kill counts: deciding not to\n' "$RATE"
printf '                   do it closes the loop. Only "no" forever is failure.\n'
printf 'open right now     %s  (all ages, incl. carried)\n' "$OPEN_NOW"
printf 'carried            %s  (CARRY/n from a prior retro-week.md)\n' "$CARRIED"
printf 'escalations        %s  (>=3 hits, still open)\n' "$ESCALATIONS"
[ "${UNSLUGGED:-0}" -gt 0 ] && \
  printf 'unslugged          %s  ← cannot join to decisions; backfill a slug\n' "$UNSLUGGED"
printf '\n'
printf '## NOT SCANNED\n\n'
printf 'Open threads, activity, what you actually worked on — all live in day-dir prose,\n'
printf 'which no pattern reads correctly. /mh:retro-week reads those files.\n'
