# kb-index — generate llms.txt + per-month INDEX.md + per-series indexes for ~/Stuff
#
# A deterministic, idempotent navigation index for the ~/Stuff knowledgebase.
# Derived entirely from file paths + headings (treemd), no LLM, no state, no cache.
# Output is a pure function of the indexed content: the freshness stamp is the newest
# indexed-file mtime (not wall-clock), so re-running with no content change produces
# byte-identical files (and the script leaves unchanged files untouched).
#
# Generated artifacts:
#   llms.txt              root navigation preamble (evergreen, months, series, recent)
#   YYYY-MM/INDEX.md      per-month index
#   .kb/series/<name>.md  cross-day index per recurring series (auto-detected)
#
# Packaged as myScripts.kbIndex (nixos-config); fd and treemd come from runtimeInputs.
#
# Usage:  kb-index            # full rebuild (≈10s for ~1000 files)

ROOT="${KB_ROOT:-$HOME/Stuff}"

cd "$ROOT"

# Exclude VCS/tooling dirs and the generated INDEX.md files themselves — the latter is
# critical: if generated indexes counted toward the freshness stamp, every run would bump
# its own mtime and never reach a fixed point. (.kb also covers .kb/series/*.md, doubly
# so since fd skips hidden dirs by default.)
EXCLUDES=(-E .git -E node_modules -E .kb -E INDEX.md)

# --- Deterministic freshness stamp: newest mtime across all indexed files ----------
newest_epoch() {
  fd -e md . "${EXCLUDES[@]}" -x stat -c '%Y' {} 2>/dev/null | awk 'NR==1||$1>m{m=$1} END{print m}'
}
EPOCH="$(newest_epoch)"
EPOCH="${EPOCH:-0}"
STAMP="$(date -u -d "@$EPOCH" +%Y-%m-%dT%H:%M:%SZ)"   # global: llms.txt only

# Per-file mtimes, gathered once. Month and series indexes stamp with the newest mtime
# of *their own* files — a global stamp there made one new note anywhere rewrite every
# INDEX.md and series file, defeating write_if_changed.
declare -A MTIME=()
while IFS=$'\t' read -r e f; do
  [ -n "$f" ] && MTIME["${f#./}"]="$e"
done < <(fd -e md . "${EXCLUDES[@]}" -x stat -c '%Y	%n' {} 2>/dev/null)

# scope_stamp < newline-separated paths -> ISO stamp of the newest one
scope_stamp() {
  local f m=0 e
  while IFS= read -r f; do
    [ -z "$f" ] && continue
    e="${MTIME[$f]:-0}"
    (( e > m )) && m="$e"
  done
  date -u -d "@$m" +%Y-%m-%dT%H:%M:%SZ
}

# Shared listing: every indexed md file, root-relative, sorted (path sort == date sort).
ALL_MD="$(fd -e md . "${EXCLUDES[@]}" | sed 's#^\./##' | sort)"
TOTAL_FILES="$(printf '%s\n' "$ALL_MD" | grep -c . || true)"

# --- Helpers -----------------------------------------------------------------------

# title FILE -> first level-1 heading, else humanized filename
derive_title() {
  local f="$1" t
  t="$(treemd -l "$f" 2>/dev/null | grep -m1 '^# ' | sed 's/^# //' || true)"
  if [ -z "$t" ]; then
    t="$(basename "$f" .md | tr '_-' '  ')"
  fi
  printf '%s' "$t"
}

# outline FILE -> up to 6 level-2 headings as indented bullets
derive_outline() {
  local f="$1"
  treemd -l "$f" 2>/dev/null | grep '^## ' | head -6 | sed 's/^## /    - /' || true
}

# recent_key FILE -> grouping dir for the recent list: dirname capped at 3 path
# components (YYYY-MM/DD-name/firstsubdir); "." for root-level files (never collapsed).
recent_key() {
  local f="$1" a b c rest rest2
  a="${f%%/*}"; [ "$a" = "$f" ] && { printf '.'; return; }
  rest="${f#*/}"
  b="${rest%%/*}"; [ "$b" = "$rest" ] && { printf '%s' "$a"; return; }
  rest2="${rest#*/}"
  c="${rest2%%/*}"; [ "$c" = "$rest2" ] && { printf '%s/%s' "$a" "$b"; return; }
  printf '%s/%s/%s' "$a" "$b" "$c"
}

# derive_dir_title DIR -> first H1 of DIR/README.md if present, else humanized dirname
derive_dir_title() {
  local d="$1" t=""
  [ -f "$d/README.md" ] && t="$(treemd -l "$d/README.md" 2>/dev/null | grep -m1 '^# ' | sed 's/^# //' || true)"
  [ -z "$t" ] && t="$(basename "$d" | tr '_-' '  ')"
  printf '%s' "$t"
}

# Atomic write-if-changed: stdin -> $1, only touches the file when content differs.
write_if_changed() {
  local dest="$1" tmp
  tmp="$(mktemp -t claude-code.kbindex.XXXXXX)"
  cat > "$tmp"
  if [ -f "$dest" ] && cmp -s "$tmp" "$dest"; then
    rm -f "$tmp"
  else
    mv "$tmp" "$dest"
    printf '  wrote %s\n' "$dest" >&2
  fi
}

# --- Series detection ----------------------------------------------------------------
# A basename appearing in >=5 *distinct day dirs* across all months is a series
# (e.g. hn-daily.md). README.md is structural, never a series. Counting distinct day
# dirs (not raw occurrences) keeps a single day's fan-out from qualifying. Computed
# before the month loop: build_month_index suppresses outlines for series members.
SERIES_BASENAMES="$(printf '%s\n' "$ALL_MD" \
  | awk -F/ '$1 ~ /^[0-9]{4}-[0-9]{2}$/ && NF>=3 && $NF!="README.md" { print $NF "\t" $1 "/" $2 }' \
  | sort -u \
  | awk -F'\t' '{ c[$1]++ } END { for (b in c) if (c[b]>=5) print b }' \
  | sort)"

declare -A IS_SERIES=()
while IFS= read -r b; do
  [ -z "$b" ] && continue
  IS_SERIES["$b"]=1
done <<< "$SERIES_BASENAMES"

# --- Per-month INDEX.md ------------------------------------------------------------
MONTHS="$(fd -t d -d 1 '^[0-9]{4}-[0-9]{2}$' . | sed 's#^\./##; s#/$##' | sort)"

build_month_index() {
  local month="$1" files prev_day="" rel day
  files="$(printf '%s\n' "$ALL_MD" | grep "^$month/" || true)"

  {
    printf '# Index — %s\n\n' "$month"
    printf '> Auto-generated by `kb-index` — do not edit by hand.\n'
    printf '> Data current through %s · %s files. Headings derived via `treemd`.\n\n' \
      "$(printf '%s\n' "$files" | scope_stamp)" "$(printf '%s\n' "$files" | grep -c . || true)"

    while IFS= read -r f; do
      [ -z "$f" ] && continue
      rel="${f#"$month"/}"           # path relative to the month dir
      day="${rel%%/*}"               # first component = DD-name day dir
      if [ "$day" != "$prev_day" ]; then
        printf '\n## %s\n\n' "$day"
        prev_day="$day"
      fi
      printf -- '- **[%s](%s)** — %s\n' "$rel" "$rel" "$(derive_title "$f")"
      # Series members get a title-only line; their outline lives in .kb/series/.
      if [ -z "${IS_SERIES[${f##*/}]:-}" ]; then
        derive_outline "$f"
      fi
    done <<< "$files"
  } | write_if_changed "$month/INDEX.md"
}

for m in $MONTHS; do
  build_month_index "$m"
done

# --- Per-series cross-day indexes (.kb/series/<name>.md) ----------------------------
build_series_index() {
  local base="$1" files f td title n
  local name="${base%.md}"
  files="$(printf '%s\n' "$ALL_MD" \
    | awk -F/ -v b="$base" '$NF==b && NF>=3 && $1 ~ /^[0-9]{4}-[0-9]{2}$/')"
  n="$(printf '%s\n' "$files" | grep -c . || true)"
  {
    printf '# Series — %s\n\n' "$name"
    printf '> Auto-generated by `kb-index` — do not edit by hand.\n'
    printf '> Data current through %s · %s occurrences, oldest first. Full `##` headings per day.\n' "$(printf '%s\n' "$files" | scope_stamp)" "$n"
    while IFS= read -r f; do
      [ -z "$f" ] && continue
      td="$(treemd -l "$f" 2>/dev/null || true)"   # one capture serves title + headings
      title="$(printf '%s\n' "$td" | grep -m1 '^# ' | sed 's/^# //' || true)"
      [ -z "$title" ] && title="${f##*/}"
      printf '\n## [%s](../../%s) — %s\n\n' "$f" "$f" "$title"
      printf '%s\n' "$td" | grep '^## ' | sed 's/^## /- /' || true
    done <<< "$files"
  } | write_if_changed ".kb/series/$name.md"
}

if [ -n "$SERIES_BASENAMES" ]; then
  mkdir -p .kb/series
  while IFS= read -r b; do
    [ -z "$b" ] && continue
    build_series_index "$b"
  done <<< "$SERIES_BASENAMES"
fi

# Stale cleanup: a basename that dropped below the threshold loses its index file,
# keeping .kb/series/ a pure function of the content.
if [ -d .kb/series ]; then
  for old in .kb/series/*.md; do
    [ -e "$old" ] || continue
    if [ -z "${IS_SERIES[$(basename "$old" .md).md]:-}" ]; then
      rm -f "$old"
      printf '  removed %s\n' "$old" >&2
    fi
  done
fi

# --- Root llms.txt -----------------------------------------------------------------
RECENT="$(fd -e md --changed-within 7d . "${EXCLUDES[@]}" 2>/dev/null \
            | sed 's#^\./##' | grep -v '/INDEX\.md$' | sort || true)"
ROOT_DOCS="$(fd -e md -d 1 . | sed 's#^\./##' | grep -vE '^(INDEX|llms)\.md$' | sort || true)"
EVERGREEN="$(printf '%s\n' "$ALL_MD" | grep '^evergreen/[^/]*\.md$' || true)"

{
  printf '# ~/Stuff — Knowledge Base Navigation\n\n'
  printf '> Auto-generated by `kb-index` — do not edit by hand.\n'
  printf '> Data current through %s · %s markdown files.\n\n' "$STAMP" "$TOTAL_FILES"

  printf 'Date-organized personal knowledgebase: `YYYY-MM/DD-name/` working dirs.\n'
  printf 'Start here, then drill into a month'\''s `INDEX.md`.\n\n'

  printf '## Navigate (treemd, fd, rg are on PATH)\n\n'
  printf -- '- Headings of a file:  `treemd -l FILE`  (`--tree` hierarchy, `-s "Sec" FILE` to extract)\n'
  printf -- '- Find files:          `fd -e md PATTERN ~/Stuff`\n'
  printf -- '- Full-text search:    `rg "term" ~/Stuff`\n'
  printf -- '- Re-index:            `kb-index`\n\n'

  if [ -n "$EVERGREEN" ]; then
    printf '## Evergreen (curated reference docs)\n\n'
    while IFS= read -r f; do
      [ -z "$f" ] && continue
      printf -- '- [%s](%s) — %s\n' "$f" "$f" "$(derive_title "$f")"
      derive_outline "$f"
    done <<< "$EVERGREEN"
    printf '\n'
  fi

  printf '## Months\n\n'
  for m in $MONTHS; do
    n="$(printf '%s\n' "$ALL_MD" | grep -c "^$m/" || true)"
    printf -- '- [%s](%s/INDEX.md) — %s files\n' "$m" "$m" "$n"
  done

  if [ -n "$SERIES_BASENAMES" ]; then
    printf '\n## Series (recurring files with cross-day indexes)\n\n'
    while IFS= read -r b; do
      [ -z "$b" ] && continue
      n="$(printf '%s\n' "$ALL_MD" \
             | awk -F/ -v x="$b" '$NF==x && NF>=3 && $1 ~ /^[0-9]{4}-[0-9]{2}$/' \
             | grep -c . || true)"
      printf -- '- [%s](.kb/series/%s.md) — %s occurrences\n' "${b%.md}" "${b%.md}" "$n"
    done <<< "$SERIES_BASENAMES"
  fi

  if [ -n "$RECENT" ]; then
    printf '\n## Recently updated (last 7 days)\n\n'
    # Collapse noise: group key = recent_key (dir capped at month/day/firstsubdir);
    # groups with >5 recent files become one line at the position of their first
    # member. A day dir with >5 direct files AND >5 under one subdir yields two
    # collapsed lines — accepted for determinism.
    declare -A _rcount=() _remitted=()
    while IFS= read -r f; do
      [ -z "$f" ] && continue
      k="$(recent_key "$f")"
      _rcount["$k"]=$(( ${_rcount["$k"]:-0} + 1 ))
    done <<< "$RECENT"
    while IFS= read -r f; do
      [ -z "$f" ] && continue
      k="$(recent_key "$f")"
      if [ "$k" != "." ] && [ "${_rcount[$k]}" -gt 5 ]; then
        if [ -z "${_remitted[$k]:-}" ]; then
          printf -- '- [%s/](%s/) — %s (%s recent files)\n' \
            "$k" "$k" "$(derive_dir_title "$k")" "${_rcount[$k]}"
          _remitted["$k"]=1
        fi
      else
        printf -- '- [%s](%s) — %s\n' "$f" "$f" "$(derive_title "$f")"
      fi
    done <<< "$RECENT"
  fi

  if [ -f OVERVIEW.md ]; then
    printf '\n## Curated highlights\n\n'
    printf 'See [OVERVIEW.md](OVERVIEW.md) for the hand-written narrative (cross-cutting\n'
    printf 'threads, standout projects). It is curated and may lag this generated index.\n'
  fi

  if [ -n "$ROOT_DOCS" ]; then
    printf '\n## Root documents\n\n'
    while IFS= read -r f; do
      [ -z "$f" ] && continue
      printf -- '- [%s](%s) — %s\n' "$f" "$f" "$(derive_title "$f")"
    done <<< "$ROOT_DOCS"
  fi
} | write_if_changed "llms.txt"

printf 'kb-index: %s files, current through %s\n' "$TOTAL_FILES" "$STAMP" >&2
