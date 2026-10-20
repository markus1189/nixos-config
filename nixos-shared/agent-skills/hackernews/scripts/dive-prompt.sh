#!/usr/bin/env nix
#! nix shell nixpkgs#bash nixpkgs#curl nixpkgs#jq nixpkgs#coreutils nixpkgs#gawk --command bash
# shellcheck shell=bash
set -euo pipefail
readonly SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
readonly SKILL_DIR="${SCRIPT_DIR%/scripts}"
readonly API_BASE="https://hacker-news.firebaseio.com/v0"

show_help() {
    cat <<EOF
Usage: ${0##*/} --check N [--dir DIR] ID...
       ${0##*/} --collect FILE [--dir DIR] ID...

Fill deepdive-prompt.md for each story and print one prompt path per ID; give
each path to a subagent as "Read <path> and follow it." The subagent writes
its dive to DIR/ID.out.md.

  --check N       which check of the day the dives belong to
  --collect FILE  append DIR/ID.out.md for every ID to FILE, in argument order;
                  fails without appending anything if one is missing or empty
  --dir DIR       default: \${TMPDIR:-/tmp}/hn-dives/$(date +%F)
EOF
}

die() { echo "Error: $*" >&2; exit 1; }

CHECK="" COLLECT="" DIR="${TMPDIR:-/tmp}/hn-dives/$(date +%F)"
IDS=()
while [[ $# -gt 0 ]]; do
    case $1 in
        -h|--help) show_help; exit 0 ;;
        --check) CHECK="${2:-}"; shift 2 || die "--check requires a number" ;;
        --collect) COLLECT="${2:-}"; shift 2 || die "--collect requires a file" ;;
        --dir) DIR="${2:-}"; shift 2 || die "--dir requires a directory" ;;
        -*) die "unknown option: $1" ;;
        *) [[ "$1" =~ ^[0-9]+$ ]] || die "not a story id: $1"; IDS+=("$1"); shift ;;
    esac
done
((${#IDS[@]})) || die "no story ids given"

if [[ -n "$COLLECT" ]]; then
    missing=()
    for id in "${IDS[@]}"; do
        [[ -s "$DIR/$id.out.md" ]] || missing+=("$DIR/$id.out.md")
    done
    ((${#missing[@]} == 0)) || die "missing or empty, nothing appended: ${missing[*]}"
    for id in "${IDS[@]}"; do
        { echo; cat "$DIR/$id.out.md"; } >> "$COLLECT"
    done
    echo "appended ${#IDS[@]} dives to $COLLECT"
    exit 0
fi

[[ "$CHECK" =~ ^[0-9]+$ ]] || die "--check N is required"
mkdir -p "$DIR"

# The template is everything below the first '---'; above it are notes for us.
template=$(awk 'f; /^---$/ && !f {f=1}' "$SKILL_DIR/deepdive-prompt.md")
profile=$(awk 'NR == 1 {next} !s && /^$/ {next} {s=1; print}' "$SKILL_DIR/references/interests.md")
tags=$(awk '/^### / {f = ($0 == "### Tag vocabulary"); next} f && /^- /' "$SKILL_DIR/references/deep-dives.md")
[[ -n "$profile" && -n "$tags" ]] || die "reader profile or tag vocabulary came out empty"

for id in "${IDS[@]}"; do
    item=$(curl --fail -s --max-time 10 "$API_BASE/item/$id.json") || die "cannot fetch item $id"
    title=$(jq -r '.title // empty' <<< "$item")
    url=$(jq -r '.url // empty' <<< "$item")
    [[ -n "$title" ]] || die "item $id has no title (dead, deleted or a comment?)"

    out="$DIR/$id.out.md"
    rm -f "$out"  # a stale dive from an earlier run must not be collected
    prompt=$template
    if [[ -z "$url" ]]; then
        # No article: drop task 1, renumber the later tasks, drop the domain link.
        prompt=$(awk '
            /^## / {tasks = ($0 == "## Tasks")}
            tasks && /^1\. Fetch the article/ {skip = 1; next}
            skip && /^   / {next}
            {skip = 0}
            tasks && match($0, /^([2-9])\. /, m) {$0 = (m[1] - 1) substr($0, 2)}
            {gsub(/ · \[bare\.domain\]\(\{\{ARTICLE_URL\}\}\)/, ""); print}
        ' <<< "$prompt")
    fi
    jq -Rrs --arg id "$id" --arg url "$url" --arg title "$title" --arg check "$CHECK" \
        --arg cli "$SCRIPT_DIR/hn-cli.sh" --arg profile "$profile" --arg tags "$tags" \
        --arg out "$out" '
        gsub("\\{\\{STORY_ID\\}\\}"; $id) | gsub("\\{\\{ARTICLE_URL\\}\\}"; $url)
        | gsub("\\{\\{STORY_TITLE\\}\\}"; $title) | gsub("\\{\\{CHECK_N\\}\\}"; $check)
        | gsub("\\{\\{HN_CLI\\}\\}"; $cli) | gsub("\\{\\{READER_PROFILE\\}\\}"; $profile)
        | gsub("\\{\\{TAG_VOCAB\\}\\}"; $tags) | gsub("\\{\\{OUT_FILE\\}\\}"; $out)
    ' <<< "$prompt" > "$DIR/$id.prompt.md"
    if grep -q '{{' "$DIR/$id.prompt.md"; then
        die "unfilled placeholder in $DIR/$id.prompt.md"
    fi
    echo "$DIR/$id.prompt.md"
done
