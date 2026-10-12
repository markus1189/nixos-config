#!/usr/bin/env bats
# hackernews' hn-cli.sh against synthetic fixtures. curl is stubbed on PATH,
# so nothing here touches the network. Fixture story 100 is built to tell the
# orderings apart: comments posted 101 < 102 < 103 < 104, HN ranks them
# 103, 101, 104, 102; 102 is deleted but keeps a live reply.

bats_require_minimum_version 1.5.0

setup() {
    bats_load_library bats-support
    bats_load_library bats-assert

    SCRIPT="$BATS_TEST_DIRNAME/../hackernews/scripts/hn-cli.sh"
    FIX="$BATS_TEST_DIRNAME/fixtures/hackernews"

    STUBS="$BATS_TEST_TMPDIR/stubs"
    mkdir -p "$STUBS"
    export FAKE_CURL_DIR="$BATS_TEST_TMPDIR/bodies"
    export FAKE_CURL_LOG="$BATS_TEST_TMPDIR/curl.log"
    mkdir -p "$FAKE_CURL_DIR"
    : > "$FAKE_CURL_LOG"

    # Fake curl: logs the URL and every --data-urlencode pair on one line,
    # answers from $FAKE_CURL_DIR/<route>, exit 22 (curl --fail) if absent.
    # Shebang from the real bash: no /usr/bin/env in the build sandbox.
    cat > "$STUBS/curl" <<EOF
#!$(command -v bash)
url=""; params=""
while [ \$# -gt 0 ]; do
    case "\$1" in
        --data-urlencode) params="\$params \$2"; shift 2 ;;
        --max-time) shift 2 ;;
        -*) shift ;;
        *) url="\$1"; shift ;;
    esac
done
echo "\$url\$params" >> "\$FAKE_CURL_LOG"
case "\$url" in
    *hn.algolia.com/api/v1/items/*) route="algolia-item-\${url##*/}" ;;
    *hn.algolia.com/api/v1/search*) route="algolia-search" ;;
    *firebaseio.com/v0/item/*) id="\${url##*/}"; route="firebase-item-\${id%.json}" ;;
    *) route="unknown" ;;
esac
[ -f "\$FAKE_CURL_DIR/\$route" ] || exit 22
cat "\$FAKE_CURL_DIR/\$route"
EOF
    chmod +x "$STUBS/curl"
    export PATH="$STUBS:$PATH"
}

hn() {
    bash "$SCRIPT" "$@"
}

answer() { # answer ROUTE FIXTURE
    cp "$FIX/$2" "$FAKE_CURL_DIR/$1"
}

thread_fixture() {
    answer algolia-item-100 algolia-item-100.json
    answer firebase-item-100 firebase-item-100.json
}

# --- thread ------------------------------------------------------------------

@test "thread: top-level comments follow HN rank, not posting order" {
    thread_fixture
    run hn -t 100
    assert_success
    run grep -o '^▸ u[0-9]*' <<< "$output"
    assert_output $'▸ u103\n▸ u101\n▸ u104'
}

@test "thread: the -n budget takes every top-level comment before any reply" {
    thread_fixture
    run hn -t 100 -n 4
    assert_success
    assert_output --partial "showing 4 of 8"
    assert_output --partial "▸ u104 [104]"
    # first reply layer in ranked-parent order: 131 (under 103) before 111
    assert_output --partial "  ▸ u131 [131]"
    refute_output --partial "[111]"
    refute_output --partial "[1311]"
    assert_output --partial "[truncated: 4 deeper comments left out"
}

@test "thread: replies under a deleted comment stay reachable" {
    thread_fixture
    run hn -t 100
    assert_success
    assert_output --partial "  ▸ u121 [121]"
    refute_output --partial "[102]"
}

@test "thread: without HN rank it falls back to oldest first and says so" {
    answer algolia-item-100 algolia-item-100.json
    run hn -t 100
    assert_success
    assert_output --partial "oldest first (HN rank unavailable)"
    run grep -o '^▸ u[0-9]*' <<< "$output"
    assert_output $'▸ u101\n▸ u103\n▸ u104'
}

@test "thread: --since keeps new comments and shows ancestors as context" {
    thread_fixture
    run hn -t 100 --since @1800000000
    assert_success
    assert_output --partial "1 new since"
    assert_output --partial "▸ u103 [103] · 1970-01-01 00:50Z · (context)"
    assert_output --partial "    ▸ u1311 [1311]"
    refute_output --partial "[101]"
}

# --- search ------------------------------------------------------------------

@test "search: exact matching unless --fuzzy" {
    answer algolia-search search-empty.json
    run hn -s arc42
    assert_success
    run grep -c 'typoTolerance=false' "$FAKE_CURL_LOG"
    assert_output 1
    : > "$FAKE_CURL_LOG"
    run hn -s arc42 --fuzzy
    assert_success
    refute grep -q typoTolerance "$FAKE_CURL_LOG"
}

@test "search: filters become one numericFilters param plus title restriction" {
    answer algolia-search search-empty.json
    run hn -s architecture --since @1700000000 --min-points 50 --title-only
    assert_success
    assert_output --partial "since 2023-11-14 · points >= 50 · title only"
    run cat "$FAKE_CURL_LOG"
    assert_output --partial "numericFilters=created_at_i>=1700000000,points>=50"
    assert_output --partial "restrictSearchableAttributes=title"
    assert_output --partial "tags=story"
}

@test "search: --in comments prints hit, its story and a link, cleaned of HTML" {
    answer algolia-search search-comments.json
    run hn -s arc42 --in comments
    assert_success
    assert_output --partial "speedbird [555] · 2026-06-14 10:06Z"
    assert_output --partial "on: Software Architecture Guide (2019) [48524084]"
    assert_output --partial "Use a template like ARC42. Agents can review & index it: https://example.com/x"
    assert_output --partial "https://news.ycombinator.com/item?id=555"
    run grep -c 'tags=comment' "$FAKE_CURL_LOG"
    assert_output 1
}

@test "search: story hits are rendered from Firebase" {
    answer algolia-search search-stories.json
    answer firebase-item-200 firebase-item-200.json
    run hn -s arc42
    assert_success
    assert_output --partial "Documenting software architecture with arc42 (innoq.com) [200]"
    assert_output --partial "146 points"
}

@test "search: story-only filters are refused for comments, before any request" {
    run hn -s arc42 --in comments --min-points 5
    assert_failure
    assert_output --partial "--min-points works only on stories"
    run hn -s arc42 --in comments --title-only
    assert_failure
    assert_output --partial "--title-only works only on stories"
    [ ! -s "$FAKE_CURL_LOG" ]
}

@test "search: no hits explains that matching is exact" {
    answer algolia-search search-empty.json
    run hn -s nothing-matches
    assert_success
    assert_output --partial "Matching is exact"
}
