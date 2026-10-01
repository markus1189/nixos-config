#!/usr/bin/env bats
# nzb-search's nzb-api.sh against recorded-shape fixtures. curl and pass are
# stubbed on PATH, so nothing here touches the network or the password store.
# The fixtures are synthesized from the shapes normalize_response documents,
# not captured from live indexers.

bats_require_minimum_version 1.5.0

setup() {
    bats_load_library bats-support
    bats_load_library bats-assert

    SCRIPT="$BATS_TEST_DIRNAME/../nzb-search/scripts/nzb-api.sh"
    FIX="$BATS_TEST_DIRNAME/fixtures/nzb-search"

    STUBS="$BATS_TEST_TMPDIR/stubs"
    mkdir -p "$STUBS"
    export FAKE_CURL_DIR="$BATS_TEST_TMPDIR/bodies"
    export FAKE_CURL_LOG="$BATS_TEST_TMPDIR/curl.log"
    mkdir -p "$FAKE_CURL_DIR"
    : > "$FAKE_CURL_LOG"

    # Fake curl: logs the URL, answers with $FAKE_CURL_DIR/<host> (or
    # default), honours -o. Shebang from the real bash: no /usr/bin/env in
    # the build sandbox.
    cat > "$STUBS/curl" <<EOF
#!$(command -v bash)
out=""; url=""
while [ \$# -gt 0 ]; do
    case "\$1" in
        -o) out="\$2"; shift 2 ;;
        -*) shift ;;
        *) url="\$1"; shift ;;
    esac
done
echo "\$url" >> "\$FAKE_CURL_LOG"
host="\${url#https://}"; host="\${host%%/*}"
body="\$FAKE_CURL_DIR/\$host"
[ -f "\$body" ] || body="\$FAKE_CURL_DIR/default"
[ -f "\$body" ] || exit 22
if [ -n "\$out" ]; then cat "\$body" > "\$out"; else cat "\$body"; fi
EOF
    printf '#!%s\necho testkey\n' "$(command -v bash)" > "$STUBS/pass"
    chmod +x "$STUBS/curl" "$STUBS/pass"
    export PATH="$STUBS:$PATH"
    export NZB_DIR="$BATS_TEST_TMPDIR/nzbs"
}

nzb() {
    bash "$SCRIPT" "$@"
}

answer() { # answer HOST FIXTURE
    cp "$FIX/$2" "$FAKE_CURL_DIR/$1"
}

# --- dispatch ---------------------------------------------------------------

@test "unknown command fails instead of becoming a raw API call" {
    run nzb serch inception
    assert_failure
    assert_output --partial "unknown command 'serch'"
    [ ! -s "$FAKE_CURL_LOG" ]
}

@test "unknown indexer fails" {
    run nzb @nope search x
    assert_failure
    assert_output --partial "unknown indexer 'nope'"
}

@test "list_indexers needs no API key" {
    rm "$STUBS/pass"
    run nzb list_indexers
    assert_success
    assert_output --partial "treasuremaps (default)"
}

# --- request building (dry run) ---------------------------------------------

@test "queries are UTF-8 percent-encoded" {
    NZB_DRY_RUN=1 run nzb search "Die Brüder" "&cat=7120"
    assert_success
    assert_output "https://treasure-maps.com/api?t=search&q=Die%20Br%C3%BCder&cat=7120&o=json"
}

@test "movie forwards extra params" {
    NZB_DRY_RUN=1 run nzb movie "inception" "&cat=2000&limit=20&extended=1"
    assert_output --partial "t=movie&title=inception&cat=2000&limit=20&extended=1"
}

@test "movie accepts IMDb IDs with tt prefix" {
    NZB_DRY_RUN=1 run nzb movie "tt1375666" "&extended=1"
    assert_output --partial "t=movie&imdbid=1375666&extended=1"
}

@test "tvsearch takes season, episode and extra params" {
    NZB_DRY_RUN=1 run nzb tvsearch "the wire" 2 5 "&extended=1"
    assert_output --partial "t=tvsearch&q=the%20wire&season=2&ep=5&extended=1"
}

@test "tvsearch with extras but no season" {
    NZB_DRY_RUN=1 run nzb tvsearch "the wire" "&cat=5000"
    assert_output --partial "t=tvsearch&q=the%20wire&cat=5000&o=json"
}

@test "book forwards extra params" {
    NZB_DRY_RUN=1 run nzb @nzbgeek book "tolkien" "&limit=10"
    assert_output "https://api.nzbgeek.info/api?t=book&q=tolkien&limit=10&o=json"
}

# --- normalization + results ------------------------------------------------

@test "search tags items with their indexer" {
    answer treasure-maps.com treasuremaps.json
    run nzb search inception
    assert_success
    run jq -r '[.channel.item[].indexer] | unique | join(",")' <<< "$output"
    assert_output "treasuremaps"
}

@test "results flattens Treasure Maps attrs, sorted by grabs" {
    answer treasure-maps.com treasuremaps.json
    run bash -c "bash '$SCRIPT' search inception | bash '$SCRIPT' results"
    assert_success
    run jq -c '[.[] | [.guid, .grabs, .size_gb, .resolution, .subs, .category]]' <<< "$output"
    assert_output '[["bbb222",340,10,"1080p",null,"2040"],["aaa111",12,4,"720p","English","Movies > HD"]]'
}

@test "results handles a single-item object without grabs, WxH resolution" {
    answer treasure-maps.com single.json
    run bash -c "bash '$SCRIPT' search one | bash '$SCRIPT' results"
    assert_success
    run jq -c '[.[] | [.guid, .size_gb, .resolution, .grabs]]' <<< "$output"
    assert_output '[["ccc333",1,"2160p",null]]'
}

@test "results reads NZBFinder's RSS XML (it ignores o=json)" {
    answer nzbfinder.ws nzbfinder.xml
    run bash -c "bash '$SCRIPT' @nzbfinder search show | bash '$SCRIPT' results"
    assert_success
    run jq -c '.[0] | [.indexer, .guid, .size_gb, .grabs, .resolution, .category]' <<< "$output"
    assert_output '["nzbfinder","0bbfda1f-3941-49dd-86d4-ab148782ff0d",2,7,"1080p","5040"]'
}

@test "results reads DrunkenSlug's unwrapped, underscore-keyed shape" {
    answer drunkenslug.com drunkenslug.json
    run bash -c "bash '$SCRIPT' @drunkenslug search book | bash '$SCRIPT' results"
    assert_success
    run jq -c '.[0] | [.indexer, .guid, .size_gb, .grabs, .category]' <<< "$output"
    assert_output '["drunkenslug","eee555",0,3,"7020"]'
}

@test "results --sort size and --table" {
    answer treasure-maps.com treasuremaps.json
    run bash -c "bash '$SCRIPT' search inception | bash '$SCRIPT' results --sort size --table"
    assert_success
    assert_line --index 0 "1. Inception.2010.German.DL.1080p.BluRay.x264-GRP"
    assert_line --index 1 "   10 GB · grabs 340 · 1080p · treasuremaps"
    assert_line --index 3 "2. Inception.2010.720p.BluRay.x264-GRP"
    assert_line --index 4 "   4 GB · grabs 12 · 720p · subs: English · treasuremaps"
}

@test "results rejects an unknown sort key" {
    run bash -c "echo '{}' | bash '$SCRIPT' results --sort date"
    assert_failure
    assert_output --partial "--sort must be"
}

# --- errors fail loudly ------------------------------------------------------

@test "JSON error object exits non-zero with its description" {
    answer treasure-maps.com error.json
    run nzb search inception
    assert_failure
    assert_output "Error: treasuremaps: Incorrect user credentials"
}

@test "Treasure Maps' bare @attributes error exits non-zero" {
    answer treasure-maps.com treasuremaps-badkey.json
    run nzb search inception
    assert_failure
    assert_output "Error: treasuremaps: Incorrect user credentials"
}

@test "NZBgeek's bad-key account status exits non-zero" {
    answer api.nzbgeek.info nzbgeek-badkey.json
    run nzb @nzbgeek search inception
    assert_failure
    assert_output "Error: nzbgeek: Invalid API Key"
}

@test "XML error under o=json exits non-zero with its description" {
    answer nzbfinder.ws error.xml
    run nzb @nzbfinder search inception
    assert_failure
    assert_output "Error: nzbfinder: Request limit reached"
}

# --- download ----------------------------------------------------------------

@test "download defaults to NZB_DIR/GUID.nzb" {
    answer treasure-maps.com valid.nzb
    run nzb download aaa111
    assert_success
    assert_output "$NZB_DIR/aaa111.nzb"
    grep -q '<nzb' "$NZB_DIR/aaa111.nzb"
}

@test "download refuses an error page posing as an NZB" {
    answer treasure-maps.com error.xml
    run nzb download aaa111 "$BATS_TEST_TMPDIR/movie.nzb"
    assert_failure
    assert_output "Error: treasuremaps: Request limit reached"
    [ ! -e "$BATS_TEST_TMPDIR/movie.nzb" ]
}

# --- search_all --------------------------------------------------------------

@test "search_all merges indexers, skips nzbfinder and survives failures" {
    answer treasure-maps.com treasuremaps.json
    answer drunkenslug.com drunkenslug.json
    answer api.nzbgeek.info error.json
    answer api.nzbplanet.net single.json
    answer nzbfinder.ws nzbfinder.xml
    run --separate-stderr nzb search_all inception
    assert_success
    run jq -r '[.channel.item[].indexer] | unique | join(",")' <<< "$output"
    assert_output "drunkenslug,nzbplanet,treasuremaps"
    run ! grep -q nzbfinder.ws "$FAKE_CURL_LOG"
}

@test "search_all output works with results" {
    answer treasure-maps.com treasuremaps.json
    answer drunkenslug.com drunkenslug.json
    answer api.nzbgeek.info single.json
    answer api.nzbplanet.net single.json
    run bash -c "bash '$SCRIPT' search_all x 2>/dev/null | bash '$SCRIPT' results"
    assert_success
    run jq 'length' <<< "$output"
    assert_output "5"
}

@test "search_all --include nzbfinder queries it" {
    answer nzbfinder.ws nzbfinder.xml
    answer default single.json
    run --separate-stderr nzb search_all --include nzbfinder show
    assert_success
    grep -q nzbfinder.ws "$FAKE_CURL_LOG"
}
