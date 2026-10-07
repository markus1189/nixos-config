#!/usr/bin/env bats
# jev.py request shapes via --dry-run (no key, no network) and its pure helpers
# (normalize, disagree) imported as a module. Live behaviour is not covered.

bats_require_minimum_version 1.5.0

setup() {
    bats_load_library bats-support
    bats_load_library bats-assert

    SCRIPT="$BATS_TEST_DIRNAME/../jev/scripts/jev.py"
    cd "$BATS_TEST_TMPDIR"
    printf '%s\n' '{"text":"a"}' '{"text":"b"}' > items.jsonl
    echo '{"u":{"type":"noul","instructions":"`item.text` is urgent."}}' > q.json
    magick -size 8x8 xc:red red.png
    echo 'not an image' > x.txt
    IMG="--model cloudflare/clef --via openrouter --no-zdr"
}

jev() { python3 "$SCRIPT" "$@"; }

@test "jev routes to OpenRouter systemone with ZDR and native state" {
    run jev --dry-run --each items.jsonl --questions q.json
    assert_success
    first=$(head -1 <<<"$output")
    assert_equal "$(jq -r .url <<<"$first")" "https://openrouter.ai/api/v1/systemone"
    assert_equal "$(jq -c .body.provider <<<"$first")" '{"zdr":true,"data_collection":"deny"}'
    assert_equal "$(jq -c .body.state <<<"$first")" '{"item":{"text":"a"}}'
}

@test "clef alias routes to Requesty EU with state as JSON text and a questions format" {
    run jev --dry-run --model clef --each items.jsonl --questions q.json
    assert_success
    first=$(head -1 <<<"$output")
    assert_equal "$(jq -r .url <<<"$first")" "https://router.eu.requesty.ai/v1/chat/completions"
    assert_equal "$(jq -r .body.model <<<"$first")" "sference/clef"
    assert_equal "$(jq -r '.body.messages[0].content' <<<"$first")" '{"item": {"text": "a"}}'
    assert_equal "$(jq -r .body.response_format.type <<<"$first")" "questions"
    assert_equal "$(jq -r '.body.response_format.questions.u.type' <<<"$first")" "noul"
}

@test "--via overrides the inferred backend" {
    run jev --dry-run --model clef --via openrouter --each items.jsonl --questions q.json
    assert_success
    assert_equal "$(head -1 <<<"$output" | jq -r .url)" "https://openrouter.ai/api/v1/systemone"
}

@test "compare mode sends every item to every model" {
    run jev --dry-run --model jev,clef --each items.jsonl --questions q.json
    assert_success
    assert_equal "$(jq -r .body.model <<<"$output" | tr '\n' ' ')" \
        "typesafe/jev-1.13 sference/clef typesafe/jev-1.13 sference/clef "
}

@test "a model listed twice is a usage error" {
    run jev --dry-run --model jev,typesafe/jev-1.13 --each items.jsonl --questions q.json
    assert_failure 2
}

@test "stdin request keeps its own model" {
    run jev --dry-run <<<'{"model":"clef","state":"s","questions":{"u":{"type":"noul","instructions":"i"}}}'
    assert_success
    assert_equal "$(jq -r .body.model <<<"$output")" "sference/clef"
}

@test "images with Jev are refused before any call" {
    run jev --dry-run --model jev,cloudflare/clef --via openrouter --no-zdr --image red.png --each items.jsonl --questions q.json
    assert_failure 2
    assert_output --partial "not supported by: typesafe/jev-1.13"
}

@test "images with sference/clef are refused (sference declares no image input)" {
    run jev --dry-run --model clef --no-zdr --image red.png --each items.jsonl --questions q.json
    assert_failure 2
    assert_output --partial "not supported by: sference/clef"
}

@test "images without --no-zdr are refused" {
    run jev --dry-run --model cloudflare/clef --via openrouter --image red.png --each items.jsonl --questions q.json
    assert_failure 2
    assert_output --partial "images need --no-zdr"
}

@test "a non-image --image file is refused" {
    run jev --dry-run $IMG --image x.txt --each items.jsonl --questions q.json
    assert_failure 2
    assert_output --partial "not PNG, JPEG or WebP"
}

@test "five global images exceed the limit and are refused" {
    run jev --dry-run $IMG --image red.png --image red.png --image red.png --image red.png --image red.png \
        --each items.jsonl --questions q.json
    assert_failure 2
    assert_output --partial "5 images > 4"
}

@test "a bad per-item image fails only that item; base64 is elided" {
    printf '%s\n' '{"t":"a","_images":["red.png"]}' '{"t":"b","_images":["x.txt"]}' > img.jsonl
    run --separate-stderr jev --dry-run $IMG --each img.jsonl --questions q.json
    assert_success
    first=$(head -1 <<<"$output")
    [[ "$(jq -r '.body.state[1].image_url.url' <<<"$first")" =~ ^data:image/png\;base64,\<[0-9]+\ bytes\>$ ]]
    assert_equal "$(jq -r '.body.state[0].text' <<<"$first")" '{"item": {"t": "a"}}'
    assert_equal "$(jq -c '.body.provider' <<<"$first")" '{"data_collection":"deny"}'
    assert_equal "$(sed -n 2p <<<"$output" | jq -r .error.code)" "image"
}

@test "per-item images over the limit warn but are still sent" {
    printf '%s\n' '{"t":"a","_images":["red.png","red.png","red.png","red.png","red.png"]}' > img.jsonl
    run --separate-stderr jev --dry-run $IMG --each img.jsonl --questions q.json
    assert_success
    assert_equal "$(jq '.body.state | length' <<<"$output")" "6"
    [[ "$stderr" == *"item 0: 5 images > 4; sending anyway"* ]]
}

@test "normalize turns a Requesty completion into the systemone shape" {
    run python3 -c '
import sys, json; sys.path.insert(0, sys.argv[1]); import jev
d = {"model": "Cloudflare/clef", "usage": {"prompt_tokens": 7, "cost": 1e-06},
     "choices": [{"message": {"content": "{\"u\": {\"type\": \"noul\", \"noul\": 0.9}}"}}]}
print(json.dumps(jev.normalize("requesty", d), sort_keys=True))' "$(dirname "$SCRIPT")"
    assert_success
    assert_output '{"answers": {"u": {"noul": 0.9, "type": "noul"}}, "model": "Cloudflare/clef", "usage": {"cost": 1e-06, "input_tokens": 7}}'
}

@test "disagree: noul across 0.5, different choice, normalized score gap >= 0.25" {
    run python3 -c '
import sys; sys.path.insert(0, sys.argv[1]); import jev
qs = {"n": {"type": "noul"}, "n2": {"type": "noul"}, "c": {"type": "choice"},
      "s": {"type": "score", "criteria": ["a", "b", "c", "d"]}, "s2": {"type": "score", "criteria": ["a", "b", "c", "d"]}}
a = {"n": {"noul": 0.4}, "n2": {"noul": 0.6}, "c": {"choice": "x"}, "s": {"score": 0.11}, "s2": {"score": 0.05}}
b = {"n": {"noul": 0.6}, "n2": {"noul": 0.9}, "c": {"choice": "y"}, "s": {"score": 1.21}, "s2": {"score": 0.56}}
print(jev.disagree(qs, {"m1": a, "m2": b}))' "$(dirname "$SCRIPT")"
    assert_success
    assert_output "['n', 'c', 's']"
}

@test "an oversized --image is shrunk to 1024 px JPEG within the byte budget and reported on stderr" {
    magick -size 1500x1500 xc: +noise Random big.png
    run --separate-stderr jev --dry-run $IMG --image big.png --each items.jsonl --questions q.json
    assert_success
    [[ "$stderr" == *"downscaled big.png: 1500x1500 png "*" -> "*" jpeg "* ]]
    url=$(head -1 <<<"$output" | jq -r '.body.state[1].image_url.url')
    [[ "$url" =~ ^data:image/jpeg\;base64,\<([0-9]+)\ bytes\>$ ]]
    (( BASH_REMATCH[1] <= 384 * 1024 ))
}

@test "per-item downscaling is reported in the item's line, small images pass untouched" {
    magick -size 1500x1500 xc: +noise Random big.png
    printf '%s\n' '{"t":"a","_images":["big.png"]}' '{"t":"b","_images":["red.png"]}' > img.jsonl
    run --separate-stderr jev --dry-run $IMG --each img.jsonl --questions q.json
    assert_success
    [[ "$(head -1 <<<"$output" | jq -r '.downscaled[0]')" == "big.png: 1500x1500 png "*" -> "*" jpeg "* ]]
    assert_equal "$(sed -n 2p <<<"$output" | jq -r '.downscaled')" "null"
    [[ "$(sed -n 2p <<<"$output" | jq -r '.body.state[1].image_url.url')" == data:image/png\;base64,* ]]
}

@test "--no-downscale refuses an --image over the byte budget" {
    magick -size 1500x1500 xc: +noise Random big.png
    run jev --dry-run $IMG --no-downscale --image big.png --each items.jsonl --questions q.json
    assert_failure 2
    assert_output --partial "> 384 KiB (every request would fail)"
}

@test "capability refusals suggest --do-it-anyway, which sends with a warning and keeps the images" {
    run jev --dry-run --model clef --no-zdr --image red.png --each items.jsonl --questions q.json
    assert_failure 2
    assert_output --partial "retry with --do-it-anyway"
    run --separate-stderr jev --dry-run --model clef --do-it-anyway --image red.png --each items.jsonl --questions q.json
    assert_success
    [[ "$stderr" == *"--do-it-anyway: sending although images need"*"sference/clef"* ]]
    [[ "$stderr" != *"images need --no-zdr"* ]]
    assert_equal "$(head -1 <<<"$output" | jq -r '.body.messages[0].content[1].type')" "image_url"
}

@test "--do-it-anyway keeps zero data retention when --no-zdr is missing" {
    run --separate-stderr jev --dry-run --model cloudflare/clef --via openrouter --do-it-anyway --image red.png \
        --each items.jsonl --questions q.json
    assert_success
    assert_equal "$(head -1 <<<"$output" | jq -c .body.provider)" '{"zdr":true,"data_collection":"deny"}'
}

@test "--do-it-anyway does not bypass usage errors" {
    run jev --dry-run --do-it-anyway --model jev,typesafe/jev-1.13 --each items.jsonl --questions q.json
    assert_failure 2
}
