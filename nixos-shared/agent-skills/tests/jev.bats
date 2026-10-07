#!/usr/bin/env bats
# jev.py request shapes via --dry-run, its helpers and call() imported as a module against
# scripted fake responses (fixtures/jev/fake.py), and main() end to end against a local stub of
# both APIs (fixtures/jev/stub.py). Nothing touches the real providers.

bats_require_minimum_version 1.5.0

setup() {
    bats_load_library bats-support
    bats_load_library bats-assert

    SCRIPT="$BATS_TEST_DIRNAME/../jev/scripts/jev.py"
    export JEV_DIR="$(dirname "$SCRIPT")" PYTHONDONTWRITEBYTECODE=1
    export PYTHONPATH="$JEV_DIR"
    JM=$(python3 -c 'import jev; print(jev.MODEL)')
    FIX="$BATS_TEST_DIRNAME/fixtures/jev"
    # Nothing may reach the real APIs: no keys, a pass that always fails, and every non-loopback
    # request sent to a dead proxy. stub() points the script at 127.0.0.1 with dummy keys.
    unset REQUESTY_BASE_URL OPENROUTER_BASE_URL OPENROUTER_API_KEY REQUESTY_API_KEY
    mkdir -p "$BATS_TEST_TMPDIR/nopass"
    printf '#!%s\nexit 1\n' "$(command -v bash)" > "$BATS_TEST_TMPDIR/nopass/pass"
    chmod +x "$BATS_TEST_TMPDIR/nopass/pass"
    export PATH="$BATS_TEST_TMPDIR/nopass:$PATH"
    export http_proxy=http://127.0.0.1:9 https_proxy=http://127.0.0.1:9 no_proxy=127.0.0.1
    cd "$BATS_TEST_TMPDIR"
    printf '%s\n' '{"text":"a"}' '{"text":"b"}' > items.jsonl
    echo '{"u":{"type":"noul","instructions":"`item.text` is urgent."}}' > q.json
    magick -size 8x8 xc:red red.png
    echo 'not an image' > x.txt
    IMG="--model cloudflare/clef --via openrouter --no-zdr"
}

jev() { python3 "$SCRIPT" "$@"; }

# fake RESPONSES_JSON [BACKEND] [QUESTIONS_JSON]: jev.call() against scripted responses.
fake() { python3 "$FIX/fake.py" "[${1}, \"${2:-openrouter}\", ${3:-null}]"; }

# stub SCRIPT_JSON: start the local API stub; jev() then talks to it with dummy keys.
stub() {
    echo "$1" > stub.json
    python3 "$FIX/stub.py" stub.json port &
    STUB_PID=$!
    for _ in $(seq 50); do [[ -s port ]] && break; sleep 0.1; done
    [[ -s port ]] || { echo "stub failed to start" >&2; return 1; }
    export OPENROUTER_BASE_URL="http://127.0.0.1:$(cat port)/v1" REQUESTY_BASE_URL="http://127.0.0.1:$(cat port)/v1"
    export OPENROUTER_API_KEY=or-key REQUESTY_API_KEY=rq-key
}

teardown() { [[ -n "${STUB_PID:-}" ]] && kill "$STUB_PID" 2>/dev/null || true; }

@test "jev routes to OpenRouter systemone with ZDR and native state" {
    run jev --dry-run --each items.jsonl --questions q.json
    assert_success
    first=$(head -1 <<<"$output")
    assert_equal "$(jq -r .url <<<"$first")" "https://openrouter.ai/api/v1/systemone"
    assert_equal "$(jq -c .body.provider <<<"$first")" '{"zdr":true,"data_collection":"deny"}'
    assert_equal "$(jq -c .body.state <<<"$output" | tr '\n' ' ')" '{"item":{"text":"a"}} {"item":{"text":"b"}} '
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
    assert_equal "$(sed -n 2p <<<"$output" | jq -r '.body.messages[0].content')" '{"item": {"text": "b"}}'
}

@test "--via overrides the inferred backend" {
    run jev --dry-run --model clef --via openrouter --no-zdr --each items.jsonl --questions q.json
    assert_success
    assert_equal "$(head -1 <<<"$output" | jq -r .url)" "https://openrouter.ai/api/v1/systemone"
}

@test "compare mode sends every item to every model" {
    run jev --dry-run --model jev,clef --each items.jsonl --questions q.json
    assert_success
    assert_equal "$(jq -r .body.model <<<"$output" | tr '\n' ' ')" "$JM sference/clef $JM sference/clef "
}

@test "a model listed twice is a usage error" {
    run jev --dry-run --model "jev,$JM" --each items.jsonl --questions q.json
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
    assert_output --partial "not supported by: $JM"
}

@test "images with sference/clef are refused (sference declares no image input)" {
    run jev --dry-run --model clef --no-zdr --image red.png --each items.jsonl --questions q.json
    assert_failure 2
    assert_output --partial "not supported by: sference/clef"
}

@test "Clef via OpenRouter without --no-zdr is refused, with or without images" {
    run jev --dry-run --model cloudflare/clef --via openrouter --image red.png --each items.jsonl --questions q.json
    assert_failure 2
    assert_output --partial "needs --no-zdr"
    run jev --dry-run --model cloudflare/clef --via openrouter --each items.jsonl --questions q.json
    assert_failure 2
    assert_output --partial "needs --no-zdr"
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
    assert_failure 1
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
    magick -size 1500x1500 -seed 1 xc: +noise Random big.png
    run --separate-stderr jev --dry-run $IMG --image big.png --each items.jsonl --questions q.json
    assert_success
    [[ "$stderr" == *"downscaled big.png: 1500x1500 png "*" -> "*" jpeg "* ]]
    url=$(head -1 <<<"$output" | jq -r '.body.state[1].image_url.url')
    [[ "$url" =~ ^data:image/jpeg\;base64,\<([0-9]+)\ bytes\>$ ]]
    (( BASH_REMATCH[1] <= 360 * 1024 ))
}

@test "per-item downscaling is reported in the item's line, small images pass untouched" {
    magick -size 1500x1500 -seed 1 xc: +noise Random big.png
    printf '%s\n' '{"t":"a","_images":["big.png"]}' '{"t":"b","_images":["red.png"]}' > img.jsonl
    run --separate-stderr jev --dry-run $IMG --each img.jsonl --questions q.json
    assert_success
    [[ "$(head -1 <<<"$output" | jq -r '.downscaled[0]')" == "big.png: 1500x1500 png "*" -> "*" jpeg "* ]]
    assert_equal "$(sed -n 2p <<<"$output" | jq -r '.downscaled')" "null"
    [[ "$(sed -n 2p <<<"$output" | jq -r '.body.state[1].image_url.url')" == data:image/png\;base64,* ]]
}

@test "--no-downscale refuses an --image over the byte budget" {
    magick -size 1500x1500 -seed 1 xc: +noise Random big.png
    run jev --dry-run $IMG --no-downscale --image big.png --each items.jsonl --questions q.json
    assert_failure 2
    assert_output --partial "> 360 KiB (every request would fail)"
}

@test "capability refusals suggest --do-it-anyway, which sends with a warning and keeps the images" {
    run jev --dry-run --model clef --no-zdr --image red.png --each items.jsonl --questions q.json
    assert_failure 2
    assert_output --partial "retry with --do-it-anyway"
    run --separate-stderr jev --dry-run --model clef --do-it-anyway --image red.png --each items.jsonl --questions q.json
    assert_success
    [[ "$stderr" == *"--do-it-anyway: sending although images need"*"sference/clef"* ]]
    [[ "$stderr" != *"needs --no-zdr"* ]]
    assert_equal "$(head -1 <<<"$output" | jq -r '.body.messages[0].content[1].type')" "image_url"
    assert_equal "$(head -1 <<<"$output" | jq -r '.body.messages[0].content[0].text')" '{"item": {"text": "a"}}'
}

@test "--do-it-anyway keeps zero data retention when --no-zdr is missing" {
    run --separate-stderr jev --dry-run --model cloudflare/clef --via openrouter --do-it-anyway --image red.png \
        --each items.jsonl --questions q.json
    assert_success
    assert_equal "$(head -1 <<<"$output" | jq -c .body.provider)" '{"zdr":true,"data_collection":"deny"}'
}

@test "--do-it-anyway does not bypass usage errors" {
    run jev --dry-run --do-it-anyway --model "jev,$JM" --each items.jsonl --questions q.json
    assert_failure 2
}

# --- routing and arguments ---

@test "~typesafe/* and any case of typesafe/ route to OpenRouter with ZDR" {
    for m in '~typesafe/jev-latest' 'TypeSafe/jev-1.13'; do
        run jev --dry-run --model "$m" --each items.jsonl --questions q.json
        assert_success
        assert_equal "$(head -1 <<<"$output" | jq -r .url)" "https://openrouter.ai/api/v1/systemone"
        assert_equal "$(head -1 <<<"$output" | jq -c .body.provider.zdr)" "true"
    done
}

@test "--no-zdr in a compare run drops ZDR for Clef only, never for Jev" {
    run jev --dry-run --model jev,cloudflare/clef --via openrouter --no-zdr --each items.jsonl --questions q.json
    assert_success
    assert_equal "$(head -2 <<<"$output" | jq -c '[.body.model, .body.provider.zdr]' | tr '\n' ' ')" \
        "[\"$JM\",true] [\"cloudflare/clef\",null] "
}

@test "a request's model conflicting with --model is a usage error; agreeing is fine" {
    req='{"model":"clef","state":"s","questions":{"u":{"type":"noul","instructions":"i"}}}'
    run jev --dry-run --model jev,clef <<<"$req"
    assert_failure 2
    assert_output --partial "model given twice"
    run jev --dry-run --model sference/clef <<<"$req"
    assert_success
    run jev --dry-run <<<'{"model":["x"],"state":"s","questions":{"u":{"type":"noul","instructions":"i"}}}'
    assert_failure 2
}

@test "--questions or --context without --each is a usage error" {
    run jev --dry-run --questions q.json <<<'{"state":"s","questions":{"u":{"type":"noul","instructions":"i"}}}'
    assert_failure 2
}

@test "empty --model and --jobs 0 are usage errors" {
    run jev --dry-run --model , --each items.jsonl --questions q.json
    assert_failure 2
    run jev --dry-run -j 0 --each items.jsonl --questions q.json
    assert_failure 2
}

@test "sference gets at most 16 questions unless forced" {
    python3 -c 'import json; print(json.dumps({f"q{i}": {"type": "noul", "instructions": "x"} for i in range(17)}))' > q17.json
    run jev --dry-run --model clef --each items.jsonl --questions q17.json
    assert_failure 2
    assert_output --partial "17 questions > 16"
    run jev --dry-run --model jev --each items.jsonl --questions q17.json
    assert_success
    run --separate-stderr jev --dry-run --model clef --do-it-anyway --each items.jsonl --questions q17.json
    assert_success
}

@test "a non-UTF-8 --each file is an input error, not a traceback" {
    printf '\xff\xfe{}\n' > bad.jsonl
    run jev --dry-run --each bad.jsonl --questions q.json
    assert_failure 2
    assert_output --partial "not UTF-8"
}

# --- images ---

@test "an empty _images list is no image at all" {
    echo '{"a":1,"_images":[]}' > e.jsonl
    run jev --dry-run --each e.jsonl --questions q.json
    assert_success
}

@test "per-item _images with Jev are refused" {
    echo '{"a":1,"_images":["red.png"]}' > e.jsonl
    run jev --dry-run --each e.jsonl --questions q.json
    assert_failure 2
    assert_output --partial "not supported by: $JM"
}

@test "cloudflare/clef images without --via openrouter are refused" {
    run jev --dry-run --model cloudflare/clef --no-zdr --image red.png --each items.jsonl --questions q.json
    assert_failure 2
    assert_output --partial "not supported by: cloudflare/clef"
}

@test "a smooth image over 1024 px is resized and keeps its format" {
    magick -size 1500x1500 gradient:red-blue grad.png
    run --separate-stderr jev --dry-run $IMG --image grad.png --each items.jsonl --questions q.json
    assert_success
    [[ "$stderr" == *"grad.png: 1500x1500 png "*" -> 1024x1024 png "* ]]
}

@test "--image and _images share one byte budget" {
    magick -size 1500x1500 -seed 1 xc: +noise Random big.png
    magick -size 1500x1500 -seed 2 xc: +noise Random big2.png
    echo '{"a":1,"_images":["big2.png"]}' > e.jsonl
    run --separate-stderr jev --dry-run $IMG --image big.png --each e.jsonl --questions q.json
    assert_success
    total=$(jq '[.body.state[1:][].image_url.url | capture("<(?<n>[0-9]+) bytes>").n | tonumber] | add' <<<"$output")
    (( total <= 360 * 1024 ))
}

@test "an animated image is read by its first frame" {
    magick -delay 10 -size 8x8 xc:red xc:blue anim.webp
    run jev --dry-run $IMG --image anim.webp --each items.jsonl --questions q.json
    assert_success
}

@test "disagree boundaries: noul at exactly 0.5 and a score gap of exactly 0.25 count" {
    run python3 -c '
import jev
qs = {"n": {"type": "noul"}, "s": {"type": "score", "criteria": ["a", "b", "c", "d", "e"]}}
print(jev.disagree(qs, {"m1": {"n": {"noul": 0.49}, "s": {"score": 0}}, "m2": {"n": {"noul": 0.5}, "s": {"score": 1}}}))'
    assert_success
    assert_output "['n', 's']"
}

# --- call(): retries, stops, malformed responses ---

@test "call: 401 and a plain 402 stop the batch" {
    run fake '[{"status": 401, "body": {"error": {"code": 401, "message": "bad key"}}}]'
    assert_equal "$(jq -c '[.stop, .result.error.code, .calls]' <<<"$output")" '[true,401,1]'
    run fake '[{"status": 402, "body": {"error": {"code": 402, "message": "no credits"}}}]'
    assert_equal "$(jq -c '[.stop, .result.error.code]' <<<"$output")" '[true,402]'
}

@test "call: Requesty 403 without metadata stops the batch, with metadata fails only the item" {
    run fake '[{"status": 403, "body": {"error": {"origin": "router", "message": "not approved"}}}]' requesty
    assert_equal "$(jq -c '[.stop, .result.error.code]' <<<"$output")" '[true,403]'
    run fake '[{"status": 403, "body": {"error": {"code": 403, "message": "flagged", "metadata": {"reasons": ["x"]}}}}]'
    assert_equal "$(jq -c '[.stop, .result.error.code]' <<<"$output")" '[false,403]'
}

@test "call: a non-JSON 403 is a per-item waf_403" {
    run fake '[{"status": 403, "body": "<html>blocked</html>"}]'
    assert_equal "$(jq -c '[.stop, .result.error.code]' <<<"$output")" '[false,"waf_403"]'
}

@test "call: the HTTP status decides, not a null or string body code" {
    run fake '[{"status": 401, "body": {"error": {"code": null, "message": "bad key"}}}]' requesty
    assert_equal "$(jq -c '[.stop, .result.error.code]' <<<"$output")" '[true,401]'
    run fake '[{"status": 429, "body": {"error": {"code": "rate_limit_exceeded"}}, "headers": {"Retry-After": "0"}},
               {"status": 200, "body": {"answers": {"u": {"type": "noul", "noul": 0.9}}}}]'
    assert_equal "$(jq -c '[.calls, .result.answers.u.noul, .sleeps]' <<<"$output")" '[2,0.9,[0.0]]'
}

@test "call: Retry-After over the cap gives up without sleeping; a negative one falls back to backoff" {
    run fake '[{"status": 429, "body": {"error": {"code": 429}}, "headers": {"Retry-After": "120"}}]'
    assert_equal "$(jq -c '[.calls, .sleeps, .result.error.code]' <<<"$output")" '[1,[],429]'
    run fake '[{"status": 429, "body": {"error": {"code": 429}}, "headers": {"Retry-After": "-5"}},
               {"status": 200, "body": {"answers": {"u": {"type": "noul", "noul": 0.1}}}}]'
    assert_equal "$(jq -c '[.calls, (.sleeps[0] >= 1)]' <<<"$output")" '[2,true]'
}

@test "call: a 520 inside a 200 body and the in-flight 402 are retried" {
    run fake '[{"status": 200, "body": {"error": {"code": 520, "message": "upstream"}}},
               {"status": 200, "body": {"answers": {"u": {"type": "noul", "noul": 0.9}}}}]'
    assert_equal "$(jq -c '[.calls, .stop]' <<<"$output")" '[2,false]'
    run fake '[{"status": 402, "body": {"error": {"code": 402, "metadata": {"limit_source": "openrouter_in_flight_budget"}}}},
               {"status": 200, "body": {"answers": {"u": {"type": "noul", "noul": 0.9}}}}]'
    assert_equal "$(jq -c '[.calls, .stop]' <<<"$output")" '[2,false]'
}

@test "call: dropped connections are retried transport errors, not crashes" {
    run fake '[{"raise": "reset"}]'
    assert_success
    assert_equal "$(jq -c '[.calls, .result.error.code]' <<<"$output")" "[$(python3 -c 'import jev; print(jev.TRIES)'),0]"
    run fake '[{"raise": "disconnect"}, {"status": 200, "body": {"answers": {"u": {"type": "noul", "noul": 0.9}}}}]'
    assert_equal "$(jq -c '[.calls, .result.answers.u.noul]' <<<"$output")" '[2,0.9]'
}

@test "call: malformed bodies become error lines" {
    run fake '[{"status": 200, "body": ["oops"]}]'
    assert_equal "$(jq -r .result.error.code <<<"$output")" "bad_response"
    run fake '[{"status": 200, "body": {"error": "upstream failed"}}]'
    assert_equal "$(jq -r .result.error.code <<<"$output")" "error"
    run fake '[{"status": 200, "body": {"choices": [{"message": {"content": "null"}}]}}]' requesty
    assert_equal "$(jq -r .result.error.code <<<"$output")" "no_answers"
    run fake '[{"status": 200, "body": {"answers": {}}}]'
    assert_equal "$(jq -r .result.error.code <<<"$output")" "missing_answers"
    run fake '[{"status": 200, "body": {"answers": {"u": {"type": "noul", "value": 0.2}}}}]'
    assert_equal "$(jq -r .result.error.code <<<"$output")" "bad_answer"
}

@test "call: a Requesty answer and its usage are normalized; a null cost is dropped" {
    run fake '[{"status": 200, "body": {"model": "m", "usage": {"prompt_tokens": 7, "cost": null},
               "choices": [{"message": {"content": "{\"u\": {\"type\": \"noul\", \"noul\": 0.9}}"}}]}}]' requesty
    assert_equal "$(jq -c .result.usage <<<"$output")" '{"input_tokens":7}'
}

# --- main() end to end against the stub ---

ANS_JEV='{"answers": {"u": {"type": "noul", "noul": 0.9}}, "usage": {"input_tokens": 10, "cost": 0.000001}, "model": "typesafe/jev-1.13-x"}'
ANS_CLEF='{"model": "clef-x", "usage": {"prompt_tokens": 10, "cost": 0.000002}, "choices": [{"message": {"content": "{\"u\": {\"type\": \"noul\", \"noul\": 0.2}}"}}]}'

@test "e2e: compare mode keys answers by model, flags disagreement and sums cost" {
    stub "{\"/v1/systemone\": [{\"body\": $ANS_JEV}], \"/v1/chat/completions\": [{\"body\": $ANS_CLEF}]}"
    run --separate-stderr jev --model jev,clef --each items.jsonl --questions q.json
    assert_success
    assert_equal "$(head -1 <<<"$output" | jq -c '[(.answers | keys), .disagree]')" "[[\"sference/clef\",\"$JM\"],[\"u\"]]"
    assert_equal "$(head -1 <<<"$output" | jq -c "[.answers[\"$JM\"].u.noul, .answers[\"sference/clef\"].u.noul]")" '[0.9,0.2]'
    [[ "$stderr" == *"4 req, 0 failed, \$0.000006, "* ]]
    [[ "$stderr" == *"compare: 2/2 items disagree (u 2)"* ]]
}

@test "e2e: a batch-stopping 403 marks the rest skipped and exits 2" {
    printf '%s\n' '{"t":1}' '{"t":2}' '{"t":3}' > three.jsonl
    stub '{"/v1/chat/completions": [{"status": 403, "body": {"error": {"origin": "router", "message": "not approved"}}}]}'
    run --separate-stderr jev --model clef -j 1 --each three.jsonl --questions q.json
    assert_failure 2
    assert_equal "$(jq -r .error.code <<<"$output" | tr '\n' ' ')" "403 skipped skipped "
    [[ "$stderr" == *"stopped on an auth/billing error"* ]]
}

@test "e2e: a forced image request that succeeds on a non-image model warns instead of calling the check stale" {
    stub "{\"/v1/chat/completions\": [{\"body\": $ANS_CLEF}]}"
    run --separate-stderr jev --model clef --do-it-anyway --image red.png --each items.jsonl --questions q.json
    assert_success
    [[ "$stderr" == *"may have ignored the images"* ]]
    [[ "$stderr" != *"likely stale"* ]]
}

@test "e2e: a forced limit that the server accepts is reported as a stale check" {
    stub "{\"/v1/systemone\": [{\"body\": $ANS_JEV}]}"
    run --separate-stderr jev $IMG --do-it-anyway --image red.png --image red.png --image red.png --image red.png \
        --image red.png --each items.jsonl --questions q.json
    assert_success
    [[ "$stderr" == *"succeeded despite: --image: 5 images > 4"*"likely stale"* ]]
}

# --- round 2 ---

@test "an empty --model is a usage error, not a silent Jev run" {
    run jev --dry-run --model "" --each items.jsonl --questions q.json
    assert_failure 2
}

@test "sference's cap allows exactly 16 questions, in any case of the model id" {
    python3 -c 'import json; print(json.dumps({f"q{i}": {"type": "noul", "instructions": "x"} for i in range(16)}))' > q16.json
    python3 -c 'import json; print(json.dumps({f"q{i}": {"type": "noul", "instructions": "x"} for i in range(17)}))' > q17.json
    run jev --dry-run --model clef --each items.jsonl --questions q16.json
    assert_success
    run jev --dry-run --model Sference/Clef --each items.jsonl --questions q17.json
    assert_failure 2
}

@test "a falsy non-list _images is an item error, not silently dropped" {
    echo '{"a":1,"_images":0}' > e.jsonl
    run jev --dry-run $IMG --each e.jsonl --questions q.json
    assert_failure 1
    assert_output --partial "_images must be a list"
}

@test "a question that is not an object is an input error" {
    echo '{"u": "is it urgent?"}' > bad-q.json
    run jev --dry-run --each items.jsonl --questions bad-q.json
    assert_failure 2
}

@test "--image is shrunk to leave room for the longest state's text" {
    magick -size 345x345 -seed 3 xc: +noise Random -depth 8 n345.png
    python3 -c 'import json; print(json.dumps({"text": "x" * 30000}))' > long.jsonl
    run --separate-stderr jev --dry-run $IMG --image n345.png --each long.jsonl --questions q.json
    assert_success
    [[ "$stderr" == *"downscaled n345.png"* ]]
    n=$(jq '.body.state[1].image_url.url | capture("<(?<n>[0-9]+) bytes>").n | tonumber' <<<"$output")
    text=$(jq '.body.state[0].text | length' <<<"$output")
    (( n + text * 3 / 4 <= 360 * 1024 ))
}

@test "per-item images leave room for that item's text; unshrinkable overruns warn" {
    magick -size 345x345 -seed 3 xc: +noise Random -depth 8 n345.png
    python3 -c 'import json; print(json.dumps({"text": "x" * 30000, "_images": ["n345.png"]}))' > long.jsonl
    run --separate-stderr jev --dry-run $IMG --each long.jsonl --questions q.json
    assert_success
    n=$(jq '.body.state[1].image_url.url | capture("<(?<n>[0-9]+) bytes>").n | tonumber' <<<"$output")
    (( n + 30000 * 3 / 4 <= 360 * 1024 ))
    run --separate-stderr jev --dry-run $IMG --no-downscale --each long.jsonl --questions q.json
    assert_success
    [[ "$stderr" == *"item 0: images plus state"*"sending anyway"* ]]
}

@test "call: choice and score answers pass the shape check; wrong types are bad_answer" {
    QS='{"c": {"type": "choice"}, "s": {"type": "score"}, "n": {"type": "noul"}}'
    run fake '[{"status": 200, "body": {"answers": {"c": {"choice": "a"}, "s": {"score": 1.2}, "n": {"noul": 1}}}}]' openrouter "$QS"
    assert_equal "$(jq -c '[.result.answers.c.choice, .result.answers.s.score, .result.answers.n.noul]' <<<"$output")" '["a",1.2,1]'
    for bad in '{"c": {"choice": 3}, "s": {"score": 1}, "n": {"noul": 0.5}}' \
               '{"c": {"choice": "a"}, "s": {"score": "2"}, "n": {"noul": 0.5}}' \
               '{"c": {"choice": "a"}, "s": {"score": 1}, "n": {"noul": true}}' \
               '{"c": {"choice": "a"}, "s": {"score": 1}, "n": {"noul": "0.9"}}' \
               '{"c": "a", "s": {"score": 1}, "n": {"noul": 0.5}}'; do
        run fake "[{\"status\": 200, \"body\": {\"answers\": $bad}}]" openrouter "$QS"
        assert_equal "$(jq -r .result.error.code <<<"$output")" "bad_answer"
    done
}

@test "call: an unclassifiable error code fails the item; error null with answers is a success" {
    run fake '[{"status": 200, "body": {"error": {"code": [1], "message": "m"}}}]'
    assert_success
    assert_equal "$(jq -c '[.result.error.code, .stop]' <<<"$output")" '["error",false]'
    run fake '[{"status": 200, "body": {"error": null, "answers": {"u": {"type": "noul", "noul": 0.5}}}}]'
    assert_equal "$(jq -c '.result.answers.u.noul' <<<"$output")" '0.5'
}

@test "call: Requesty content that is already an object is accepted" {
    run fake '[{"status": 200, "body": {"choices": [{"message": {"content": {"u": {"type": "noul", "noul": 0.7}}}}]}}]' requesty
    assert_equal "$(jq -c .result.answers.u.noul <<<"$output")" '0.7'
}

@test "e2e: each backend gets its own key and request shape" {
    stub "{\"/v1/systemone\": [{\"body\": $ANS_JEV}], \"/v1/chat/completions\": [{\"body\": $ANS_CLEF}]}"
    run --separate-stderr jev --model jev,clef --each items.jsonl --questions q.json
    assert_success
    assert_equal "$(jq -r 'select(.path == "/v1/systemone") | .auth' stub.json.log | sort -u)" "Bearer or-key"
    assert_equal "$(jq -r 'select(.path == "/v1/chat/completions") | .auth' stub.json.log | sort -u)" "Bearer rq-key"
    assert_equal "$(jq -r 'select(.path == "/v1/chat/completions") | .body.response_format.type' stub.json.log | sort -u)" "questions"
}

@test "e2e: per-item failures exit 1 and are counted" {
    stub '{"/v1/systemone": [{"status": 400, "body": {"error": {"code": 400, "message": "x"}}}]}'
    run --separate-stderr jev --each items.jsonl --questions q.json
    assert_failure 1
    assert_equal "$(jq -r .error.code <<<"$output" | tr '\n' ' ')" "400 400 "
    [[ "$stderr" == *"2 req, 2 failed"* ]]
}

@test "e2e: compare with one model failing reports its error and skips disagree" {
    stub "{\"/v1/systemone\": [{\"body\": $ANS_JEV}], \"/v1/chat/completions\": [{\"status\": 400, \"body\": {\"error\": {\"message\": \"x\"}}}]}"
    run --separate-stderr jev --model jev,clef --each items.jsonl --questions q.json
    assert_failure 1
    first=$(head -1 <<<"$output")
    assert_equal "$(jq -c '[.error["sference/clef"].code, .disagree, (.answers | keys)]' <<<"$first")" "[400,null,[\"$JM\"]]"
    [[ "$stderr" == *"compare: 0/0 items disagree"* ]]
}

@test "e2e: a forced check is not called stale on the strength of another model's answers" {
    python3 -c 'import json; print(json.dumps({f"q{i}": {"type": "noul", "instructions": "x"} for i in range(17)}))' > q17.json
    ans=$(python3 -c 'import json; print(json.dumps({"answers": {f"q{i}": {"type": "noul", "noul": 0.5} for i in range(17)}}))')
    stub "{\"/v1/systemone\": [{\"body\": $ans}], \"/v1/chat/completions\": [{\"status\": 400, \"body\": {\"error\": {\"message\": \"x\"}}}]}"
    run --separate-stderr jev --model jev,clef --do-it-anyway --each items.jsonl --questions q17.json
    assert_failure 1
    [[ "$stderr" != *"likely stale"* ]]
    [[ "$stderr" != *"succeeded despite"* ]]
}

@test "e2e: a cost missing from the response is estimated and marked" {
    stub '{"/v1/systemone": [{"body": {"answers": {"u": {"type": "noul", "noul": 0.9}}, "usage": {"input_tokens": 1000000}}}]}'
    run --separate-stderr jev --each items.jsonl --questions q.json
    assert_success
    [[ "$stderr" == *'$0.084000 (est.)'* ]]
}

@test "--context reaches state.context as JSON when it parses, else as text" {
    echo '{"rubric": 1}' > ctx.json; echo 'plain words' > ctx.txt
    run jev --dry-run --each items.jsonl --questions q.json --context ctx.json
    assert_equal "$(head -1 <<<"$output" | jq -c .body.state.context)" '{"rubric":1}'
    run jev --dry-run --each - --questions q.json --context ctx.txt <items.jsonl
    assert_equal "$(head -1 <<<"$output" | jq -r .body.state.context)" "plain words"
}

@test "a closed output pipe ends quietly" {
    seq 3000 | sed 's/.*/{"n":&}/' > many.jsonl
    run bash -c "python3 '$SCRIPT' --dry-run --each many.jsonl --questions q.json | head -1 >/dev/null; echo \"\${PIPESTATUS[0]}\""
    assert_output "1"
}

# --- round 3 ---

@test "the suite cannot reach a real API: a live run without the stub has no key" {
    run jev --each items.jsonl --questions q.json
    assert_failure 2
    assert_output --partial "no key"
}

@test "call: backoff doubles and the last attempt does not sleep" {
    run fake '[{"status": 503, "body": {"error": {"code": 503}}}]'
    assert_equal "$(jq -c '[.calls, [.sleeps[] | floor], .result.error.code]' <<<"$output")" '[5,[1,2,4,8],503]'
}

@test "call: every retryable status is retried, a 400 is not, a non-JSON 502 keeps its status" {
    for st in 408 429 500 502 503 504 520 524 529; do
        run fake "[{\"status\": $st, \"body\": {\"error\": {\"code\": $st}}}, {\"status\": 200, \"body\": {\"answers\": {\"u\": {\"type\": \"noul\", \"noul\": 0.1}}}}]"
        assert_equal "$(jq -c .calls <<<"$output")" "2"
    done
    run fake '[{"status": 400, "body": {"error": {"code": 400}}}]'
    assert_equal "$(jq -c '[.calls, .sleeps]' <<<"$output")" '[1,[]]'
    run fake '[{"status": 502, "body": "<html>bad gateway</html>"}, {"status": 200, "body": {"answers": {"u": {"type": "noul", "noul": 0.1}}}}]'
    assert_equal "$(jq -c .calls <<<"$output")" "2"
}

@test "call: Retry-After of exactly the cap is honoured; NaN and an HTTP-date keep the backoff" {
    run fake '[{"status": 429, "body": {"error": {"code": 429}}, "headers": {"Retry-After": "60"}},
               {"status": 200, "body": {"answers": {"u": {"type": "noul", "noul": 0.1}}}}]'
    assert_equal "$(jq -c '[.calls, .sleeps]' <<<"$output")" '[2,[60.0]]'
    for ra in "Wed, 21 Oct 2015 07:28:00 GMT" "nan"; do
        run fake "[{\"status\": 429, \"body\": {\"error\": {\"code\": 429}}, \"headers\": {\"Retry-After\": \"$ra\"}},
                   {\"status\": 200, \"body\": {\"answers\": {\"u\": {\"type\": \"noul\", \"noul\": 0.1}}}}]"
        assert_success
        assert_equal "$(jq -c '[.calls, (.sleeps[0] >= 1)]' <<<"$output")" '[2,true]'
    done
}

@test "call: NaN in an answer or the cost is not a number" {
    run fake '[{"status": 200, "body": "{\"answers\": {\"u\": {\"type\": \"noul\", \"noul\": NaN}}}"}]'
    assert_equal "$(jq -r .result.error.code <<<"$output")" "bad_answer"
}

@test "transparency is flattened onto white when an image goes JPEG" {
    magick -size 8x8 xc:none clear.png
    run python3 -c '
import base64, jev
u, n, note = jev.load_image("clear.png", 1, True)
assert u.startswith("data:image/jpeg;"), u[:30]
open("out.jpg", "wb").write(base64.b64decode(u.split(",", 1)[1]))'
    assert_success
    assert_equal "$(magick out.jpg -format '%[fx:int(255*mean+0.5)]' info:)" "255"
}

@test "image metadata is stripped even when nothing else changes" {
    magick -size 8x8 xc:red -set comment secret-gps tagged.png
    assert_equal "$(magick tagged.png -format %c info:)" "secret-gps"
    run --separate-stderr jev --dry-run $IMG --image tagged.png --each items.jsonl --questions q.json
    assert_success
    assert_equal "$stderr" ""
    run python3 -c '
import base64, jev
u, n, note = jev.load_image("tagged.png", 10**6, True)
assert note is None, note
open("out.png", "wb").write(base64.b64decode(u.split(",", 1)[1]))'
    assert_success
    assert_equal "$(magick out.png -format %c info:)" ""
}

@test "exactly four images pass the count limit" {
    run jev --dry-run $IMG --image red.png --image red.png --image red.png --image red.png --each items.jsonl --questions q.json
    assert_success
}

@test "elide reports the image's exact size" {
    run python3 -c '
import base64, jev
u, n, note = jev.load_image("red.png", 10**6, True)
print(jev.elide({"u": u})["u"].split("<")[1].split(" ")[0], n)'
    assert_success
    read -r shown real <<<"$output"
    assert_equal "$shown" "$real"
}

@test "disagree skips score questions with no or a single criterion" {
    run python3 -c '
import jev
qs = {"a": {"type": "score", "criteria": ["only"]}, "b": {"type": "score"}}
print(jev.disagree(qs, {"m1": {"a": {"score": 0}, "b": {"score": 0}}, "m2": {"a": {"score": 0}, "b": {"score": 3}}}))'
    assert_success
    assert_output "[]"
}

@test "input errors exit 2: empty --each, broken questions, bad stdin" {
    : > empty.jsonl
    run jev --dry-run --each empty.jsonl --questions q.json
    assert_failure 2
    echo '{broken' > broken.json
    run jev --dry-run --each items.jsonl --questions broken.json
    assert_failure 2
    run jev --dry-run --each items.jsonl
    assert_failure 2
    run jev --dry-run <<<'{broken'
    assert_failure 2
    run jev --dry-run <<<'{"questions":{"u":{"type":"noul","instructions":"i"}}}'
    assert_failure 2
    echo '{"u": {"type": ["noul"], "instructions": "x"}}' > listtype.json
    run jev --dry-run --each items.jsonl --questions listtype.json
    assert_failure 2
}

@test "_images null is an item error, not an image-less request" {
    echo '{"a":1,"_images":null}' > n.jsonl
    run jev --dry-run $IMG --each n.jsonl --questions q.json
    assert_failure 1
    assert_output --partial "_images must be a list"
}

@test "a null --context is still sent as the context" {
    echo null > ctx.json
    run jev --dry-run --each items.jsonl --questions q.json --context ctx.json
    assert_equal "$(head -1 <<<"$output" | jq -c '.body.state | has("context")')" "true"
}

@test "Jev via Requesty needs --no-zdr, and --do-it-anyway does not lift that" {
    run jev --dry-run --model jev --via requesty --each items.jsonl --questions q.json
    assert_failure 2
    assert_output --partial "loses zero data retention"
    run jev --dry-run --model jev --via requesty --do-it-anyway --each items.jsonl --questions q.json
    assert_failure 2
    run jev --dry-run --model jev --via requesty --no-zdr --each items.jsonl --questions q.json
    assert_success
}

@test "non-ASCII state is charged at its escaped length" {
    run python3 -c '
import jev
print(jev.sent_len({"t": "ä" * 100}) > 600)'
    assert_output "True"
}

@test "e2e: an env key's surrounding whitespace is ignored; a key with control characters is a usage error" {
    stub "{\"/v1/systemone\": [{\"body\": $ANS_JEV}]}"
    OPENROUTER_API_KEY=$'or-key\n' run --separate-stderr jev --each items.jsonl --questions q.json
    assert_success
    assert_equal "$(jq -r .auth stub.json.log | sort -u)" "Bearer or-key"
    OPENROUTER_API_KEY=$'or\x01key' run jev --each items.jsonl --questions q.json
    assert_failure 2
}

@test "e2e: without an env key the first line of pass is used; a failing pass is a usage error" {
    stub "{\"/v1/systemone\": [{\"body\": $ANS_JEV}]}"
    unset OPENROUTER_API_KEY
    mkdir bin
    printf '#!%s\n[ "$1" = api/openrouter/jev-skill ] && printf "pass-key\\nlogin: x\\n"\n' "$(command -v bash)" > bin/pass
    chmod +x bin/pass
    PATH="$PWD/bin:$PATH" run --separate-stderr jev --each items.jsonl --questions q.json
    assert_success
    assert_equal "$(jq -r .auth stub.json.log | sort -u)" "Bearer pass-key"
    printf '#!%s\necho partial\nexit 1\n' "$(command -v bash)" > bin/pass
    PATH="$PWD/bin:$PATH" run jev --each items.jsonl --questions q.json
    assert_failure 2
    assert_output --partial "no key"
}

@test "e2e: a single stdin request prints {model, answers, usage}" {
    stub "{\"/v1/systemone\": [{\"body\": $ANS_JEV}]}"
    run --separate-stderr jev <<<'{"state":"s","questions":{"u":{"type":"noul","instructions":"i"}}}'
    assert_success
    assert_equal "$(jq -c '[.model, .answers.u.noul, .usage.cost]' <<<"$output")" '["typesafe/jev-1.13-x",0.9,0.000001]'
    [[ "$stderr" == *"1 req, 0 failed"* ]]
}

@test "e2e: a bad _images entry fails only its item; i counts non-blank lines" {
    stub "{\"/v1/systemone\": [{\"body\": $ANS_JEV}]}"
    printf '%s\n' '{"t":"a","_images":["red.png"]}' '' '{"t":"b","_images":["x.txt"]}' > img.jsonl
    run --separate-stderr jev $IMG --each img.jsonl --questions q.json
    assert_failure 1
    assert_equal "$(jq -c '[.i, .error.code // .answers.u.noul]' <<<"$output" | tr '\n' ' ')" '[0,0.9] [1,"image"] '
}

@test "e2e: answers without cost or tokens are flagged unknown" {
    stub '{"/v1/systemone": [{"body": {"answers": {"u": {"type": "noul", "noul": 0.9}}}}]}'
    run --separate-stderr jev --each items.jsonl --questions q.json
    assert_success
    [[ "$stderr" == *'$0.000000 + unknown'* ]]
}

@test "e2e: a stop on one backend does not throw away the other's answers" {
    stub "{\"/v1/systemone\": [{\"body\": $ANS_JEV}], \"/v1/chat/completions\": [{\"status\": 401, \"body\": {\"error\": {\"message\": \"bad key\"}}}]}"
    run --separate-stderr jev --model jev,clef -j 1 --each items.jsonl --questions q.json
    assert_failure 2
    assert_equal "$(jq -c "[.answers[\"$JM\"].u.noul, .error[\"sference/clef\"].code]" <<<"$output" | tr '\n' ' ')" '[0.9,401] [0.9,"skipped"] '
}

# --- round 4 ---

@test "e2e: the live request carries ZDR, the questions and the images" {
    stub "{\"/v1/systemone\": [{\"body\": $ANS_JEV}]}"
    run --separate-stderr jev --each items.jsonl --questions q.json
    assert_success
    assert_equal "$(jq -c .body.provider stub.json.log | sort -u)" '{"zdr":true,"data_collection":"deny"}'
    assert_equal "$(jq -c .body.questions stub.json.log | sort -u)" "$(jq -c . q.json)"
    assert_equal "$(jq -c .body.state stub.json.log | sort | tr '\n' ' ')" '{"item":{"text":"a"}} {"item":{"text":"b"}} '
    rm stub.json.log
    run --separate-stderr jev $IMG --image red.png --each items.jsonl --questions q.json
    assert_success
    assert_equal "$(jq -c .body.provider stub.json.log | sort -u)" '{"data_collection":"deny"}'
    assert_equal "$(jq -r '.body.state[1].image_url.url[:22]' stub.json.log | sort -u)" "data:image/png;base64,"
}

@test "e2e: a live compare run keeps ZDR for Jev and drops it only for Clef" {
    stub "{\"/v1/systemone\": [{\"body\": $ANS_JEV}]}"
    run --separate-stderr jev --model jev,cloudflare/clef --via openrouter --no-zdr --each items.jsonl --questions q.json
    assert_equal "$(jq -c "select(.body.model == \"$JM\") | .body.provider" stub.json.log | sort -u)" '{"zdr":true,"data_collection":"deny"}'
    assert_equal "$(jq -c 'select(.body.model == "cloudflare/clef") | .body.provider' stub.json.log | sort -u)" '{"data_collection":"deny"}'
}

@test "e2e: the live path validates answer shapes" {
    stub '{"/v1/systemone": [{"body": {"answers": {"u": {"type": "noul", "value": 0.2}}}}]}'
    run --separate-stderr jev --each items.jsonl --questions q.json
    assert_failure 1
    assert_equal "$(jq -r .error.code <<<"$output" | tr '\n' ' ')" "bad_answer bad_answer "
}

@test "e2e: models that agree are not counted as disagreeing" {
    agree='{"model": "clef-x", "usage": {"prompt_tokens": 10, "cost": 0.000002}, "choices": [{"message": {"content": "{\"u\": {\"type\": \"noul\", \"noul\": 0.8}}"}}]}'
    stub "{\"/v1/systemone\": [{\"body\": $ANS_JEV}], \"/v1/chat/completions\": [{\"body\": $agree}]}"
    run --separate-stderr jev --model jev,clef --each items.jsonl --questions q.json
    assert_success
    assert_equal "$(jq -c .disagree <<<"$output" | sort -u)" "[]"
    [[ "$stderr" == *"compare: 0/2 items disagree (u 0)"* ]]
}

@test "shared --image travels with per-item images" {
    echo '{"a":1,"_images":["red.png"]}' > e.jsonl
    run jev --dry-run $IMG --image red.png --each e.jsonl --questions q.json
    assert_success
    assert_equal "$(jq '.body.state | length' <<<"$output")" "3"
}

@test "two large per-item images share the byte budget" {
    magick -size 1500x1500 -seed 1 xc: +noise Random big.png
    magick -size 1500x1500 -seed 2 xc: +noise Random big2.png
    echo '{"a":1,"_images":["big.png","big2.png"]}' > e.jsonl
    run --separate-stderr jev --dry-run $IMG --each e.jsonl --questions q.json
    assert_success
    total=$(jq '[.body.state[1:][].image_url.url | capture("<(?<n>[0-9]+) bytes>").n | tonumber] | add' <<<"$output")
    (( total <= 360 * 1024 ))
}

@test "metadata is stripped on the JPEG re-encode path too" {
    magick -size 1500x1500 -seed 1 xc: +noise Random -set comment secret-gps -quality 95 big.jpg
    run python3 -c '
import base64, jev
u, n, note = jev.load_image("big.jpg", 360 << 10, True)
assert note and "jpeg" in note, note
open("out.jpg", "wb").write(base64.b64decode(u.split(",", 1)[1]))'
    assert_success
    assert_equal "$(magick out.jpg -format %c info:)" ""
}

@test "an EXIF-rotated photo is sent upright on both encode paths" {
    magick -size 40x20 xc:red plain.jpg
    # EXIF APP1 with Orientation=6 (rotate 90 CW), spliced after SOI; magick alone cannot write the tag.
    python3 -c '
import struct
ifd = b"MM\x00\x2a\x00\x00\x00\x08" + struct.pack(">H", 1) + struct.pack(">HHIHH", 0x0112, 3, 1, 6, 0) + b"\x00\x00\x00\x00"
app1 = b"Exif\x00\x00" + ifd
d = open("plain.jpg", "rb").read()
open("rot.jpg", "wb").write(d[:2] + b"\xff\xe1" + struct.pack(">H", len(app1) + 2) + app1 + d[2:])'
    assert_equal "$(magick rot.jpg -format '%[orientation] %wx%h' info:)" "RightTop 40x20"
    run python3 -c '
import base64, jev
for budget, name in ((10**6, "keep.jpg"), (1, "jpeg.jpg")):  # first pass, and the shrink-to-JPEG loop
    u, n, note = jev.load_image("rot.jpg", budget, True)
    open(name, "wb").write(base64.b64decode(u.split(",", 1)[1]))'
    assert_success
    assert_equal "$(magick keep.jpg -format '%wx%h' info:)" "20x40"
    assert_equal "$(magick jpeg.jpg -format '%[fx:h>w]' info:)" "1"
}

@test "call: a 402 with metadata other than the in-flight budget still stops the batch" {
    run fake '[{"status": 402, "body": {"error": {"code": 402, "message": "key limit", "metadata": {"limit_source": "key"}}}}]'
    assert_equal "$(jq -c '[.stop, .result.error.code, .calls]' <<<"$output")" '[true,402,1]'
}

# --- round 6: varied data, so a mix-up between items, models or images shows ---

ans_jev() { echo "{\"answers\": {\"u\": {\"type\": \"noul\", \"noul\": $1}}, \"usage\": {\"cost\": 0.000001}, \"model\": \"j\"}"; }
ans_clef() { echo "{\"model\": \"c\", \"usage\": {\"prompt_tokens\": 10, \"cost\": 0.000002}, \"choices\": [{\"message\": {\"content\": \"{\\\"u\\\": {\\\"type\\\": \\\"noul\\\", \\\"noul\\\": $1}}\"}}]}"; }

@test "e2e: each item gets its own answers, single model and compare" {
    stub "{\"/v1/systemone\": [{\"body\": $(ans_jev 0.9)}, {\"body\": $(ans_jev 0.1)}],
           \"/v1/chat/completions\": [{\"body\": $(ans_clef 0.8)}, {\"body\": $(ans_clef 0.2)}]}"
    run --separate-stderr jev -j 1 --each items.jsonl --questions q.json
    assert_success
    assert_equal "$(jq -c '[.i, .answers.u.noul]' <<<"$output" | tr '\n' ' ')" '[0,0.9] [1,0.1] '
    kill "$STUB_PID"; rm -f port stub.json.log
    stub "{\"/v1/systemone\": [{\"body\": $(ans_jev 0.9)}, {\"body\": $(ans_jev 0.1)}],
           \"/v1/chat/completions\": [{\"body\": $(ans_clef 0.8)}, {\"body\": $(ans_clef 0.2)}]}"
    run --separate-stderr jev -j 1 --model jev,clef --each items.jsonl --questions q.json
    assert_success
    assert_equal "$(jq -c "[.i, .answers[\"$JM\"].u.noul, .answers[\"sference/clef\"].u.noul]" <<<"$output" | tr '\n' ' ')" \
        '[0,0.9,0.8] [1,0.1,0.2] '
}

@test "several images each travel, shared ones first" {
    magick -size 64x64 gradient:blue-yellow blue.png
    # Sizes after metadata stripping, which re-encodes every image; red and blue differ.
    read -r r b <<<"$(python3 -c 'import jev; print(*(jev.load_image(p, 10**6, True)[1] for p in ("red.png", "blue.png")))')"
    [[ "$r" != "$b" ]]
    sizes() { jq -r '[.body.state[1:][].image_url.url | capture("<(?<n>[0-9]+) bytes>").n] | join(" ")' <<<"$1" | head -1; }
    run jev --dry-run $IMG --image red.png --image blue.png --each items.jsonl --questions q.json
    assert_success
    assert_equal "$(sizes "$output")" "$r $b"
    echo '{"a":1,"_images":["blue.png","red.png"]}' > e.jsonl
    run jev --dry-run $IMG --each e.jsonl --questions q.json
    assert_equal "$(sizes "$output")" "$b $r"
    echo '{"a":1,"_images":["blue.png"]}' > e.jsonl
    run jev --dry-run $IMG --image red.png --each e.jsonl --questions q.json
    assert_equal "$(sizes "$output")" "$r $b"
}

@test "e2e: live per-item images reach the wire, and live lines report downscaling" {
    magick -size 1500x1500 gradient:red-blue grad.png
    stub "{\"/v1/systemone\": [{\"body\": $ANS_JEV}]}"
    printf '%s\n' '{"t":"a","_images":["red.png"]}' '{"t":"b","_images":["grad.png"]}' > img.jsonl
    run --separate-stderr jev -j 1 $IMG --each img.jsonl --questions q.json
    assert_success
    assert_equal "$(jq -r '.body.state[1].type' stub.json.log | sort -u)" "image_url"
    assert_equal "$(jq -c '.downscaled | length' <<<"$output" | tr '\n' ' ')" "0 1 "
    [[ "$(sed -n 2p <<<"$output" | jq -r '.downscaled[0]')" == "grad.png: 1500x1500 png "* ]]
}

@test "e2e: a single stdin request reports --image downscaling in its output" {
    magick -size 1500x1500 gradient:red-blue grad.png
    stub "{\"/v1/systemone\": [{\"body\": $ANS_JEV}]}"
    run --separate-stderr jev $IMG --image grad.png <<<'{"state":"s","questions":{"u":{"type":"noul","instructions":"i"}}}'
    assert_success
    [[ "$(jq -r '.downscaled[0]' <<<"$output")" == "grad.png: 1500x1500 png "* ]]
}

@test "non-ASCII state reaches the model unescaped" {
    echo '{"text":"Größe"}' > de.jsonl
    run jev --dry-run --model clef --each de.jsonl --questions q.json
    assert_equal "$(jq -r '.body.messages[0].content' <<<"$output")" '{"item": {"text": "Größe"}}'
    run jev --dry-run $IMG --image red.png --each de.jsonl --questions q.json
    assert_equal "$(jq -r '.body.state[0].text' <<<"$output")" '{"item": {"text": "Größe"}}'
}
