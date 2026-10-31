#!/usr/bin/env bats
# pocketcasts.py's pure parts imported as a module: episode choice, uuid guard, list shaping.
# Nothing touches Pocket Casts.

bats_require_minimum_version 1.5.0

setup() {
    bats_load_library bats-support
    bats_load_library bats-assert
    export PYTHONPATH="$BATS_TEST_DIRNAME/../pocketcasts/scripts" PYTHONDONTWRITEBYTECODE=1
    export http_proxy=http://127.0.0.1:9 https_proxy=http://127.0.0.1:9
}

# py CODE: run CODE with `p` = the pocketcasts module.
py() { python3 -c "import json, pocketcasts as p
$1"; }

# hist(pcts): history entries newest first, each 1000 s long and played to pct %.
HIST="def hist(*pcts): return [{'uuid': f'u{n}', 'title': f'Ep {n}', 'podcastTitle': 'Show', 'duration': 1000, 'playedUpTo': c * 10} for n, c in enumerate(pcts)]"

@test "choose: latest started episode by default" {
    run py "$HIST
ep, why, alt = p.choose(hist(0, 50, 100)); print(ep['uuid'], why)"
    assert_output "u1 latest started"
}

@test "choose: a barely started newest episode above a finished one is taken as autoplay" {
    run py "$HIST
ep, why, alt = p.choose(hist(1, 95, 40)); print(ep['uuid'], len(alt))"
    assert_output "u1 2"
}

@test "choose: barely started with nothing finished below stays the pick" {
    run py "$HIST
ep, why, alt = p.choose(hist(5, 40)); print(ep['uuid'], why)"
    assert_output "u0 latest started"
}

@test "choose: several title matches exit 3 with the candidates" {
    run py "$HIST
p.choose(hist(10, 20), title='Ep')"
    assert_failure 3
    assert_output --partial "several episodes match"
}

@test "choose: a title without letters or digits is refused" {
    run py "$HIST
p.choose(hist(10), title='!!')"
    assert_failure 1
    assert_output --partial "no letters or digits"
}

@test "choose: --show narrows the default pick" {
    run py "$HIST
h = hist(50, 50); h[1]['podcastTitle'] = 'Other Pod'
ep, why, alt = p.choose(h, show='other pod'); print(ep['uuid'])"
    assert_output "u1"
}

@test "an empty or malformed uuid is refused before any request" {
    run py "import argparse
for s in ('', '1738c9e1', '1738C9E1-B073-4EC2-B7B8-BB7641704F92'):
    try: p.uuid_arg(s); print('accepted', repr(s))
    except argparse.ArgumentTypeError as e: print(e)
print(p.uuid_arg('1738c9e1-b073-4ec2-b7b8-bb7641704f92'))"
    assert_output "not a uuid: ''
not a uuid: '1738c9e1'
not a uuid: '1738C9E1-B073-4EC2-B7B8-BB7641704F92'
1738c9e1-b073-4ec2-b7b8-bb7641704f92"
}

@test "the CLI rejects an empty uuid with exit 2, before logging in" {
    run python3 "$BATS_TEST_DIRNAME/../pocketcasts/scripts/pocketcasts.py" queue ''
    assert_failure 2
    assert_output --partial "not a uuid: ''"
}

@test "up next: progress from episodeSync joins its episode; one without stays unknown" {
    run py "eps = p.up_next_episodes({'episodes': [{'uuid': 'a', 'podcast': 'P', 'title': 'A'}, {'uuid': 'b', 'podcast': 'P', 'title': 'B'}],
                       'episodeSync': [{'uuid': 'a', 'playedUpTo': 500, 'duration': 1000}]})
print([(e['podcastUuid'], p.line(e)[:4]) for e in eps])"
    assert_output "[('P', ' 50%'), ('P', '   -')]"
}

@test "positions: seconds or [HH:]MM:SS" {
    run py "print(p.seconds('75'), p.seconds('10:00'), p.seconds('1:02:03'))"
    assert_output "75 600 3723"
}
