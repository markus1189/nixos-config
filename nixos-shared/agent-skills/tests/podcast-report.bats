#!/usr/bin/env bats
# podcast.py's pure parts imported as a module against synthetic fixtures: transcript parsing,
# feed item matching and transcript order. Nothing touches Pocket Casts, iTunes or a feed.
# Episode choice lives in pocketcasts.py: tests/pocketcasts.bats.

bats_require_minimum_version 1.5.0

setup() {
    bats_load_library bats-support
    bats_load_library bats-assert
    export PYTHONPATH="$BATS_TEST_DIRNAME/../podcast-report/scripts" PYTHONDONTWRITEBYTECODE=1
    FIX="$BATS_TEST_DIRNAME/fixtures/podcast-report"
    export http_proxy=http://127.0.0.1:9 https_proxy=http://127.0.0.1:9
}

# py CODE: run CODE with `p` = the podcast module, FIX = the fixture dir.
py() { python3 -c "import json, podcast as p
FIX='$FIX'
$1"; }

# feed-matching scaffold: ITEMS = the fixture feed's items, m(url, title) -> matched title, how
FEED="import xml.etree.ElementTree as ET; ITEMS = ET.parse(FIX + '/feed.xml').getroot().findall('./channel/item')
def m(url, title):
    i, how = p.match_item(ITEMS, {'url': url, 'title': title})
    return (i.findtext('title') if i is not None else None, how)"

@test "VTT cues without an hour field are kept" {
    run py "print([r[0] for r in p.parse_cues(open(FIX + '/hourless.vtt').read())])"
    assert_success
    assert_output "[1, 3599, 3605, 3610, 3610]"
}

@test "VTT: inline tags are stripped, several voices in one cue become separate rows" {
    run py "print(p.parse_cues(open(FIX + '/hourless.vtt').read())[2:])"
    assert_output "[(3605, 'Anna', 'after one hour tagged'), (3610, 'Ben', 'Hi Anna'), (3610, 'Anna', 'Hi Ben')]"
}

@test "SRT: SPEAKER_nn prefix sets the speaker, unprefixed cues inherit it" {
    run py "print(p.parse_cues(open(FIX + '/prefixed.srt').read()))"
    assert_output "[(10, 'SPEAKER_00', 'Hallo und herzlich willkommen'), (12, 'SPEAKER_00', 'zur Folge.'), (46, 'SPEAKER_03', 'Moin Moin.')]"
}

@test "SRT turns merge per speaker" {
    run py "print(p.to_turns('application/x-subrip', open(FIX + '/prefixed.srt').read()))"
    assert_output "([[10, 'SPEAKER_00', 'Hallo und herzlich willkommen zur Folge.'], [46, 'SPEAKER_03', 'Moin Moin.']], True)"
}

@test "HTML transcript: cite/time/p become timed, labelled turns" {
    run py "print(p.to_turns('text/html', open(FIX + '/transcript.html').read()))"
    assert_output "([[10, 'SPEAKER_00', 'Hallo und herzlich willkommen.'], [3646, 'SPEAKER_03', 'Moin Moin.']], True)"
}

@test "plain text transcript: one turn, reported as without timestamps" {
    run py "print(p.to_turns('text/plain', 'Anna: hello  Ben: hi'))"
    assert_output "([[0, '?', 'Anna: hello Ben: hi']], False)"
}

@test "hms pads hours" {
    run py "print(p.hms(9), p.hms(3646))"
    assert_output "00:00:09 01:00:46"
}

@test "match: exact enclosure URL" {
    run py "$FEED
print(m('https://cdn.example.com/show/ep249.mp3', 'whatever'))"
    assert_output "('Episode 249: Renamed Episode', 'exact')"
}

@test "match: tracking redirects stripped on either side" {
    run py "$FEED
print(m('https://dts.podtrac.com/redirect.mp3/cdn.example.com/show/tracked.mp3', 'x'))"
    assert_output "('Tracked', 'stripped')"
}

@test "match: a title hit on an item without enclosure is skipped" {
    run py "$FEED
print(m('https://elsewhere.example.com/gone.mp3', 'Renamed Episode'))"
    assert_output "(None, None)"
}

@test "match: IDs in the query are told apart, a shared path tail alone matches nothing" {
    run py "$FEED
print(m('https://media.example.com/audio.mp3?episodeId=bbb', 'x'), m('https://other.example.com/audio.mp3?id=zzz', 'x'))"
    assert_output "('Query B', 'exact') (None, None)"
}

@test "an empty title never matches by title" {
    run py "$FEED
print(m('https://elsewhere.example.com/x.mp3', '!!'))"
    assert_output "(None, None)"
}

@test "podcast namespace: the old GitHub URL is read too" {
    run py "$FEED
print([t.get('type') for t in p.pi_all(ITEMS[1], 'transcript')], [x.text for x in p.pi_all(ITEMS[1], 'person')])"
    assert_output "['text/vtt'] ['Jane Doe']"
}

@test "the publisher's transcripts come before Pocket Casts' generated one, each by type preference" {
    run py "print([(t['type'], t.get('origin')) for t in p.ordered([
        {'type': 'text/vtt', 'origin': 'pocketcasts'}, {'type': 'text/vtt'}, {'type': 'image/png'}, {'type': 'application/json'}])])"
    assert_output "[('application/json', None), ('text/vtt', None), ('image/png', None), ('text/vtt', 'pocketcasts')]"
}

@test "podcast.py finds pocketcasts.py next to itself" {
    run py "print(p.choose.__module__)"
    assert_output "pocketcasts"
}
