# Transcript sources when `transcript` finds none

`transcript` already tries the feed's transcripts and Pocket Casts' generated one. On the
last 40 history episodes (2026-10-31, 14 shows) 15 had a feed transcript, 22 a generated
one, 33 either; the 7 without were fiction (Wolf 359) and ZEIT. This page is for the rest.

Fetch pages with a browser User-Agent; check for "Just a moment" (Cloudflare) before
trusting the body. Counting "transcript" on a page proves nothing: menus and episode
descriptions match too. Open the page and look.

| Host | Where | Speakers | Notes |
|---|---|---|---|
| Buzzsprout | feed tag: html, json, srt, vtt | `SPEAKER_nn` | json has word timing; diarization sometimes merges two speakers into one turn |
| beehiiv | feed tag: vtt | none | Cloudflare blocks the default UA |
| Podigee | feed tag: json, vtt | none | |
| programmier.bar page | episode page | names | name-labelled transcript; confirms the Buzzsprout mapping |
| Substack (dwarkesh.com) | episode page, after "Transcript" | names as standalone lines | timestamps only per publisher section; the footer ("Recent Episodes", author name) parses as a speaker turn: cut it |
| NPR | `npr.org/transcripts/<id>`, linked from the episode page | `Name:` per paragraph | |
| zeit.de | — | — | 403 |
| Pocket Casts | show notes JSON, `pocket_casts_transcripts` | none (0 of 27 checked) | generated only where the feed has none, mostly |
| Transistor, Libsyn, Acast, Spotify for Creators | nothing found | — | |
