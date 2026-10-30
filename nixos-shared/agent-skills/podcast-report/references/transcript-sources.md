# Transcript sources when the feed has none

Measured on 22 shows from the user's history (2026-10-29): 5 had a feed transcript,
2 a transcript on the publisher page, 13 nothing. Expect to transcribe most episodes.

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
| Transistor, Libsyn, Acast, Spotify for Creators | nothing found | — | |
