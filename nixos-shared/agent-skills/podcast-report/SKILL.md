---
name: podcast-report
description: "Summarizes a podcast episode the user names (show and/or title), or else the latest one from their Pocket Casts history, into a report (markdown in ~/Stuff plus a Claude Doc). Use only when the user asks to summarize a podcast episode ('fass die Folge X von Y zusammen', 'summarize the episode about Z', 'fass die Podcast-Folge zusammen')."
---

# Podcast report

The user triggers this on the go, mostly via Remote Control while running: no follow-up
questions unless the script exits 3.

## Pipeline

1. **Resolve.** `./scripts/podcast.py episode -o WORK`, `WORK` = a new dir in the scratchpad.
   Any hint in the request wins: show/title words → `--show S` and/or `--title T` (substrings);
   a topic or guest → pick from `./scripts/podcast.py history`, then `--uuid`. The user often
   hears several episodes in a row and wants only one of them. Only with no
   hint at all fall back to the default, the latest started episode, skipping one that
   autoplay just began. Report `picked_because` and
   `alternatives` in one line, so a wrong pick is visible at once.
2. **Skip fiction.** `fiction: true` → stop and say so. Readings of stories count too
   (LeVar Burton Reads is filed under Performing Arts, not Fiction).
3. **Transcript**, first that works:
   - `./scripts/podcast.py transcript WORK`
   - the publisher page: [references/transcript-sources.md](references/transcript-sources.md)
   - `./scripts/podcast.py audio WORK`, then the transcribe-audio skill. Its speaker
     labels are best effort and sometimes confidently wrong: treat them as unlabelled.
   The transcript stays in `WORK`; it never goes into ~/Stuff.
4. **Speakers.** Map labels to people from `notes.md` and `persons` first, then self-intros,
   being addressed by name, who presents which topic per the show notes, elimination.
   Spellings come from the show notes; ASR mangles names.
5. **Report** per [references/report-template.md](references/report-template.md).
6. **Deliver** both:
   - `$(stuff-today --default)/podcast-<show>-<episode-slug>.md`
   - a Claude Doc with the same content via the `anthropic-skills:docs` skill; its link goes
     in the md header.
   The user comments in the Doc; apply each change to the Doc and the md together.

## When the script stops

| Exit | Meaning | Do |
|---|---|---|
| 1 | error, message says why | `pass … failed`: the gpg key is locked; tell the user to unlock it at the desktop (or over phone-tunnel) and stop. Network/feed errors: retry once, then report. |
| 2 | no feed transcript | next step in 3 |
| 3 | needs a choice | several episodes match → rerun with `--uuid` if the request decides it, else ask with the printed list. No iTunes match → rerun with `--feed` from the candidates, or find the feed URL on the show's site. |

**Script Execution:** Always invoke scripts by absolute path: resolve `./scripts/` against
this SKILL.md's directory. All scripts use Nix shebangs, so no dependency installation is
needed.
