# elfeed-score

Rules: `nixos-shared/packages/emacs/elfeed.score`. Emacs reads and writes
that file in the checkout directly (`elfeed-score-serde-score-file` in
`emacs-config.el`); it is not in the nix store, so a rule change is live
after `= l` in the search buffer, with no rebuild.

## Editing

Both ways work: `= a…` from the elfeed search buffer, or by hand in the file.

- **Never symlink it** into `~/.emacs.d`. elfeed-score saves via temp-file +
  rename (`elfeed-score-rule-stats--sexp-to-file`), which replaces a symlink
  with a plain file and silently detaches it from the repo.
- **Comments don't survive.** Every `= a` rewrites the whole file from the
  in-memory rules through `pp`. Put the reason for a rule in the commit
  message, not in the file. The committed layout is the writer's own output,
  so an `= a` diff shows only the new rule.
- **After a `git pull` or hand edit, reload with `= l`.** elfeed-score refuses
  `= a` when the file's mtime is newer than its last load (`…-dirty-p`), so
  your in-memory rules can't overwrite changes it hasn't seen.
- The rule-hit stats (`~/.emacs.d/elfeed.stats`) stay machine-local. They're
  rewritten after every update and would only add churn to the history.
- The repo is public. Rules are about as revealing as the feed list in
  `emacs-config.el`, which is already public. Keep it that way.

## Measuring

The label comes from what you do in elfeed: `mh/pocketed` (set by
`mh/elfeed-raindrop-add-url`) means *saved to read*; read without it means
*skipped*. Labels exist from 2026-05-26, the first pocketed entry.

```bash
nixos-shared/packages/emacs/elfeed-score-eval.py            # needs running Emacs with elfeed
nixos-shared/packages/emacs/elfeed-score-eval.py --export f.tsv   # reuse an export
```

It calls `mh/elfeed-score-export`, which **rescores every labeled entry
against the rules loaded right now** without writing to the db. To try a rule
change: edit the file, `= l`, rerun, compare. Nothing is committed until you
commit it, and nothing in the db changes until you rescore (`= v`).

Output:

| section | read it as |
|---|---|
| AUC, recall@10% / @25% | share of pocketed entries in the top 10/25% of the ranking. `rescored` = current rules, `stored` = what the search buffer showed. |
| headroom row | current rules + a feed weight learned on everything before the last 30 days, tested on those 30 days. The gap to `current rules, last 30d` is what a feed-level rule set could still gain. |
| rules | hits / pocketed / lift (pocket rate ÷ base rate). Lift ≫ 1 for positive rules and ≈ 0 for negative ones means the rule works; 0 hits means it's dead. |
| underscored | feeds you pocket often that score ≤ 0: candidates for a `feed` rule. |
| noise | feeds with 100+ entries, never pocketed, scoring ≥ 0: demote or unsubscribe. |

Baseline 2026-09-29 (30,715 labeled, 648 pocketed): current rules AUC 0.71,
recall@10% 0.43; with learned feed weights 0.90 / 0.64.

## Caveats

- The label is *saved*, not *interesting*: something read inline or opened
  in the browser without pocketing counts as skipped. HN in particular
  (8.5k entries, 2 pockets) may be read somewhere else.
- Feedback loop: what ranks high gets seen and pocketed more. Treat a feed
  that went to zero after a demotion with suspicion.
- Stored scores go stale; only new entries are scored. `= v` rescores the
  current search.
- `mh/elfeed-score-export` uses elfeed-score's internal `--explain-*`
  functions. If an elfeed-score update breaks it, look there first.
