Update claude-code in nixpkgs. Arguments: $ARGUMENTS

## Files
- `pkgs/by-name/cl/claude-code/manifest.zst.json` — version source of
  truth; `package.nix` fetches the zstd-compressed binary it lists and
  `unzstd`s it at install time, and is never touched by the updater. No
  npm build (`claude-code-bin` folded in, #511120). The plain
  `manifest.json` was **removed** in #556673 (merged 2026-09-01); any
  reference to it is stale.
- `pkgs/applications/editors/vscode/extensions/anthropic.claude-code/default.nix`
  — one vsix hash per arch; the only `.nix` file the update changes.
- Upstream: `curl -s
  https://registry.npmjs.org/@anthropic-ai/claude-code/latest | jq -r
  .version`

## Policy (not negotiable)
- **Never dispatch the CI review unless the user asks.**
  `on-success=mark_as_ready` takes the PR out of draft, so dispatching
  is the human-review gate — not a step you complete. A fresh ask is
  needed for any commit set the user has not seen (see the vsix-hash
  gotcha).
- **Both commits carry `Assisted-by: <tool> (<model>)`** — the tool
  actually executing this (`Claude Code` when run from it) plus the
  model as a prose name, e.g. `Claude Opus 5`, not the raw model
  id. `Co-authored-by:` does not satisfy the policy, and a wrong tool
  name is grounds for closing the PR. If you cannot determine your own
  tool name or model version, ask; if unattended, stop before
  committing rather than guess.
- **The PR body must separately disclose** the same tool and model — CONTRIBUTING.md: *"Pull request summaries and review comments must be disclosed separately to commits."* The commit trailer does not cover the PR summary. Literal opening line for the body:
  ```
  Prepared with the assistance of <tool> (<model>). Version and hash bumps were produced by the standard maintainers/scripts/update.nix script.
  ```
- **The PR stays a draft** until the all-arch review is green; the
  ticks are assertions about the PR at the moment it goes ready. Draft
  status exempts only *full self-review* (`CONTRIBUTING.md`,
  Exemptions) — disclosure is never waived.
- **Unattended run (cron, headless, no user reachable)?** Opening the
  draft PR is fine — it is marked not-ready. Stop there: never
  dispatch, never mark ready, never close anyone's PR. A human reviews
  before the flip.

The steps below are guidance — adapt them. The **bolded** warnings in
them are not.

## Flow
1. Sync to clean `master`: `git fetch upstream && git checkout master
   && git reset --hard upstream/master`. **The fetch is load-bearing**
   — `upstream/master` is only as fresh as the last one, and a stale
   reset puts a false OLD in the PR title and both commit
   subjects. **`upstream`, not `origin`** — the fork's master is ~230k
   commits stale. Read OLD here from `manifest.zst.json`; a feature
   branch's manifest is ahead of master and is not OLD. Stop and
   report if OLD == NEW.
2. Branch `claude-code-OLD-to-NEW`. If your own earlier update PR is
   still open and npm has moved past it, branch fresh from `master` to
   the newest version rather than extending it — identical net diff,
   no force-push, supersedes the old PR. **Different case:** if
   `master` restructured the packaging under an open draft of yours
   (#556673 deleted `manifest.json` mid-flight), rebase instead of
   opening a second PR — reset to fresh `master`, re-run the updater,
   then force-push onto the *existing* PR branch with
   `--force-with-lease=<branch>:<remote sha>` and fix the title and
   body with `gh pr edit`. Re-derive OLD from the newly merged
   `master`: the OLD the PR was opened against is dead, and the branch
   name keeping the old span is cosmetic.
3. Run the updater from the checkout root:
   ```bash
   echo "" | NIX_PATH=nixpkgs=$PWD nix-shell maintainers/scripts/update.nix --arg predicate \
     '(path: pkg: builtins.elem path [["claude-code"] ["vscode-extensions" "anthropic" "claude-code"]])'
   ```
   `NIX_PATH=`
   is required (no root channel on this machine; the inner `nix-shell`
   resolves `<nixpkgs>`). **Set the Bash timeout to 300000ms** — the
   60s default kills it mid-run and the wreckage looks like a
   different bug; recover with `git checkout -- pkgs/`, never by
   hand-editing. The updater owns every version and hash — **never
   hand-edit them** (one exception: the vsix-hash gotcha).
4. Re-read NEW from `manifest.zst.json` — npm, `downloads.claude.ai` and
   the VS Marketplace publish on independent timelines, so trust the
   file over the npm check for titles and commit subjects. If the
   extension still shows OLD the marketplace hasn't published yet:
   drop the extension commit and ship the `claude-code` bump
   alone. Then `nix fmt
   pkgs/applications/editors/vscode/extensions/anthropic.claude-code/default.nix`
   → `NIXPKGS_ALLOW_UNFREE=1 nix-build -A claude-code` (its
   `versionCheckHook` proves the binary reports NEW) and
   `NIXPKGS_ALLOW_UNFREE=1 nix-build -A
   vscode-extensions.anthropic.claude-code` (the prefix is needed on
   both; env does not survive between calls). Local builds cover only
   the host arch: count the hash lines `default.nix` has — derive it,
   never hardcode, the arch list has changed before — and confirm via
   `git diff` that the updater rewrote all of them. If it comes up
   short, **don't force-fetch from the marketplace** — ship it, note
   the stale arch in the PR body, and let CI name it.
5. Two commits: `claude-code` first,
   `vscode-extensions.anthropic.claude-code` second (the vsix-hash
   gotcha amends the latter as HEAD). Subject `pkg: OLD -> NEW`,
   changelog URL in body, `Assisted-by:` trailer on both.
6. Push, write the body to a temp file, then:
   ```bash
   PR_URL=$(gh pr create --draft --repo NixOS/nixpkgs --title "claude-code: OLD -> NEW" --body-file "$BODY")
   ```
   Body = the disclosure line, a
   one-line summary, the changelog URL, then
   `.github/PULL_REQUEST_TEMPLATE.md` **including its reference-link
   definitions** (drop them and `[CONTRIBUTING.md]` renders as literal
   brackets). Tick exactly seven: the three platform boxes, `Ran
   nixpkgs-review`, `Tested basic functionality`, `Fits
   CONTRIBUTING.md`, `Follows the automation/AI policy`. The
   release-note boxes apply only to a major or breaking bump. Verify
   the ticks landed with `gh pr view "$NUM" --json body` — upstream
   retitles the labels and a silent no-op ships an under-ticked
   PR. `NUM` is the trailing element of `$PR_URL`. If `$DISPLAY` is
   set: `nohup bash -c "DISPLAY=:0 xdg-open '$PR_URL'" >/dev/null 2>&1
   &`.  If the PR is ever going ready *without* a green all-arch
   review, untick the platform boxes and `Ran nixpkgs-review` first.
7. `gh pr list --repo NixOS/nixpkgs --search "claude-code in:title"
   --state open`. Others': report only. Your own superseded one: close
   only with user approval *and* a green review here. A competing PR
   can restructure the packaging and not just the version, so check
   *what* landed: `gh pr view N --json state,mergedAt,mergeCommit`
   (there is no `merged` field). If one merged under you, go back to
   step 1.
8. Report the changelog from
   `raw.githubusercontent.com/anthropics/claude-code/main/CHANGELOG.md`,
   never from memory. Extract the NEW section specifically; it lags
   npm by hours — if absent, say so instead of reporting the previous
   version. Lead with security/permission-boundary fixes, then
   breaking changes, then new env vars (name them exactly). Skip UI
   churn.

7 and 8 don't wait on the review.

## Review dispatch (when asked)
```bash
gh workflow run review --repo markus1189/nixpkgs-review-gha -f pr=NUM \
  -f x86_64-linux=true -f aarch64-linux=true \
  -f x86_64-darwin=no -f aarch64-darwin=yes_sandbox_relaxed \
  -f push-to-cache=true -f upterm=false \
  -f post-result=true -f on-success=mark_as_ready
```
It prints the run URL; the trailing number is `RUN_ID`. Babysit in background, polling until status is `completed` — not `!= in_progress` (Actions sits in `queued` between `prepare` and the arch matrix, so that exits after `prepare` alone). Expect `prepare`, 3× `review (...)`, `report`, all `success`. Confirm the flip with `gh pr view NUM --json isDraft`. If the arch jobs are green but `report` failed, the builds are fine and the flip didn't happen: diagnose with `gh run view $RUN_ID --log-failed`, then **stop and report** — never run `gh pr ready` yourself, that is the gate.

## Gotchas
- "Not updating version, already X" → not on clean master.
- The updater's package listing is not a state check; it has printed a
  version the tree did not contain. Trust `jq`/`grep` on the files.
- **vsix hash mismatch** (surfaces only in CI) — the one sanctioned
  hand-edit. Target **the failing arch**, not a hardcoded
  `x86_64-linux`: `gh run view $RUN_ID --repo
  markus1189/nixpkgs-review-gha --log-failed | grep -E
  "got:|specified:"`. Fix that entry, amend the extension commit,
  force-with-lease. Re-dispatching needs a **fresh ask**: the amended
  hash is content the user has not seen, it cannot be verified
  locally, and the retry would flip the PR to ready on it.
