Update claude-code in nixpkgs. Arguments: $ARGUMENTS

## Quick Reference

**Files:**
- `pkgs/by-name/cl/claude-code/package.nix` - Binary distribution package (formerly claude-code-bin)
- `pkgs/by-name/cl/claude-code/manifest.json` - Binary manifest (version source)
- `pkgs/applications/editors/vscode/extensions/anthropic.claude-code/default.nix` - VSCode extension

Note: `claude-code-bin` was folded into `claude-code` (nixpkgs PR #511120, merged 2026-04-23). There is no longer a separate `-bin` package, no `package-lock.json`, and no npm build — `claude-code` now pulls a prebuilt binary from the manifest.

**Branch naming:** `claude-code-OLD-to-NEW` (e.g., `claude-code-2.1.118-to-2.1.119`)

## Workflow

### 1. Prepare (ALWAYS start here)
```bash
git checkout master
git fetch upstream && git pull upstream master
git status --short  # Must be clean
```

If not clean or on wrong branch: `git reset --hard upstream/master`

### 2. Check Versions
```bash
# Current version in nixpkgs (manifest is the source of truth)
jq -r '.version' pkgs/by-name/cl/claude-code/manifest.json

# Latest on npm
curl -s https://registry.npmjs.org/@anthropic-ai/claude-code/latest | jq -r '.version'
```

**If already up-to-date:** Stop here, nothing to do.

**If a prior version's update PR is still open (yours, unmerged) and npm has moved on:** default to a **fresh branch from `master`** targeting the newest npm version. The net diff is identical to extending the old branch, but a clean branch keeps history simple and avoids a force-push. This **supersedes** the in-flight PR — close it per step 13 (needs your approval, and only after the new PR's review is green).

### 3. Create Branch
```bash
git checkout -b claude-code-OLD-to-NEW
```

### 4. Run Update Script
Pipe empty input to handle interactive prompt. The `claude-code-bin` attr no longer exists but leaving it in the predicate is harmless (unmatched entries are ignored):
```bash
echo "" | nix-shell maintainers/scripts/update.nix --arg predicate \
  '(path: pkg: builtins.elem path [["claude-code"] ["vscode-extensions" "anthropic" "claude-code"]])'
```
**Timeout:** 300000ms (5 minutes)

**If script fails:** Do NOT fix manually. Ask user or retry with higher timeout.

### 5. Verify Versions Match
Both must show the same version:
```bash
jq -r '.version' pkgs/by-name/cl/claude-code/manifest.json
grep 'version = ' pkgs/applications/editors/vscode/extensions/anthropic.claude-code/default.nix
```

### 6. Format
```bash
nix fmt pkgs/by-name/cl/claude-code/package.nix \
       pkgs/applications/editors/vscode/extensions/anthropic.claude-code/default.nix
```

### 7. Build and Validate

```bash
env NIXPKGS_ALLOW_UNFREE=1 nix-build -A claude-code
env NIXPKGS_ALLOW_UNFREE=1 nix-build -A vscode-extensions.anthropic.claude-code
```

The `claude-code` build runs `versionCheckHook` against `claude --version` — a successful build confirms the binary reports the expected version.

The update script now refreshes **all four** vsix hashes in `default.nix` (one per arch). The local `nix-build` above only validates the **host** arch's hash; the other three are validated by the all-arch `nixpkgs-review-gha` run (step 12). So instead of force-fetching from the marketplace, just confirm the updater actually rewrote all four hash lines:

```bash
F=pkgs/applications/editors/vscode/extensions/anthropic.claude-code/default.nix
changed=$(git diff -- "$F" | grep -cE '^\+ *hash = "sha256-')
echo "$changed of 4 vsix hashes updated"
[ "$changed" -eq 4 ] || echo "WARNING: expected 4 updated hashes — updater may have regressed to host-only; CI (step 12) will flag any stale arch, fix per Troubleshooting"
```

If the count is 4, all hashes are fresh and CI will confirm them. If it is not 4 (updater regression), let CI surface the stale arch and patch it per Troubleshooting.

### 8. Commit (TWO separate commits)

Per the nixpkgs [Automation/AI policy](https://github.com/NixOS/nixpkgs/blob/master/CONTRIBUTING.md#automationai-policy), LLM-assisted commits **must** carry an `Assisted-by:` trailer naming the tool plus the primary model name and version (a `Co-authored-by:` does **not** satisfy this). Replace the model/version in the trailer with the actual model used this session (e.g. `Claude Opus 4.8`).

```bash
# First: claude-code binary package
git add pkgs/by-name/cl/claude-code/
git commit -m "$(cat <<'EOF'
claude-code: OLD -> NEW

https://github.com/anthropics/claude-code/blob/main/CHANGELOG.md

Assisted-by: Claude Code (Claude Opus 4.8)
EOF
)"

# Second: vscode extension
git add pkgs/applications/editors/vscode/extensions/anthropic.claude-code/
git commit -m "$(cat <<'EOF'
vscode-extensions.anthropic.claude-code: OLD -> NEW

https://github.com/anthropics/claude-code/blob/main/CHANGELOG.md

Assisted-by: Claude Code (Claude Opus 4.8)
EOF
)"
```

### 9. Push
```bash
git push origin claude-code-OLD-to-NEW
```

### 10. Create Draft PR
```bash
# Prepare PR body from template with summary prepended and boxes ticked
BODY_FILE=$(mktemp)
cat > "$BODY_FILE" <<SUMMARY
Update claude-code and vscode-extensions.anthropic.claude-code to NEW.

https://github.com/anthropics/claude-code/blob/main/CHANGELOG.md

> [!NOTE]
> Prepared with the assistance of Claude Code (Claude Opus 4.8). Version and hash bumps were produced by the standard \`maintainers/scripts/update.nix\` script; the change was built locally and validated across all four arches via \`nixpkgs-review\`, and reviewed by a human (@markus1189) before being marked ready.

SUMMARY
cat .github/PULL_REQUEST_TEMPLATE.md >> "$BODY_FILE"

# Tick the relevant checkboxes
sed -i 's/- \[ \] x86_64-linux/- [x] x86_64-linux/' "$BODY_FILE"
sed -i 's/- \[ \] aarch64-linux/- [x] aarch64-linux/' "$BODY_FILE"
sed -i 's/- \[ \] x86_64-darwin/- [x] x86_64-darwin/' "$BODY_FILE"
sed -i 's/- \[ \] aarch64-darwin/- [x] aarch64-darwin/' "$BODY_FILE"
sed -i 's/- \[ \] Ran `nixpkgs-review`/- [x] Ran `nixpkgs-review`/' "$BODY_FILE"
sed -i 's/- \[ \] Tested basic functionality/- [x] Tested basic functionality/' "$BODY_FILE"
sed -i 's/- \[ \] Fits \[CONTRIBUTING/- [x] Fits [CONTRIBUTING/' "$BODY_FILE"
sed -i 's/- \[ \] Follows the \[automation\/AI policy\]/- [x] Follows the [automation\/AI policy]/' "$BODY_FILE"

gh pr create --draft --title "claude-code: OLD -> NEW" \
  --body-file "$BODY_FILE" --repo NixOS/nixpkgs
rm "$BODY_FILE"
```

### 11. Open PR in Browser
```bash
nohup bash -c 'DISPLAY=:0 xdg-open $PR_URL' >/dev/null 2>&1 &
```

### 12. Trigger nixpkgs-review-gha
**Do not dispatch automatically — wait for explicit user request.** This is the human-in-the-loop checkpoint required by the AI policy: the draft exemption only holds while the PR is a draft, and `on-success=mark_as_ready` flips it to ready, so the responsible person must have reviewed the change before dispatching. Surface the command so the user can invoke it. Posts result as PR comment; `on-success=mark_as_ready` flips draft → ready.
```bash
gh workflow run review --repo markus1189/nixpkgs-review-gha \
  -f pr=NUM \
  -f x86_64-linux=true -f aarch64-linux=true \
  -f x86_64-darwin=yes_sandbox_relaxed -f aarch64-darwin=yes_sandbox_relaxed \
  -f push-to-cache=true -f upterm=false \
  -f post-result=true -f on-success=mark_as_ready
```
Recent runs: `gh run list --workflow=review --repo markus1189/nixpkgs-review-gha --limit 5`

After dispatch, babysit the run. Capture `RUN_ID` from the dispatch output URL (`gh workflow run` prints the run URL; the trailing number is the ID — or use `gh run list --workflow=review --repo markus1189/nixpkgs-review-gha --limit 1`):

```bash
# Babysit: poll until status is COMPLETED.
# WARNING: do NOT poll for "!= in_progress" — GitHub Actions passes through a
# `queued` state between the `prepare` phase and the arch matrix, so that
# condition exits early reporting success when only `prepare` has finished.
RUN_ID=<run id from dispatch URL>
until [ "$(gh run view "$RUN_ID" --repo markus1189/nixpkgs-review-gha --json status --jq '.status')" = "completed" ]; do
  sleep 60
done
gh run view "$RUN_ID" --repo markus1189/nixpkgs-review-gha \
  --json conclusion,jobs --jq '{conclusion, jobs: [.jobs[] | {name, conclusion}]}'
```

Run this as a background task (`run_in_background`) so completion notifies the agent. Expect 6 jobs (`prepare`, 4 arch `review (...)`, `report`) all `success`.

### 13. Check for Obsolete PRs
Search for open claude-code update PRs that are now superseded:
```bash
gh pr list --repo NixOS/nixpkgs --search "claude-code in:title" --state open \
  --json number,title,author,url --jq '.[] | "\(.number) | \(.author.login) | \(.title) | \(.url)"'
```
Handle the results by case:

- **Other contributors' PRs** for older versions: list them to the user; do not act on them.
- **Your own prior in-flight PR** superseded by this run (the step-2 fork case): closing it requires **two conditions** — (a) explicit user approval, and (b) the new PR's review is **green across all arches** (step 12 passed). Never close on faith at creation time; if the new PR fails CI, the old PR is the fallback. Suggested close command (only after both conditions met):
  ```bash
  gh pr close <OLD_NUM> --repo NixOS/nixpkgs \
    --comment "Superseded by #<NEW_NUM>, which bumps straight to <NEW_VERSION>."
  ```

## Troubleshooting

**"Not updating version, already X.X.X"** - Not on clean master. Reset: `git checkout master && git reset --hard upstream/master`

**Versions don't match** - Dirty branch. Reset to master, delete bad branch, start over.

**vscode-ext hash mismatch (only surfaces on CI)** - Normally the updater refreshes all four hashes, but if one is stale (updater regression, or marketplace re-rolled the vsix bytes) it only fails in CI's clean sandbox. Fetch CI's `got:` hash from the failed review run, paste into the matching arch entry in `default.nix`, `git commit --amend --no-edit` into the vscode-ext commit, `git push --force-with-lease`, re-trigger review.
```bash
RUN=<run id>
JOB=$(gh api repos/markus1189/nixpkgs-review-gha/actions/runs/$RUN/jobs \
      --jq '.jobs[] | select(.name=="review (x86_64-linux)") | .id')
gh api repos/markus1189/nixpkgs-review-gha/actions/jobs/$JOB/logs | grep -E "got:|specified:"
```
