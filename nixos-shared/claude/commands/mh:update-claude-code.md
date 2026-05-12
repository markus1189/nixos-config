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

The vscode-ext build only validates the **host** arch's vsix hash. The update script bumps `version` but only refreshes the host's `hash`, so the other three stay stale and would only fail in CI's clean sandbox. Force-fetch all four directly from the marketplace and compare against `default.nix`:

```bash
F=pkgs/applications/editors/vscode/extensions/anthropic.claude-code/default.nix
VERSION=$(awk -F'"' '/version = "/{print $2; exit}' "$F")
URL_BASE="https://anthropic.gallery.vsassets.io/_apis/public/gallery/publisher/anthropic/extension/claude-code/$VERSION/assetbyname/Microsoft.VisualStudio.Services.VSIXPackage"

check() {
  local sys=$1 arch=$2 expected raw actual
  expected=$(grep -A2 "\"$sys\"" "$F" | grep -oE 'sha256-[A-Za-z0-9+/=]+' | head -1)
  for _ in 1 2 3; do
    raw=$(nix-prefetch-url --type sha256 "$URL_BASE?targetPlatform=$arch" 2>/dev/null)
    [ -n "$raw" ] && break
    sleep 2
  done
  if [ -z "$raw" ]; then
    printf '%-16s FETCH-FAILED\n' "$sys"; return
  fi
  actual=$(nix hash convert --hash-algo sha256 --to sri "$raw" 2>/dev/null)
  if [ "$expected" = "$actual" ]; then
    printf '%-16s OK   %s\n' "$sys" "$expected"
  else
    printf '%-16s MISMATCH\n  specified: %s\n  got:       %s\n' "$sys" "$expected" "$actual"
  fi
}

tmp=$(mktemp -d -t claude-code.XXXXXX)
trap 'rm -rf "$tmp"' EXIT
echo "version=$VERSION"
check x86_64-linux   linux-x64    > "$tmp/1" &
check aarch64-linux  linux-arm64  > "$tmp/2" &
check x86_64-darwin  darwin-x64   > "$tmp/3" &
check aarch64-darwin darwin-arm64 > "$tmp/4" &
wait
cat "$tmp"/{1,2,3,4}
```

Any `MISMATCH` line shows `specified:` and `got:` — paste each `got:` value into the matching arch entry in `default.nix`, re-run until clean. Marketplace occasionally drops one of the four parallel connections; the inline retry covers it.

### 8. Commit (TWO separate commits)
```bash
# First: claude-code binary package
git add pkgs/by-name/cl/claude-code/
git commit -m "$(cat <<'EOF'
claude-code: OLD -> NEW

https://github.com/anthropics/claude-code/blob/main/CHANGELOG.md
EOF
)"

# Second: vscode extension
git add pkgs/applications/editors/vscode/extensions/anthropic.claude-code/
git commit -m "$(cat <<'EOF'
vscode-extensions.anthropic.claude-code: OLD -> NEW

https://github.com/anthropics/claude-code/blob/main/CHANGELOG.md
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

gh pr create --draft --title "claude-code: OLD -> NEW" \
  --body-file "$BODY_FILE" --repo NixOS/nixpkgs
rm "$BODY_FILE"
```

### 11. Open PR in Browser
```bash
nohup bash -c 'DISPLAY=:0 xdg-open $PR_URL' >/dev/null 2>&1 &
```

### 12. Trigger nixpkgs-review-gha
**Do not dispatch automatically — wait for explicit user request.** Surface the command so the user can invoke it. Posts result as PR comment; `on-success=mark_as_ready` flips draft → ready.
```bash
gh workflow run review --repo markus1189/nixpkgs-review-gha \
  -f pr=NUM \
  -f x86_64-linux=true -f aarch64-linux=true \
  -f x86_64-darwin=yes_sandbox_relaxed -f aarch64-darwin=yes_sandbox_relaxed \
  -f push-to-cache=true -f upterm=false \
  -f post-result=true -f on-success=mark_as_ready
```
Recent runs: `gh run list --workflow=review --repo markus1189/nixpkgs-review-gha --limit 5`

After dispatch, loop to check the report for any failed builds.

### 13. Check for Obsolete PRs
Search for open claude-code update PRs that are now superseded:
```bash
gh pr list --repo NixOS/nixpkgs --search "claude-code in:title" --state open \
  --json number,title,author,url --jq '.[] | "\(.number) | \(.author.login) | \(.title) | \(.url)"'
```
List any version-update PRs targeting older versions to the user so they can decide to close them.

## Troubleshooting

**"Not updating version, already X.X.X"** - Not on clean master. Reset: `git checkout master && git reset --hard upstream/master`

**Versions don't match** - Dirty branch. Reset to master, delete bad branch, start over.

**vscode-ext hash mismatch (only surfaces on CI)** - Update script bumps `version` but NOT `hash` in `anthropic.claude-code/default.nix`. Local builds can pass from stale FOD cache while CI fails; marketplace also occasionally re-rolls vsix bytes. Fetch CI's `got:` hash from the failed review run, paste into `default.nix`, `git commit --amend --no-edit` into the vscode-ext commit, `git push --force-with-lease`, re-trigger review.
```bash
RUN=<run id>
JOB=$(gh api repos/markus1189/nixpkgs-review-gha/actions/runs/$RUN/jobs \
      --jq '.jobs[] | select(.name=="review (x86_64-linux)") | .id')
gh api repos/markus1189/nixpkgs-review-gha/actions/jobs/$JOB/logs | grep -E "got:|specified:"
```
