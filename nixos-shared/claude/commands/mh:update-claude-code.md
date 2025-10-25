---
description: Update claude-code package in nixpkgs with automated process
argument-hint: [new-version] (optional - will check npm if not provided)
allowed-tools: Bash, Read, Write, Edit, Grep, TodoWrite
---

I'll help you update the claude-code package in nixpkgs following the proper process. This involves checking versions, creating a branch, running the update script, and preparing for a PR.

Arguments: $ARGUMENTS

**FIRST STEP - ALWAYS SWITCH TO MASTER:**
Before doing ANYTHING else, you MUST:
1. Switch to master branch: `git checkout master`
2. Fetch and pull latest: `git fetch upstream && git pull upstream master`

DO NOT assume you are already on master or that you are in the middle of an update. ALWAYS start fresh from master.

Let me start by creating a todo list and following the systematic update process:

1. **Prepare Repository** - Switch to master and update with upstream
2. **Check Current and Latest Versions** - Compare nixpkgs version with npm registry
3. **Create Branch** - Only after confirming a new version exists
4. **Run Update Command** - Use update.sh maintainer script
5. **Verify Version Sync** - CRITICAL: Check both packages show same version
6. **Fix npmDepsHash** - If needed, update hash from build error
7. **Format Files** - Run nix fmt on changed .nix files
8. **Build and Test** - Verify BOTH packages build successfully
9. **Commit Changes** - Follow nixpkgs commit format with changelog
10. **Push Branch** - Ready for PR creation

**Critical Requirements:**
- MUST switch to clean master branch FIRST - never assume you're already on the right branch
- **BOTH packages MUST be at the SAME version** - claude-code CLI and VSCode extension must match
- update.sh does NOT update npmDepsHash - you must fix it manually if build fails
- Unfree license requires `NIXPKGS_ALLOW_UNFREE=1` or `env NIXPKGS_ALLOW_UNFREE=1` prefix for commands
- Follow nixpkgs commit format: `claude-code: old-version -> new-version`
- **NEVER include npm diff links** - Only use changelog link: `https://github.com/anthropics/claude-code/blob/main/CHANGELOG.md`
- **Format changed files** - Run `nix fmt` on modified .nix files before committing

**Update Command (RECOMMENDED):**
Use the maintainer update script which handles everything automatically:
```bash
./pkgs/by-name/cl/claude-code/update.sh
```
This script automatically:
- Fetches the latest version from npm
- Updates claude-code package with nix-update
- Updates the VSCode extension
- Generates lockfiles for both packages

**Key Files Updated by Script:**
- `pkgs/by-name/cl/claude-code/package.nix` - Version and src hash
- `pkgs/by-name/cl/claude-code/package-lock.json` - npm dependencies
- `pkgs/applications/editors/vscode/extensions/anthropic.claude-code/default.nix` - VSCode extension

**KNOWN LIMITATION - npmDepsHash:**
⚠️ The update script does NOT automatically update the `npmDepsHash` field in package.nix.
You MUST manually update it after the script runs if the build fails with a hash mismatch.
See "Fixing npmDepsHash" section below.

**Verify package-lock.json Version (REQUIRED):**
Check that the version in package-lock.json matches the new version:
```bash
grep '"version":' pkgs/by-name/cl/claude-code/package-lock.json | head -1
```
The version must match the NEW-VERSION being updated to (e.g., "2.0.22").

**Format Changed Files (REQUIRED):**
Run `nix fmt` on modified .nix files before committing:
```bash
nix fmt pkgs/by-name/cl/claude-code/package.nix
nix fmt pkgs/applications/editors/vscode/extensions/anthropic.claude-code/default.nix
```

**Verify Both Packages In Sync (CRITICAL):**
Before building, verify both packages are at the SAME version:
```bash
grep -E 'version.*2\.0\.' pkgs/by-name/cl/claude-code/package.nix pkgs/applications/editors/vscode/extensions/anthropic.claude-code/default.nix
grep '"version":' pkgs/by-name/cl/claude-code/package-lock.json | head -1
```
All three MUST show the same version (e.g., "2.0.25"). If they don't match, DO NOT PROCEED.

**Build Verification (CRITICAL):**
**ALL changed packages MUST build successfully** before committing:
```bash
env NIXPKGS_ALLOW_UNFREE=1 nix-build -A claude-code
env NIXPKGS_ALLOW_UNFREE=1 nix-build -A vscode-extensions.anthropic.claude-code
```
Both builds must succeed. If either fails, see "Troubleshooting" section below.

**Fixing npmDepsHash (if build fails):**
If `nix-build -A claude-code` fails with a hash mismatch error like:
```
error: hash mismatch in fixed-output derivation
  specified: sha256-XylBq0/zu7iSTPiLAkewQFeh1OmtJv9nUfnCb66opVE=
  got:       sha256-NjmCmOwepKaSZ+vji0sUlZ1nSkcG02kNokGe37YYtX0=
```

Update the npmDepsHash in package.nix:
1. Copy the "got:" hash from the error
2. Edit `pkgs/by-name/cl/claude-code/package.nix`
3. Replace the old npmDepsHash value with the new one from "got:"
4. Re-run the build to verify it succeeds

**Commit Changes:**
After formatting and verifying builds succeed, commit the changes:
```bash
git add -A
git commit -m "$(cat <<'EOF'
claude-code: OLD-VERSION -> NEW-VERSION

https://github.com/anthropics/claude-code/blob/main/CHANGELOG.md
EOF
)"
```
Replace OLD-VERSION and NEW-VERSION with actual version numbers.

**Push Branch:**
```bash
git push origin claude-code-OLD-VERSION-to-NEW-VERSION
```
Use actual version numbers in branch name.

---

## Troubleshooting

### Issue: update.sh says "Not updating version, already X.X.X"

**Cause:** You're not on a clean master branch. The script checks the current package.nix version.

**Solution:**
1. `git checkout master`
2. `git reset --hard upstream/master`
3. Delete any existing update branches
4. Create a fresh branch: `git checkout -b claude-code-update-X.X.X`
5. Run update.sh again

### Issue: Versions don't match between packages

**Symptoms:** claude-code package.nix shows different version than VSCode extension

**Cause:** Running update.sh on a dirty/partial branch

**Solution:**
1. Reset to clean master (see above)
2. Delete the bad branch
3. Start over from clean master
4. Verify versions match BEFORE building

### Issue: Build fails with npmDepsHash mismatch

**Cause:** update.sh doesn't update npmDepsHash automatically

**Solution:** See "Fixing npmDepsHash" section above - copy the "got:" hash from error and update package.nix
