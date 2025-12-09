---
description: Update claude-code package in nixpkgs with automated process
argument-hint: [new-version] (optional - will check npm if not provided)
allowed-tools: Bash, Read, Write, Edit, Grep, TodoWrite
---

I'll help you update the claude-code package in nixpkgs following the proper process. This involves checking versions, creating a branch, running the update script, and preparing for a PR.

Arguments: $ARGUMENTS

## ⚠️ CRITICAL: MANDATORY FIRST STEP - SWITCH TO MASTER ⚠️

**STOP! READ THIS FIRST!**

You MUST execute these commands BEFORE doing ANYTHING else:

```bash
git checkout master
git fetch upstream && git pull upstream master
```

**ABSOLUTE RULES:**
- ❌ **NEVER** assume you are already on master
- ❌ **NEVER** assume you are starting fresh
- ❌ **NEVER** continue if you see an existing `claude-code-update-*` branch
- ✅ **ALWAYS** switch to master first, no exceptions
- ✅ **ALWAYS** pull latest from upstream
- ✅ **ALWAYS** check git status shows clean master before proceeding

**If you see an existing update branch or any uncommitted changes: STOP, switch to master, reset hard.**

---

## Workflow Steps

After switching to master and confirming clean state, create a todo list:

1. **Prepare Repository** - Switch to master and update with upstream (MANDATORY FIRST)
2. **Check Current and Latest Versions** - Compare nixpkgs version with npm registry
3. **Create Branch** - Only after confirming a new version exists
4. **Run update.sh Script** - MANDATORY: Use automated script with high timeout (120s+)
5. **Handle Script Issues** - If script fails/hangs: STOP and ask user OR retry with higher timeout
6. **Fix npmDepsHash** - ONLY after update.sh completes, fix hash from build error
7. **Format Files** - Run nix fmt on changed .nix files
8. **Build and Test** - Verify BOTH packages build successfully
9. **Commit Changes** - Create TWO separate commits (one per package) following nixpkgs format with changelog
10. **Push Branch** - Ready for PR creation

## Critical Requirements

**ABSOLUTE PROHIBITIONS:**
- ❌ **NEVER manually update files instead of running update.sh**
- ❌ **NEVER kill update.sh and fix things yourself**
- ❌ **NEVER proceed if update.sh fails - STOP and ask or retry**
- ❌ **NEVER work on an existing update branch**

**MANDATORY REQUIREMENTS:**
- ✅ MUST switch to clean master branch FIRST
- ✅ MUST run update.sh with timeout ≥ 120000ms (2 minutes)
- ✅ If update.sh hangs: STOP, ask user, or retry with higher timeout (300000ms)
- ✅ BOTH packages MUST be at SAME version
- ✅ After update.sh: ONLY fix npmDepsHash if build fails, nothing else
- ✅ Unfree license requires `NIXPKGS_ALLOW_UNFREE=1` prefix
- ✅ Follow nixpkgs commit format: `claude-code: old-version -> new-version`
- ✅ Use changelog link only: `https://github.com/anthropics/claude-code/blob/main/CHANGELOG.md`
- ✅ Format with `nix fmt` before committing
- ✅ Create TWO separate commits: one for claude-code package, one for vscode-extensions

## Update Command (MANDATORY - NOT OPTIONAL)

**You MUST run this script (referred to as update.sh). Do NOT manually update files:**

```bash
nix-shell maintainers/scripts/update.nix --argstr commit true --arg predicate '(path: pkg: builtins.elem path [["claude-code"] ["vscode-extensions" "anthropic" "claude-code"]])'
```

**Script timeout:** Use `timeout: 120000` minimum (2 minutes). If it hangs, retry with `timeout: 300000` (5 minutes).

**If script fails or hangs:**
1. DO NOT try to fix manually
2. STOP and ask the user what to do
3. OR retry with higher timeout
4. Show user the error output

This script automatically updates:
- Version and src hash in package.nix
- package-lock.json for npm dependencies
- VSCode extension version and hash
- All necessary lockfiles

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

**Commit Changes (TWO SEPARATE COMMITS):**
After formatting and verifying builds succeed, create TWO commits (one per package):

**First commit - claude-code package:**
```bash
git add pkgs/by-name/cl/claude-code/package.nix pkgs/by-name/cl/claude-code/package-lock.json
git commit -m "$(cat <<'EOF'
claude-code: OLD-VERSION -> NEW-VERSION

https://github.com/anthropics/claude-code/blob/main/CHANGELOG.md
EOF
)"
```

**Second commit - vscode extension:**
```bash
git add pkgs/applications/editors/vscode/extensions/anthropic.claude-code/default.nix
git commit -m "$(cat <<'EOF'
vscode-extensions.anthropic.claude-code: OLD-VERSION -> NEW-VERSION

https://github.com/anthropics/claude-code/blob/main/CHANGELOG.md
EOF
)"
```

Replace OLD-VERSION and NEW-VERSION with actual version numbers in both commits.

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
