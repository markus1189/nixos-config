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
4. **Run Update Command** - Use nix-update via maintainer script
5. **Build and Test** - Verify the package works correctly
6. **Commit Changes** - Follow nixpkgs commit format with changelog
7. **Push Branch** - Ready for PR creation
8. **Create PR** - Use dry-run first, then create and open

**Critical Requirements:**
- MUST switch to master branch FIRST - never assume you're already on the right branch
- Unfree license requires `NIXPKGS_ALLOW_UNFREE=1` or `env NIXPKGS_ALLOW_UNFREE=1` prefix for commands
- Follow nixpkgs commit format: `claude-code: old-version -> new-version`
- **NEVER include npm diff links** - Only use changelog link: `https://github.com/anthropics/claude-code/blob/main/CHANGELOG.md`
- **Format changed files** - Run `nix fmt` on modified .nix files before committing

**Update Command (RECOMMENDED):**
Use nix-update directly with explicit version:
```bash
env NIXPKGS_ALLOW_UNFREE=1 nix-shell -p nix-update --run 'nix-update --build --commit --version X.Y.Z claude-code'
```
Replace X.Y.Z with the target version number (e.g., 2.0.17).
⚠️ Important: nix-update will add npm diff link to commit message - this MUST be replaced with changelog link before pushing!

**Alternative - Maintainer update script:**
```bash
./pkgs/by-name/cl/claude-code/update.sh
```
⚠️ Note: This script may fail due to NIX_PATH issues. If it fails, use the manual nix-update method above.

**Build Verification (CRITICAL):**
After update, **ALL changed packages MUST build successfully**. For claude-code updates:
```bash
env NIXPKGS_ALLOW_UNFREE=1 nix-build -A claude-code
env NIXPKGS_ALLOW_UNFREE=1 nix-build -A vscode-extensions.anthropic.claude-code
```
Check `git status` to identify all modified packages and build each one.

**Key Files:**
- `pkgs/by-name/cl/claude-code/package.nix` - Version and hashes
- `pkgs/by-name/cl/claude-code/package-lock.json` - npm dependencies
- `pkgs/applications/editors/vscode/extensions/anthropic.claude-code/default.nix` - VSCode extension (MUST update manually!)

**IMPORTANT: VSCode Extension Update:**
The VSCode extension is NOT automatically updated by nix-update. You MUST manually update it:
  1. Edit `pkgs/applications/editors/vscode/extensions/anthropic.claude-code/default.nix`
  2. Update version to match claude-code version
  3. Set hash to empty string `""`
  4. Build to get correct hash: `env NIXPKGS_ALLOW_UNFREE=1 nix-build -A vscode-extensions.anthropic.claude-code 2>&1 | grep -A 2 "got:"`
  5. Copy the correct hash from the "got:" line (format: `sha256-...`)
  6. Update the hash in the file
  7. Rebuild to verify: `env NIXPKGS_ALLOW_UNFREE=1 nix-build -A vscode-extensions.anthropic.claude-code`
  8. Add to commit and amend (after verifying authorship with `git log -1 --format='%an %ae'`):
     ```bash
     git add pkgs/applications/editors/vscode/extensions/anthropic.claude-code/default.nix && git commit --amend --no-edit
     ```

Note: If using the `update.sh` script, it may attempt to update the VSCode extension automatically, but this often fails due to NIX_PATH issues.

**Fixing Commit Message (REQUIRED):**
nix-update adds npm diff link which MUST be replaced with changelog link:
```bash
git commit --amend -m "$(cat <<'EOF'
claude-code: OLD-VERSION -> NEW-VERSION

https://github.com/anthropics/claude-code/blob/main/CHANGELOG.md
EOF
)"
```
Replace OLD-VERSION and NEW-VERSION with actual version numbers.

**Push Branch:**
After amending, force push is required:
```bash
git push -f origin claude-code-OLD-VERSION-to-NEW-VERSION
```
Use actual version numbers in branch name.

**PR Creation Process (MANDATORY):**
When PR is ready, run `gh pr create --dry-run --fill --base master` and ask user if this is okay (MISSION CRITICAL).
Note: Do NOT use --template flag as it's not needed with --fill.
