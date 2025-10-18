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

**Critical Requirements:**
- MUST switch to master branch FIRST - never assume you're already on the right branch
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
- `pkgs/by-name/cl/claude-code/package.nix` - Version and hashes
- `pkgs/by-name/cl/claude-code/package-lock.json` - npm dependencies
- `pkgs/applications/editors/vscode/extensions/anthropic.claude-code/default.nix` - VSCode extension

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

**Build Verification (CRITICAL):**
**ALL changed packages MUST build successfully** before committing:
```bash
env NIXPKGS_ALLOW_UNFREE=1 nix-build -A vscode-extensions.anthropic.claude-code
env NIXPKGS_ALLOW_UNFREE=1 nix-build -A claude-code
```
Both builds must succeed. If either fails, investigate and fix before proceeding.

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
