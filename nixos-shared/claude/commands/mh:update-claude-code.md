---
description: Update claude-code package in nixpkgs with automated process
argument-hint: [new-version] (optional - will check npm if not provided)
allowed-tools: Bash, Read, Write, Edit, Grep, TodoWrite
---

I'll help you update the claude-code package in nixpkgs following the proper process. This involves checking versions, creating a branch, running the update script, and preparing for a PR.

Arguments: $ARGUMENTS

Let me start by creating a todo list and following the systematic update process:

1. **Prepare Repository** - Ensure we're on master and up to date with upstream
2. **Check Current and Latest Versions** - Compare nixpkgs version with npm registry
3. **Create Branch** - Only after confirming a new version exists
4. **Run Update Command** - Use nix-update via maintainer script
5. **Build and Test** - Verify the package works correctly
6. **Commit Changes** - Follow nixpkgs commit format with changelog
7. **Push Branch** - Ready for PR creation
8. **Create PR** - Use dry-run first, then create and open

**Critical Requirements:**
- Must be on latest upstream master FIRST
- Unfree license requires `NIXPKGS_ALLOW_UNFREE=1` or `env NIXPKGS_ALLOW_UNFREE=1` prefix for commands
- Follow nixpkgs commit format: `claude-code: old-version -> new-version`
- **NEVER include npm diff links** - Only use changelog link: `https://github.com/anthropics/claude-code/blob/main/CHANGELOG.md`
- **Format changed files** - Run `nix fmt` on modified .nix files before committing

**Update Command (RECOMMENDED):**
Use the maintainer update script which handles BOTH packages automatically:
```bash
./pkgs/by-name/cl/claude-code/update.sh
```

**Alternative - Manual nix-update:**
```bash
env NIXPKGS_ALLOW_UNFREE=1 nix-shell -p nix-update --run 'nix-update --build --commit claude-code'
```
⚠️ Note: This updates ONLY the claude-code CLI package. VSCode extension must be updated separately (see below).
⚠️ Important: nix-update will add npm diff link to commit message - this MUST be replaced with changelog link before pushing!

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
- If using the `update.sh` script (recommended), the VSCode extension is updated automatically ✅
- If using manual `nix-update`, you MUST manually update the VSCode extension:
  1. Edit `pkgs/applications/editors/vscode/extensions/anthropic.claude-code/default.nix`
  2. Update version to match claude-code version
  3. Set hash to empty string `""`
  4. Build to get correct hash: `env NIXPKGS_ALLOW_UNFREE=1 nix-build -A vscode-extensions.anthropic.claude-code 2>&1`
  5. Copy the correct hash from the error message line starting with "got:" (format: `sha256-...`)
  6. Update the hash in the file
  7. Rebuild to verify: `env NIXPKGS_ALLOW_UNFREE=1 nix-build -A vscode-extensions.anthropic.claude-code`
  8. Add to commit and amend (after verifying authorship): `git add pkgs/applications/editors/vscode/extensions/anthropic.claude-code/default.nix && git commit --amend --no-edit`

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
