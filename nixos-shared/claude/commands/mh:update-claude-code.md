---
description: Update claude-code package in nixpkgs with automated process
argument-hint: [new-version] (optional - will check npm if not provided)
allowed-tools: Bash, Read, Write, Edit, Grep, TodoWrite
---

Update claude-code in nixpkgs. Arguments: $ARGUMENTS

## Quick Reference

**Files:**
- `pkgs/by-name/cl/claude-code/package.nix` - Main package
- `pkgs/by-name/cl/claude-code/package-lock.json` - npm lockfile
- `pkgs/applications/editors/vscode/extensions/anthropic.claude-code/default.nix` - VSCode extension

**Branch naming:** `claude-code-OLD-to-NEW` (e.g., `claude-code-2.1.6-to-2.1.7`)

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
# Current version in nixpkgs
grep -E '^  version = ' pkgs/by-name/cl/claude-code/package.nix

# Latest on npm
curl -s https://registry.npmjs.org/@anthropic-ai/claude-code/latest | jq -r '.version'
```

**If already up-to-date:** Stop here, nothing to do.

### 3. Create Branch
```bash
git checkout -b claude-code-OLD-to-NEW
```

### 4. Run Update Script
Pipe empty input to handle interactive prompt:
```bash
echo "" | nix-shell maintainers/scripts/update.nix --arg predicate \
  '(path: pkg: builtins.elem path [["claude-code"] ["vscode-extensions" "anthropic" "claude-code"]])'
```
**Timeout:** 300000ms (5 minutes)

**If script fails:** Do NOT fix manually. Ask user or retry with higher timeout.

### 5. Verify Versions Match
All three must show the same version:
```bash
grep -E '^  version = ' pkgs/by-name/cl/claude-code/package.nix
grep 'version = ' pkgs/applications/editors/vscode/extensions/anthropic.claude-code/default.nix
grep '"version":' pkgs/by-name/cl/claude-code/package-lock.json | head -1
```

### 6. Format
```bash
nix fmt pkgs/by-name/cl/claude-code/package.nix \
       pkgs/applications/editors/vscode/extensions/anthropic.claude-code/default.nix
```

### 7. Build Both Packages
```bash
env NIXPKGS_ALLOW_UNFREE=1 nix-build -A claude-code
env NIXPKGS_ALLOW_UNFREE=1 nix-build -A vscode-extensions.anthropic.claude-code
```

**If npmDepsHash mismatch:** Copy the "got:" hash from error, update `npmDepsHash` in package.nix, rebuild.

### 8. Commit (TWO separate commits)
```bash
# First: claude-code package
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

### 10. Create PR (optional, if user requests)
Use default nixpkgs template:
```bash
gh pr create --draft --title "claude-code: OLD -> NEW" \
  --body-file .github/PULL_REQUEST_TEMPLATE.md
```

## Troubleshooting

**"Not updating version, already X.X.X"** - Not on clean master. Reset: `git checkout master && git reset --hard upstream/master`

**Versions don't match** - Dirty branch. Reset to master, delete bad branch, start over.

**npmDepsHash mismatch** - Copy "got:" hash from build error to package.nix, rebuild.
