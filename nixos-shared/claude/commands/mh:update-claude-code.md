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
4. **Run Update Script** - Use the automated update script
5. **Build and Test** - Verify the package works correctly
6. **Commit Changes** - Follow nixpkgs commit format with changelog
7. **Push Branch** - Ready for PR creation

**Critical Requirements:**
- Must be on latest upstream master FIRST
- Unfree license requires `NIXPKGS_ALLOW_UNFREE=1`
- Follow nixpkgs commit format: `claude-code: old-version -> new-version`
- Include relevant changelog entries if found, otherwise just changelog link (no generic sentences)
- **Format changed files** - Run `nix fmt` on modified .nix files before committing

**Key Files:**
- `pkgs/by-name/cl/claude-code/package.nix` - Version and hashes
- `pkgs/by-name/cl/claude-code/package-lock.json` - npm dependencies
- `pkgs/by-name/cl/claude-code/update.sh` - Automated update script

Let me start the process...