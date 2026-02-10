# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Architecture Overview

### Host Configuration Hierarchy
```
Host configs (xps/, nuc/, p1/)
    └── Import shared modules from nixos-shared/
        ├── common-packages.nix, common-programs.nix, common-services.nix
        ├── packages/ (custom package definitions)
        └── home-manager/ (user environment configs)
```

### Host Configurations
| Host | Build Method | User | Purpose |
|------|--------------|------|---------|
| `p1/` | `laptop/activate.sh` (traditional) | markus | ThinkPad P1 (primary laptop) |
| `nuc/` | `nuc/activate.sh` (traditional) | mediacenter | Home server |
| `xps/` | Legacy (not in use) | markus | Old XPS laptop |
| `nix-on-droid/` | Separate flake | n/a | Android/Termux |

### Shared Modules (`nixos-shared/`)
- **Configuration**: `common-packages.nix`, `common-programs.nix`, `common-services.nix`
- **Custom packages**: `packages/` - emacs, xmonad, xmobarrc, kmonad, tmux, scripts
- **Home Manager**: `home-manager/` - user-level configs (git, zsh, dunst, firefox, vim, claude-code)
- **LLM packages**: `llm-packages/` - custom plugins (llm-gemini, llm-bedrock-anthropic, llm-perplexity)
- **Claude Code configs**: `claude/` - commands, skills, output-styles, docs
- **Overlays**: `shared-overlays.nix` - ndt, visidata, xclip overlays

### Secrets Management
Two systems in use:
- **agenix**: Runtime secrets (passwords, API keys) decrypted at boot. Files in `secrets/*.age`, config in `my-agenix.nix`
- **git-secret**: Build-time secrets (Nix expressions with sensitive values). Decrypt before build: `git secret reveal`

### Package Sources
- **niv**: External sources managed in `ndt/sources.nix`
- **ndt**: Custom Nix development tools from github.com/markus1189/ndt

## System Commands

### Building Configurations
```bash
# P1 ThinkPad (primary laptop) - runs sudo nixos-rebuild with p1/configuration.nix
laptop/activate.sh

# NUC home server (traditional) - runs sudo nixos-rebuild with nuc/configuration.nix
nuc/activate.sh

# Nix-on-Droid (from nix-on-droid/ directory)
nix-on-droid switch --flake ./nix-on-droid
```

### Validation and Testing
```bash
# Syntax check before building
nix-instantiate --parse path/to/file.nix

# Test without switching
nixos-rebuild test -I nixos-config=/path/to/configuration.nix

# Update flake inputs
nix flake update
```

### Option Reference (offline, version-matched)
```bash
# Browse all NixOS options (~370k lines) - always matches current system version
man configuration.nix

# Browse all Home Manager options (~82k lines)
man home-configuration.nix
```

## Development Workflow

### Adding Packages
- **System packages**: `nixos-shared/common-packages.nix`
- **User packages**: Host-specific `home.nix` files
- **Custom packages**: Create in `nixos-shared/packages/`

### Adding Services
- **Shared services**: `nixos-shared/common-services.nix`
- **Host-specific**: Respective host directory (e.g., `nuc/adguard.nix`)

### Home Manager Integration
Home Manager is integrated via `nixos-shared/home-manager/module.nix`. Each host imports its `home.nix`:
```nix
(import ../nixos-shared/home-manager/module.nix {
  homeNixFile = ./home.nix;
})
```

## Emacs Configuration

### Adding Emacs Packages
1. Add package name to `nixos-shared/packages/emacs/default.nix` (alphabetically in package list)
2. Add `use-package` configuration in `nixos-shared/packages/emacs/emacs-config.el`

Package sources: `epkgs.melpaPackages`, `epkgs.elpaPackages`, `epkgs`

### Elfeed RSS Feed Management

Feeds configured in `nixos-shared/packages/emacs/emacs-config.el`. ALWAYS verify feed doesn't already exist before adding.

**Feed Types and Patterns:**
```bash
# Search for feed sections
rg ":subreddit" nixos-shared/packages/emacs/emacs-config.el     # Reddit
rg ":channelId" nixos-shared/packages/emacs/emacs-config.el     # YouTube
rg ":owner.*:repo" nixos-shared/packages/emacs/emacs-config.el  # GitHub
rg ":id" nixos-shared/packages/emacs/emacs-config.el            # Newsletters
```

**Feed Formats (add alphabetically within each section):**
```elisp
;; Reddit
(:subreddit "name" :threshold 70 :tags (tag1))

;; YouTube (get channel ID: yt-dlp --print "%(channel_id)s" --playlist-end 1 "URL")
(:channelId "UCxxx" :title "Name" :tags (tag1))

;; GitHub releases
(:owner "user" :repo "name" :tags (github))

;; Regular RSS
("https://example.com/feed.xml" tag1 tag2)

;; Kill The Newsletter
(:id "newsletter-id" :title "Name" :tags (newsletter))
```

**Commit format**: `elfeed: add [type] [name]` (e.g., `elfeed: add r/programming`, `elfeed: add youtube TechChannel`)

**Common tags**: `programming`, `llm`, `sport`, `electronics`, `news`, `hackernews`, `github`, `newsletter`, `youtube`, `reddit`

## Autorandr Profile Management

### Adding New Profiles
1. Configure displays manually, save: `autorandr --save temp_profile`
2. Extract fingerprints: `autorandr --fingerprint`, check `~/.config/autorandr/temp_profile/{config,setup}`
3. Add to `home.nix` in `programs.autorandr.profiles` with:
   - **Fingerprint**: Only connected displays with exact EDID (not wildcards like `"DP-1" = "*"`)
   - **CRTC assignments**: Critical - copy exact values from temp config
   - **Unused ports**: Explicitly `enable = false`
4. Test: `home-manager switch && autorandr` (should show "detected")
5. Cleanup: `autorandr --remove temp_profile`

## Adding New Host Configurations

1. Create host directory with `configuration.nix` and `hardware-configuration.nix`
2. Import shared modules from `nixos-shared/` as needed
3. Create `home.nix` for user-level configuration
4. Either: create `activate.sh` (traditional) or add to `flake.nix` (flake-based)

## Claude Code Configurations

Custom Claude Code setup in `nixos-shared/claude/`:
- `commands/` - Custom slash commands (mh:agent-race, mh:iterate, mh:fact-check, etc.)
- `skills/` - Custom skills (each skill is a subdirectory with skill.md)
- `CLAUDE-global.md` - Global instructions

**Skills location**: `nixos-shared/claude/skills/[skill-name]/skill.md`

Home-manager integration in `nixos-shared/home-manager/claude-code/` supports sound hooks, deny rules, and additional allowed commands.
