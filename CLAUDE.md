# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## System Management Commands

### Building and Switching Configurations
- `sudo nixos-rebuild switch -I nixos-config=/path/to/configuration.nix` - Build and switch to new configuration
- Use the `activate.sh` scripts in each host directory for simplified switching:
  - `laptop/activate.sh` - Activates XPS configuration
  - `nuc/activate.sh` - Activates NUC configuration

### Host Configurations
- `xps/` - XPS laptop configuration
- `nuc/` - NUC home server configuration  
- `p1/` - P1 ThinkPad configuration
- `laptop/` - General laptop configuration files

### Flake Operations
- `nix flake update` - Update flake inputs
- `nixos-rebuild switch --flake .#nixos-p1` - Build using flake configuration

## Architecture Overview

### Configuration Structure
- **Host-specific configs**: Each machine has its own directory (`xps/`, `nuc/`, `p1/`) containing `configuration.nix` and `hardware-configuration.nix`
- **Shared modules**: `nixos-shared/` contains common configuration imported by all hosts:
  - `common-packages.nix` - System-wide packages
  - `common-programs.nix` - Program configurations
  - `common-services.nix` - System services
  - `home-manager/` - User environment configurations
  - `packages/` - Custom package definitions

### Home Manager Integration
- Home Manager is integrated via `nixos-shared/home-manager/module.nix`
- Each host imports its specific home configuration file (e.g., `./home.nix`)
- User-specific configurations are in `home-manager/` subdirectories

### Secrets Management
- Uses agenix for secret management (`my-agenix.nix`)
- Secrets are stored in `secrets/` directory as `.age` files
- `secrets.dummy.nix` shows the expected structure for secrets
- Real secrets are in `secrets.nix` (encrypted)

### Package Management
- Custom packages are defined in `nixos-shared/packages/`
- Overlays are managed in `shared-overlays.nix`
- Package sources are managed via `ndt/sources.nix` using niv

### Service Configurations
- Media services: Jellyfin, Plex, Kodi (NUC)
- Network services: AdGuard, Wireguard
- Backup services: Restic with B2 backend
- Monitoring: Prometheus setup

## Development Workflow

### Making Changes
1. Edit configuration files in appropriate host directory or `nixos-shared/`
2. Test changes with `nixos-rebuild test` before switching
3. Use `nixos-rebuild switch` or host-specific `activate.sh` scripts
4. Secrets should be added to `secrets.nix` (encrypted) with structure matching `secrets.dummy.nix`

### Adding New Packages
- System packages: Add to `nixos-shared/common-packages.nix`
- User packages: Add to appropriate `home.nix` file
- Custom packages: Create in `nixos-shared/packages/` directory

### Service Configuration
- Add new services to `nixos-shared/common-services.nix` for shared services
- Host-specific services go in the respective host directory

## Emacs Elfeed RSS Feed Management

RSS feeds are configured in `nixos-shared/packages/emacs/emacs-config.el`. The configuration supports multiple feed types organized in distinct sections.

### Quick Commands for RSS Feed Management

```bash
# Search for specific feed sections
rg ":subreddit" nixos-shared/packages/emacs/emacs-config.el
rg "channelId" nixos-shared/packages/emacs/emacs-config.el
rg "owner.*repo" nixos-shared/packages/emacs/emacs-config.el
```

### Adding RSS Feeds

#### Reddit Feeds (Alphabetical Order)
**Location**: Search for `:subreddit` - typically around line 2180-2230
**Insertion**: Add in alphabetical order within the `:subreddit` list

```elisp
(:subreddit "ai_agents")                        ; Basic reddit feed
(:subreddit "programming" :threshold 70)        ; Custom score threshold
(:subreddit "running" :tags (sport))           ; With tags
```

#### Regular RSS/Atom Feeds (Alphabetical Order) 
**Location**: Search for `("https://` - typically around line 2350-2530
**Insertion**: Add in alphabetical order by domain name

```elisp
("https://example.com/feed.xml" tag1 tag2)
("https://simonwillison.net/atom/everything/" programming llm)
```

#### YouTube Channel Feeds (Alphabetical by Title)
**Location**: Search for `:channelId` - typically around line 2270-2320
**Insertion**: Add alphabetically by title

```elisp
(:channelId "UCchannelid" :title "Channel Name" :tags (tag1 tag2))
```

#### GitHub Release Feeds (Alphabetical by Repo)
**Location**: Search for `:owner.*:repo` - typically around line 2240-2260
**Insertion**: Add alphabetically by repository name

```elisp
(:owner "username" :repo "reponame" :tags (github))
```

#### Newsletter Feeds (Kill The Newsletter)
**Location**: Search for `:id` - typically around line 2330-2340
**Insertion**: Add alphabetically by title

```elisp
(:id "newsletter-id" :title "Newsletter Name" :tags (newsletter))
```

### Workflow for Adding Feeds

1. **Search for the correct section**:
   ```bash
   rg ":subreddit" nixos-shared/packages/emacs/emacs-config.el  # For Reddit
   ```

2. **Add feed in alphabetical order** within the appropriate list

3. **Commit with standard format**:
   ```bash
   git add nixos-shared/packages/emacs/emacs-config.el
   git commit -m "elfeed: add [feed-type] [feed-name]"
   ```
   Examples:
   - `elfeed: add r/ai_agents`
   - `elfeed: add simonwillison.net RSS feed`
   - `elfeed: add youtube channel TechName`

### Tag System Reference
Common tags: `programming`, `sport`, `analog`, `electronics`, `reading`, `news`, `hackernews`, `github`, `newsletter`, `youtube`, `reddit`, `llm`, `hacking`

### Feed Processing Notes
- Reddit feeds use localhost:9999 server with threshold-based filtering
- Thresholds filter posts by score (default varies by subreddit)
- All feeds support optional tagging for organization
- YouTube feeds auto-tag with `youtube`
- GitHub feeds auto-tag with `github`

## Emacs Package Management

Emacs packages are managed through Nix in `nixos-shared/packages/emacs/default.nix`. The configuration uses `emacs.pkgs.withPackages` to create a custom Emacs with all required packages.

### Adding New Emacs Packages

To add a new Emacs package, you need to modify two files:

#### 1. Add Package to Nix Configuration
In `nixos-shared/packages/emacs/default.nix`, add the package name to the list around line 80-200:
```nix
git-link
git-timemachine        # <- New package added here
go-mode
```

Packages are sourced from:
- `epkgs.melpaPackages` - MELPA packages
- `epkgs.elpaPackages` - GNU ELPA packages  
- `epkgs` - Other package sources

#### 2. Configure Package in Emacs Config
In `nixos-shared/packages/emacs/emacs-config.el`, add the `use-package` configuration:
```elisp
(use-package git-timemachine
  :ensure t
  :bind (("C-x v t" . git-timemachine)))
```

### Custom Packages
For packages not in standard repositories, custom packages are defined at the top of `default.nix`:
- `quick-yes` - Local elisp file
- `dired-plus` - From ndt sources
- `iy-go-to-char` - From ndt sources
- `hurl-mode` - From ndt sources

### External Sources
Some packages come from `ndtSources` (managed via niv in `ndt/sources.nix`). These are typically Git repositories or specific versions not available in standard package repositories.