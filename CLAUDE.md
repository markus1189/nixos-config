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
- `nix-on-droid/` - Nix-on-Droid configuration for Android/Termux

### Flake Operations
- `nix flake update` - Update flake inputs
- `nixos-rebuild switch --flake .#nixos-p1` - Build using flake configuration

### Nix-on-Droid Operations
- `nix-on-droid switch --flake ./nix-on-droid` - Switch to nix-on-droid configuration
- `nix-on-droid build --flake ./nix-on-droid` - Build without switching
- Configuration entry point: `nix-on-droid/nix-on-droid.nix`

## Architecture Overview

### Configuration Structure
- **Host-specific configs**: Each machine has its own directory (`xps/`, `nuc/`, `p1/`) containing `configuration.nix` and `hardware-configuration.nix`
- **Nix-on-Droid config**: `nix-on-droid/` contains Android/Termux configuration with flake-based setup including home-manager integration
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

### Hardware Detection
- Hardware-specific settings are configured via `lib._custom_` options
- The `hardware-detection.nix` module formalizes hardware-related configuration
- **Wireless Interface Detection**:
  - Each host specifies its wireless interface name via `lib._custom_.wirelessInterface`
  - To find your wireless interface, run: `detect-wireless-interface` (provided by hardware-detection module)
  - Or manually: `ip link show | grep -E '^[0-9]+: wl'`
  - Common patterns: `wlp2s0`, `wlp0s20f3`, `wlp58s0`
  - Set in host-specific files (e.g., `xps/xps.nix`, `nuc/configuration.nix`, `p1/p1.nix`)

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
2. **Validate syntax**: Check Nix file syntax with `nix-instantiate --parse <file>.nix`
3. Test changes with `nixos-rebuild test` before switching
4. Use `nixos-rebuild switch` or host-specific `activate.sh` scripts
5. Secrets should be added to `secrets.nix` (encrypted) with structure matching `secrets.dummy.nix`

### Syntax Validation
Before building configurations, validate Nix syntax:
```bash
# Check syntax of a specific file
nix-instantiate --parse laptop/home.nix

# Successful validation returns no output
# Syntax errors show line numbers and error details
```

### Adding New Packages

**Package Organization Hierarchy:**

Packages are organized by scope and use case:

1. **`nixos-shared/common-packages.nix`** - Core CLI tools shared by ALL hosts
   - **Criteria**: CLI-only, useful on servers and desktops, no GUI dependencies
   - **Examples**: git, jq, nix tools, htop, curl, wget, bat, httpie
   - **When to use**: When adding universal command-line utilities

2. **`laptop/packages.nix`** - Laptop-specific GUI & development tools
   - **Criteria**: GUI applications, desktop utilities, development tools for interactive use
   - **Examples**: chromium, spotify, discord, emacs, docker-compose, rofi, gimp
   - **When to use**: GUI apps, desktop utilities, development tools for XPS and P1
   - **Imported by**: XPS and P1 configurations

3. **`nuc/packages.nix`** - NUC media server packages
   - **Criteria**: Media playback, server operations, headless-friendly tools
   - **Examples**: bashmount, tigervnc, remind, wyrd
   - **When to use**: Packages needed specifically for the NUC media server
   - **Note**: Service-specific packages (kodi, plex) stay in their respective service files

4. **`p1/packages.nix`** - P1 work-specific tools (future use)
   - **Criteria**: Only packages needed specifically for work on P1, not on XPS
   - **Examples**: Corporate VPN clients, work-only applications
   - **When to use**: Tools only needed for work laptop

5. **User packages** - Add to appropriate `home.nix` file
   - For user-level packages managed by home-manager

6. **Custom packages** - Create in `nixos-shared/packages/` directory
   - For custom package definitions and scripts

**Decision Tree:**
```
Is it a GUI application or desktop utility?
├─ Yes → laptop/packages.nix
└─ No → Is it needed on ALL systems (including servers)?
    ├─ Yes → nixos-shared/common-packages.nix
    └─ No → Host-specific packages.nix (nuc/packages.nix, p1/packages.nix, etc.)
```

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

ALWAYS make sure that the feed you are adding does not yet exist!

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

**WebFetch Usage**: Before adding, use WebFetch to analyze the website:
- Extract the proper site title for consistent naming
- Identify content topics to assign relevant tags
- Verify the RSS/Atom feed URL is functional

```elisp
("https://example.com/feed.xml" tag1 tag2)
("https://simonwillison.net/atom/everything/" programming llm)
```

#### YouTube Channel Feeds (Alphabetical by Title)
**Location**: Search for `:channelId` - typically around line 2270-2320
**Insertion**: Add alphabetically by title

**Getting Channel ID**: Use yt-dlp to extract channel ID from YouTube URL:
```bash
yt-dlp --print "%(channel_id)s" --playlist-end 1 "https://www.youtube.com/@channelname"
```

**WebFetch Usage**: Use WebFetch to analyze the YouTube channel:
- Extract the proper channel title for consistent naming
- Identify channel content themes to assign relevant tags
- Verify the channel is active and matches expected content type

```elisp
(:channelId "UCchannelid" :title "Channel Name" :tags (tag1 tag2))
```

#### GitHub Release Feeds (Alphabetical by Repo)
**Location**: Search for `:owner.*:repo` - typically around line 2240-2260
**Insertion**: Add alphabetically by repository name

**WebFetch Usage**: Use WebFetch to analyze the GitHub repository:
- Extract repository description and primary language
- Identify project type to assign relevant tags beyond `github`
- Verify the repository is actively maintained and has releases

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

1. **Extract feed information using WebFetch**:
   For RSS/Atom feeds and websites, use WebFetch to determine proper titles and relevant tags:
   ```bash
   # Use WebFetch to analyze the website and extract:
   # - Proper site/blog title for consistent naming
   # - Content topics to determine appropriate tags
   # - Verify RSS/Atom feed URL is correct
   ```

2. **Search for the correct section**:
   ```bash
   rg ":subreddit" nixos-shared/packages/emacs/emacs-config.el  # For Reddit
   ```

3. **Add feed in alphabetical order** within the appropriate list

4. **Commit with standard format**:
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

## Autorandr Profile Management

Autorandr profiles in NixOS home-manager configurations require careful setup to ensure proper display detection and switching. This section documents the proven workflow for adding new autorandr profiles.

### Workflow for Adding New Autorandr Profiles

#### 1. Manual Configuration Phase
First, configure your displays manually to the desired layout:

1. **Set up displays manually**:
   - Use xrandr, GUI tools, or other display management tools
   - Configure resolution, positioning, primary display, etc.
   - Ensure the setup works correctly

2. **Save temporary profile**:
   ```bash
   autorandr --save temp_profilename
   ```

3. **Extract configuration data**:
   ```bash
   # Get display fingerprints (EDID values)
   autorandr --fingerprint
   
   # View the generated config
   cat ~/.config/autorandr/temp_profilename/config
   cat ~/.config/autorandr/temp_profilename/setup
   ```

#### 2. NixOS Integration Steps

Add the profile to your host's `home.nix` file in the `programs.autorandr.profiles` section:

```nix
"profilename" = {
  fingerprint = {
    # Only include connected displays - exclude disconnected ones
    "DP-2" = "00ffffffffffff0010acefa04c523430..."; # Full EDID from setup file
    "DP-3" = "00ffffffffffff0010acf4a04c523430..."; # Full EDID from setup file  
    "eDP-1" = internalDisplay; # Use existing variable if available
  };

  config = {
    "DP-1".enable = false; # Explicitly disable unused ports
    "DP-2" = {
      enable = true;
      crtc = 2; # CRITICAL: Include CRTC assignment from working config
      position = "1920x0";
      mode = "1920x1600";
      rate = "59.95";
    };
    "DP-3" = {
      primary = true;
      enable = true;
      crtc = 0; # CRITICAL: Include CRTC assignment from working config
      position = "0x0";
      mode = "1920x1600";
      rate = "59.95";
    };
    "eDP-1".enable = false; # Internal display off for docking station setup
  };
};
```

#### 3. Key Configuration Points

**CRTC Assignments**: The most critical element for working profiles. Extract from your temporary config:
```bash
# Look for lines like these in temp config:
# output DP-3
# crtc 0        <- This number goes in your nix config
# output DP-2  
# crtc 2        <- This number goes in your nix config
```

**Fingerprint Management**:
- Only include displays that are actually connected in your target setup
- Do NOT include `"DP-1" = "*";` if DP-1 is disconnected - this prevents profile detection
- Use exact EDID values from the setup file, not wildcards

**Display Properties**:
- Copy exact values for mode, rate, and position from working config
- Include primary display designation
- Explicitly disable unused displays

#### 4. Testing and Verification

1. **Apply configuration**:
   ```bash
   home-manager switch
   ```

2. **Verify profile detection**:
   ```bash
   autorandr  # Should show your profile as "(detected)"
   ```

3. **Test profile switching**:
   ```bash
   autorandr --dry-run profilename  # Shows what commands would run
   autorandr --load profilename     # Actually applies the profile
   ```

4. **Compare configurations** (if issues):
   ```bash
   # Save current working setup as temp profile
   autorandr --save temp_working
   
   # Compare with generated profile
   diff -u ~/.config/autorandr/profilename/config ~/.config/autorandr/temp_working/config
   diff -u ~/.config/autorandr/profilename/setup ~/.config/autorandr/temp_working/setup
   ```

#### 5. Common Issues and Solutions

**Profile not detected**:
- Check fingerprint section only includes connected displays
- Remove entries for disconnected displays (like `"DP-1" = "*";`)
- Verify EDID values match exactly

**Profile loads but displays wrong**:
- Missing CRTC assignments - add `crtc = N;` to each display config
- Wrong positioning or mode values - copy exact values from working setup

**Home-manager build fails**:
- Syntax errors in nix configuration
- Missing quotes around EDID strings
- Malformed display configuration

#### 6. Cleanup

After successful configuration, remove temporary profiles:
```bash
autorandr --remove temp_profilename
```

### Example: Complete Working Profile

Here's a complete example of a working dual-monitor profile:

```nix
"homeoffice" = {
  fingerprint = {
    "DP-2" = "00ffffffffffff0010acefa04c5234300a1e010380582578eeee95a3544c99260f5054a54b00714f81008180a940d1c00101010101012d5080a070402e6030203a00706f3100001a000000ff00354b4330333033353034524c0a000000fc0044454c4c20553338313844570a000000fd001855197322000a20202020202001fa020322f14d9005040302071601141f12135a230907078301000067030c0010003844023a801871382d40582c4500706f3100001e565e00a0a0a0295030203500706f3100001acd4600a0a0381f4030203a00706f3100001a134c00a0f040176030203a00706f3100001a000000000000000000000000000000000000000000d8";
    "DP-3" = "00ffffffffffff0010acf4a04c5234300a1e0104b55825783eee95a3544c99260f5054a54b00714f81008180a940d1c00101010101012d5080a070402e6030203a00706f3100001a000000ff00354b4330333033353034524c0a000000fc0044454c4c20553338313844570a000000fd001855197328000a202020202020016902031af14d9005040302071601141f12135a2309070783010000023a801871382d40582c4500706f3100001e565e00a0a0a0295030203500706f3100001acd4600a0a0381f4030203a00706f3100001a4c9a00a0f0402e6030203a00706f3100001a134c00a0f040176030203a00706f3100001a0000000000000000000000000000000000000000ea";
    "eDP-1" = internalDisplay;
  };

  config = {
    "DP-1".enable = false;
    "DP-2" = {
      enable = true;
      crtc = 2;
      position = "1920x0";
      mode = "1920x1600";
      rate = "59.95";
    };
    "DP-3" = {
      primary = true;
      enable = true;
      crtc = 0;
      position = "0x0";
      mode = "1920x1600";
      rate = "59.95";
    };
    "eDP-1".enable = false;
  };
};
```

This example shows:
- Only connected displays (DP-2, DP-3, eDP-1) in fingerprints
- Proper CRTC assignments (0 and 2)
- Exact EDID fingerprints from working setup
- Complete display configuration with positioning and modes
