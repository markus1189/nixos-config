# Agents documentation for nixos-configuration

This repository is the source of truth for shared and per-machine
configuration of nixos machines: servers, laptops, mobile phones.

WARNING: this repo is public

## Architecture Overview

### Host Configuration Hierarchy
```
Host configs (p1/, p1g8/, nuc/; laptops share laptop/laptop.nix)
    └── Import shared modules from nixos-shared/
        ├── common-packages.nix, common-programs.nix, common-services.nix
        ├── packages/ (custom package definitions)
        └── home-manager/ (user environment configs)
```

### Host Configurations
| Host | Build Method | User | Purpose |
|------|--------------|------|---------|
| `p1/` | `./activate.sh` (flake attr `p1`, hostname `nixos-p1`) | markus | ThinkPad P1 (primary laptop) |
| `p1g8/` | `./activate.sh p1g8` | markus | ThinkPad P1 Gen 8 |
| `nuc/` | `./activate.sh` (flake attr `nuc`) | mediacenter | Home server |
| `nix-on-droid/` | Separate flake | n/a | Android/Termux |

### Shared Modules (`nixos-shared/`)
- **Configuration**: `common-packages.nix`, `common-programs.nix`, `common-services.nix`
- **Custom packages**: `packages/` - emacs, xmonad, xmobarrc, kanata, tmux, scripts
- **Home Manager**: `home-manager/` - user-level configs (git, zsh, dunst, firefox, vim, claude-code)
- **Claude Code configs**: `claude/` - commands, skills, output-styles, docs
- **Overlays**: `shared-overlays.nix` (a function of flake `inputs`) - wallpapers, visidata, xclip overlays; flake-level overlays (emacs-overlay, masterPkgs) in `flake-base.nix`

### Secrets Management
All secrets are **agenix** runtime secrets, decrypted to `/run/agenix/*` at
boot: files in `secrets/*.age`, recipient rules in `secrets/secrets.nix`,
shared declarations in `nixos-shared/runtime-secrets.nix` (user-owned tokens)
plus per-module `age.secrets` declarations. Edit with `agenix -e secrets/<f>.age`.
Nothing secret is embedded in the nix store at build time.

### Package Sources
All external dependencies are **flake inputs** (see `flake.nix` / `flake.lock`):
- Modules/flakes: `home-manager` (nix-community), `agenix`, `disko`,
  `emacs-overlay`, `nix-index-database`, `marginal` (own flake output →
  `pkgs.marginal`), `nixpkgs-master` (→ `pkgs.masterPkgs`),
  `llm-agents` (numtide; → `pkgs.agent-browser`, overriding nixpkgs' much
  older one, and the source of the agent-browser skill text)
- Non-flake source trees (`flake = false`): darktable (with submodules!),
  fasd, visidata, gptel, xclip, stevenblack-hosts, zsh-histdb, dired-plus,
  iy-go-to-char
- Consumers take `inputs` in their module arguments (NixOS modules get it
  via `specialArgs`, home-manager modules via `extraSpecialArgs`, both set
  up in `flake.nix` / `nixos-shared/flake-base.nix`) and use `inputs.<name>`
  directly as a source path; `.rev`, `.shortRev` and `.lastModifiedDate`
  are available for version strings
- Update: `nix flake update <input>` (or all: `nix flake update`), then rebuild

## System Commands

### Building Configurations
```bash
# Any host (defaults to $(hostname); p1's hostname nixos-p1 is aliased)
./activate.sh            # sudo nixos-rebuild switch --flake .#<host>
./activate.sh p1g8       # explicit host attr

# Nix-on-Droid (from nix-on-droid/ directory; separate flake)
nix-on-droid switch --flake ./nix-on-droid
```

### Validation and Testing
```bash
# Syntax check before building
nix-instantiate --parse path/to/file.nix

# Evaluate a host without building (fast)
nix eval --raw .#nixosConfigurations.p1.config.system.build.toplevel.drvPath

# Build without switching, then inspect the delta
nixos-rebuild build --flake .#p1
nix store diff-closures /run/current-system ./result

# Derivation-level delta without building anything
diff <(nix-store -qR "$(nix-store --query --deriver /run/current-system)" | sort) \
     <(nix-store -qR "$(nix eval --raw .#nixosConfigurations.p1.config.system.build.toplevel.drvPath)" | sort)

# Build or run a single custom script (drv-identical to the host-installed set)
nix build .#myScripts.<scriptName>
nix run .#myScripts.<scriptName>

# Eval-only sanity across all outputs (fast); full check also runs the
# bats suites (claude statusline + dangerous-commands hook) as flake checks
nix flake check --no-build
nix flake check

# Update flake inputs (all or one)
nix flake update
nix flake update emacs-overlay
```

**Flake caveat**: only **git-tracked** files exist for flake evaluation —
`git add` new files before building, or eval fails with "path does not exist".
A tree still being edited also re-hashes on every eval — commit or `git add -A`
before comparing anything.

**Diffing what a change did** — what changed, what shouldn't have, why
something is in the closure; scoped to one package or host, not all of them.
`system.configurationRevision` changes every toplevel by design, so the
invariant is *which* drvs differ, never *that* the drvPath differs. Method,
scoping, failure modes: [docs/derivation-diffing.md](docs/derivation-diffing.md).

**nuc update model**: `system.autoUpgrade` rebuilds nightly from the
committed `flake.lock` (no channel, no automatic input updates). Updating
nuc means `nix flake update` + commit on a laptop, then pull on nuc.

### Option Reference (offline, version-matched)
```bash
# Browse all NixOS options (~370k lines) - always matches current system version
man configuration.nix

# Browse all Home Manager options (~82k lines)
man home-configuration.nix
```

## Development Workflow

### Adding Packages
- **System packages (all hosts)**: `nixos-shared/common-packages.nix`
- **System packages (laptop only)**: `laptop/programs.nix`
- **User packages**: Host-specific `home.nix` files
- **Custom packages**: Create in `nixos-shared/packages/`
- **Python packages**: `nixos-shared/python-env.nix` — the global `python3`
  (all hosts) is a `withPackages` env; no pip.

### Adding Services
- **Shared services**: `nixos-shared/common-services.nix`
- **Host-specific**: Respective host directory (e.g., `nuc/adguard.nix`)

### Home Manager Integration
Shared invariants (`useUserPackages`/`useGlobalPkgs`) live in
`nixos-shared/home-manager/module.nix`, imported once via `flake-base.nix`.
Each host wires its `home.nix` with the standard idiom:
```nix
home-manager.users.${config.my.userName}.imports = [ ./home.nix ];
```
The imports-list form merges: several modules can contribute to the same
user (e.g. `laptop/home.nix` + per-host autorandr profiles in
`p1/home.nix` / `p1g8/home.nix`).

## Commit Conventions

`<scope>: <imperative summary>`, where scope is the component that
changed (reuse an existing one from `git log --oneline`). The scope is
**what changed, not who changed it** — `claude-code:` means the
claude-code package changed, not that an agent made the commit.

## Emacs Configuration

### emacs-overlay (nix-community)

`nixos-shared/flake-base.nix` applies the [nix-community/emacs-overlay](https://github.com/nix-community/emacs-overlay), **pinned via `flake.lock`** (update with `nix flake update emacs-overlay`). This overlay replaces `emacs.pkgs` entirely with a newer MELPA snapshot, so all Emacs package versions come from the overlay, not base nixpkgs.

**This is the first thing to check when an Emacs package build breaks.** Two failure modes:

| Symptom | Cause | Fix |
|---|---|---|
| `"marked as broken, refusing to evaluate"` | `meta.broken = true` — overlay's MELPA JSON has an `error` field for this version | `overrideScope` + `meta = { broken = false; }` |
| `"variable $src should point to the source"` | `src = null` — overlay has a new MELPA version but hasn't computed its sha256 yet | `overrideScope` + `fetchzip` pinned to last good commit |

**Fix pattern** (in `nixos-shared/packages/emacs/default.nix`):
```nix
# Add fetchzip to function args, then:
emacsPackages = emacs.pkgs.overrideScope (self: super: {
  somepackage = super.somepackage.overrideAttrs (_: {
    src = fetchzip {
      url = "https://github.com/owner/repo/archive/<last-good-commit>.tar.gz";
      sha256 = "<sha256-from-overlay-json>";
    };
    meta = { broken = false; };
  });
});
```

Use `overrideScope` (not just `overrideAttrs` on the list item) so transitive dependents also get the fixed version.

**Finding the last good sha256** from cached overlay snapshots in the nix store:
```bash
find /nix/store -maxdepth 2 -name "recipes-archive-melpa.json" \
  | xargs nix run nixpkgs#jq -- -r \
      --arg p "somepackage" \
      '.[] | select(.ename==$p) | [(.unstable.version|join(".")), .unstable.sha256, .unstable.commit] | @tsv' \
  2>/dev/null | sort -r | head
```

These workarounds are **temporary** — remove them once the overlay's JSON catches up.

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
4. Add the host to `nixosConfigurations` in `flake.nix` (via `mkHost`)

## Claude Code Configurations

Custom Claude Code setup in `nixos-shared/claude/`:
- `commands/` - Custom slash commands (mh:agent-race, mh:iterate, mh:fact-check, etc.)
- `CLAUDE-global.md` - Global instructions

**Skills location**: `nixos-shared/agent-skills/[skill-name]/SKILL.md` —
harness-neutral, one validated derivation per skill; build a single one with
`nix build .#agentSkills.<skillName>`. Skills sourced from other repos come
from a flake input or, like agent-browser, straight out of the package's own
`$out` so text and binary cannot drift apart.

Home-manager integration in `nixos-shared/home-manager/claude-code/` supports sound hooks, deny rules, and additional allowed commands.
