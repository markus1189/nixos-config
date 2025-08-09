# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

# Commands

## Nix-on-Droid

To activate the configuration after changes:
```bash
nix-on-droid switch --flake ~/.config/nix-on-droid
```

To check configuration without activating:
```bash
nix-on-droid build --flake ~/.config/nix-on-droid
```

To update flake inputs:
```bash
nix flake update
```

## Git Operations

Available tools: `git`, `gh` (GitHub CLI)

# Architecture

This is a Nix-on-Droid configuration for setting up a development environment on Android via Termux.

## Configuration Structure

- `flake.nix`: Main flake definition with inputs (nixpkgs, home-manager, nix-on-droid)
- `nix-on-droid.nix`: System and home-manager configuration
- `flake.lock`: Lockfile for reproducible builds

## Key Components

- **Shell**: Zsh with starship prompt, completion, autosuggestions, and syntax highlighting
- **Development Tools**: claude-code, git, openssh, gh (GitHub CLI), comma (for running programs not in PATH)
- **Font**: Fira Code VF automatically installed to Termux
- **Package Management**: Uses nixpkgs unstable with allowUnfree for claude-code
- **Time Zone**: Set to Europe/Berlin (configure in nix-on-droid.nix:41)

The configuration integrates home-manager for user-level package and service management within the nix-on-droid system.