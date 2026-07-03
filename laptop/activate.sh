#!/usr/bin/env bash
set -euo pipefail

# Uses a `path:` flake (not a plain git flake) so the untracked,
# git-secret revealed nixos-shared/secrets.nix is included in the flake
# source — a git flake only copies tracked files and would silently
# build with the dummy secrets from secrets.dummy.nix.
REPO="/home/markus/repos/nixos-config"

if [[ ! -f "${REPO}/nixos-shared/secrets.nix" ]]; then
  echo "ERROR: ${REPO}/nixos-shared/secrets.nix is missing — run 'git secret reveal' first." >&2
  echo "       Building without it would silently use the dummy secrets." >&2
  exit 1
fi

echo "Activating P1 configuration $(date)"
sudo nixos-rebuild switch --flake "path:${REPO}#p1"
echo "Activated $(date)"
