#!/usr/bin/env bash
set -euo pipefail

# `path:` flake so the untracked, git-secret revealed
# nixos-shared/secrets.nix is included — see laptop/activate.sh.
REPO="/home/mediacenter/repos/nixos-config"

if [[ ! -f "${REPO}/nixos-shared/secrets.nix" ]]; then
  echo "ERROR: ${REPO}/nixos-shared/secrets.nix is missing — run 'git secret reveal' first." >&2
  echo "       Building without it would silently use the dummy secrets." >&2
  exit 1
fi

echo "activating $(date)"
sudo nixos-rebuild switch --flake "path:${REPO}#nuc"
echo "activated $(date)"
