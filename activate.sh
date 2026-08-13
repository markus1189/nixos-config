#!/usr/bin/env bash
set -euo pipefail

repo="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
host="${1:-$(hostname)}"

echo "Activating ${host} configuration $(date)"
sudo nixos-rebuild switch --flake "${repo}#${host}"
echo "Activated $(date)"
