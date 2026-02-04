#!/usr/bin/env bash
set -euo pipefail

echo "Activating P1 configuration $(date)"
sudo nixos-rebuild -I nixos-config=/home/markus/repos/nixos-config/p1/configuration.nix switch
echo "Activated $(date)"

