#!/usr/bin/env bash

darktable="nix-prefetch-git --fetch-submodules https://github.com/darktable-org/darktable HEAD > darktable-version.json"
lensfun="nix-prefetch-git https://github.com/lensfun/lensfun HEAD > lensfun-version.json"
hosts="nix-prefetch-git https://github.com/StevenBlack/hosts HEAD > hosts-version.json"

parallel -j3 --tty bash -c ::: "$darktable" "$lensfun" "$hosts"
