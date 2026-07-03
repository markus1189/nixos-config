# Resolve the secrets attrset — import this instead of ./secrets.nix.
#
# The real secrets live in ./secrets.nix, produced by `git secret reveal`
# from secrets.nix.secret.  That file is gitignored, and a git-based flake
# only copies *tracked* files into the store, so under plain flake
# evaluation it can never exist — the dummies from ./secrets.dummy.nix are
# used instead (fine for eval/CI, useless on a real machine).
#
# To build a real system, reveal the secrets and use a *path* flake so
# untracked files are included in the source copy:
#
#   git secret reveal
#   sudo nixos-rebuild switch --flake "path:$PWD#<host>"
#
# (The activate.sh scripts do exactly this.)
if builtins.pathExists ./secrets.nix then
  import ./secrets.nix
else
  builtins.trace
    "WARNING: nixos-shared/secrets.nix not found (not revealed, or git-flake eval) — using DUMMY secrets"
    (import ./secrets.dummy.nix)
