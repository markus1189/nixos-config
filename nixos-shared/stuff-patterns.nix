# What of ~/Stuff leaves the machine it was written on, in one place.
# Read by ./syncthing.nix (the `stuff` folder's .stignore) and by
# ./packages/scripts/default.nix (backup-stuff's rclone excludes), so
# the sync and the backup cannot disagree about what is junk.
#
# Plain names only; each consumer adds its own glob syntax (Syncthing
# matches a bare name at any depth, rclone `name/**`).
{
  # Generated or vendored trees agents leave in day dirs. Decompiler
  # output is the thing to watch: enormous by file COUNT, not size.
  #   *venv*     .venv, venv, and oddities like .unitypy_venv
  #   jadx*      jadx-out AND jadx_out AND jadx_full
  #   sources    jadx's java output; every such dir under ~/Stuff is generated
  #   decompiled generic catch for the same thing under another name
  # They also hold notes nobody wrote (node_modules alone ships ~2,850
  # .md), so the sync must skip them before it lets *.md through.
  junkDirs = [
    ".git"
    ".direnv"
    "__pycache__"
    "node_modules"
    "*venv*"
    ".mypy_cache"
    ".pytest_cache"
    "target"
    "jadx*"
    "sources"
    "decompiled"
    "smali*"
  ];

  # The only files that travel between hosts: notes and the code they
  # talk about. Opt-in, so the next 22G of agent scratch stays local
  # without anyone extending a list (measured 2026-09-28 on p1g8: these
  # are ~2,070 files / 36 MiB of 48k files / 11.6 GiB after junkDirs).
  syncExtensions = [
    "md"
    "org"
    "py"
    "sh"
    "nix"
    "hs"
    "ts"
    "el"
    "toml"
    "yaml"
    "yml"
  ];

  # Every file below a dir of this name syncs, whatever its type. Not
  # plain `sync`: extracted Kotlin/npm trees have 6 (2026-10-03).
  syncAllFilesDir = "_sync-all-files";
}
