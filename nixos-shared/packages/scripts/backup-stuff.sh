# Mirror ~/Stuff to Google Drive backup, skipping whales + regenerable junk.
# Writes through the existing gdrive: FUSE mount (no rclone remote config needed).
#
# One writer only. `rclone sync` mirrors, so a host whose ~/Stuff lacks files
# another host has would delete them from the backup. Only notes and code sync
# between hosts (Syncthing, nixos-shared/stuff-patterns.nix); everything else
# lives on one host. So the destination belongs to BACKUP_HOST, every other
# host refuses to run, and other hosts' local-only files are not backed up.
#
# Decompiler output is the thing to watch: it is enormous by file COUNT, not by
# size, and Drive bills a round trip per file. On 2026-09-18 the gdrive mount
# came back from a restart owing 10614 tiny files (59 MiB) -- 8900 of them .java
# from one jadx run -- and could not finish uploading them inside the unit's
# start timeout. Hence the excludes are deliberately broad. They are
# generated into $EXCLUDES (prepended by the Nix wrapper) from
# nixos-shared/stuff-patterns.nix, the list the Syncthing `stuff` folder
# skips too; see there for what each pattern catches.
#
# Usage:
#   backup-stuff --dry-run     # rehearse; prints what would transfer/delete
#   backup-stuff               # do it, with live progress
#   MAX_SIZE=25M backup-stuff  # tighter size cap for this run
# Run --dry-run first the first time to confirm the exclude set.

readonly BACKUP_HOST=p1g8

host="$(uname -n)"
if [ "$host" != "$BACKUP_HOST" ]; then
  printf 'backup-stuff: the backup mirrors %s'\''s ~/Stuff; refusing on %s.\n' \
    "$BACKUP_HOST" "$host" >&2
  printf 'backup-stuff: rclone sync would delete what this host lacks.\n' >&2
  exit 1
fi

SRC="$HOME/Stuff"
DEST="$HOME/mounts/rclone/gdrive/Ablage/Backup/Stuff"
MAX_SIZE="${MAX_SIZE:-50M}"     # tune: 50M keeps meeting audio; 25M/10M drop more

rclone sync "$SRC" "$DEST" \
  --progress \
  --transfers=8 \
  --max-size "$MAX_SIZE" \
  --exclude-from "$EXCLUDES" \
  "$@"
