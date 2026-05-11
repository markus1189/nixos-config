# Reuse a popup-scratch window matching $PWD or create one rooted there.
# Window names use just the basename for a tidy status line; full $PWD is
# stored as a per-window `@cwd` option and used for dedup matching.
# $PWD is set by `display-popup -d "#{pane_current_path}"` in the binding.
# Shebang is supplied by pkgs.writeShellScript at build time.
set -euo pipefail

name=${PWD##*/}
[ -z "$name" ] && name=/   # root

if ! tmux has-session -t popup-scratch 2>/dev/null; then
  tmux new-session -d -s popup-scratch -c "$PWD" -n "$name"
  tmux set-option -w -t 'popup-scratch:^' @cwd "$PWD"
fi

win=$(tmux list-windows -t popup-scratch -F $'#{@cwd}\t#{window_index}' \
        | awk -F'\t' -v p="$PWD" '$1 == p { print $2; exit }')

if [ -z "$win" ]; then
  tmux new-window -t popup-scratch -n "$name" -c "$PWD"
  win=$(tmux display-message -p -t popup-scratch '#{window_index}')
  tmux set-option -w -t "popup-scratch:$win" @cwd "$PWD"
fi

tmux select-window -t "popup-scratch:$win"
exec tmux attach -t popup-scratch
