# Reuse a popup-scratch window matching $PWD or create one rooted there.
# $PWD is set by `display-popup -d "#{pane_current_path}"` in the binding.
# Shebang is supplied by pkgs.writeShellScript at build time.
set -euo pipefail

tmux new-session -d -s popup-scratch -c "$PWD" -n "$PWD" 2>/dev/null || true

win=$(tmux list-windows -t popup-scratch -F $'#{window_name}\t#{window_index}' \
        | awk -F'\t' -v p="$PWD" '$1 == p { print $2; exit }')

if [ -n "$win" ]; then
  tmux select-window -t "popup-scratch:$win"
else
  tmux new-window -t popup-scratch -n "$PWD" -c "$PWD"
fi

exec tmux attach -t popup-scratch
