# Cycle to the next window in the current session that is flagged for a
# bell (!), activity (#) or silence (~), wrapping around. Repeated invocations
# advance through the flagged windows, since visiting a window clears its flag.
# $1 = current window index (passed from the binding via `#{window_index}`);
# falls back to querying tmux if absent. Shebang is supplied by
# pkgs.writeShellScript at build time.
set -euo pipefail

cur=${1:-$(tmux display-message -p '#{window_index}')}

target=$(
  tmux list-windows \
    -F '#{window_index} #{window_bell_flag}#{window_activity_flag}#{window_silence_flag}' \
  | awk -v cur="$cur" '
      $2 ~ /1/ {
        if (first == "")           first = $1
        if ($1 > cur && nxt == "") nxt   = $1
      }
      END { print (nxt != "" ? nxt : first) }
    '
)

if [ -n "$target" ]; then
  tmux select-window -t ":$target"
else
  tmux display-message "No window with a bell"
fi
