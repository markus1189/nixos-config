# Focus the window in the current session that most recently raised an alert
# (bell `!`, activity `#` or silence `~`), i.e. the flagged window with the
# newest #{window_activity} timestamp. Selecting a window clears its flag, so
# repeated invocations walk from newest to oldest alert until none remain.
# Shebang is supplied by pkgs.writeShellScript at build time.
set -euo pipefail

target=$(
  tmux list-windows \
    -F '#{window_index} #{window_bell_flag}#{window_activity_flag}#{window_silence_flag} #{window_activity}' \
  | awk '
      $2 ~ /1/ && $3 > best { best = $3; idx = $1 }
      END { print idx }
    '
)

if [ -n "$target" ]; then
  tmux select-window -t ":$target"
else
  tmux display-message "No window with an alert"
fi
