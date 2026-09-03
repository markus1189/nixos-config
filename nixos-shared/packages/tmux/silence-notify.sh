# Raise a dunst notification for a tmux silence alert. The bell tmux sends on
# silence (visual-silence off) only reaches the terminal emulator, which is
# useless when the ghostty window sits on another workspace.
#
# Called from the alert-silence hook with the *alerting* window's fields, not
# the current one. tmux fires the hook once per silence episode -- the silence
# timer only re-arms once the pane produces output again -- so this cannot
# spam. With the default silence-action "other" nothing fires while the window
# is focused; set silence-action "any" to be told regardless.
#
# Shebang is supplied by pkgs.writeShellScript at build time.
set -euo pipefail

session="$1"
index="$2"
name="$3"
window_id="$4"

# run-shell inherits the tmux *server's* environment: a server started over
# ssh has no display to notify on.
[ -n "${DISPLAY:-}" ] || exit 0

# Stack tag per window, so a repeat alert for the same window replaces its
# predecessor instead of piling up.
@dunstify@ -a tmux -u normal \
  -h "string:x-dunst-stack-tag:tmux-silence-$window_id" \
  "󰤄 tmux: silence" "$session:$index <b>$name</b> went quiet"
