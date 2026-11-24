# Reverse tunnel from this host's sshd to a serveo private alias, so Termux on
# the phone can `ssh -J serveo.net` in without an open port. serveo only
# forwards bytes; end-to-end security rests on the phone pinning this host's
# key. See ~/Stuff/2026-10/19-p1/phone-shell-e2e-tunnels.md.
#
# SERVEO_KNOWN_HOSTS is set by the Nix wrapper.

readonly session=phone-tunnel
readonly port=4241
readonly default_limit=10m
readonly state="${XDG_STATE_HOME:-$HOME/.local/state}/phone-tunnel"
readonly alias_file="$state/alias"

usage() {
  cat <<EOF
Usage: phone-tunnel start [LIMIT]   open the tunnel; closes itself after LIMIT (default $default_limit, timeout(1) syntax)
       phone-tunnel stop            close it now
       phone-tunnel status
       phone-tunnel wait            block until it is closed, then print why
       phone-tunnel alias           print the alias the phone's ssh config needs
       phone-tunnel termux-config   print a block to paste into Termux: pinned host keys + ssh config
EOF
}

notify() {
  notifySendTelegram "$1" >/dev/null 2>&1 || echo "phone-tunnel: telegram notify failed" >&2
}

# Fixed, so the phone's ssh config never changes. serveo sees it, so it is no
# secret; it is random only so strangers can't reach sshd through it.
tunnel_alias() {
  if [[ ! -s $alias_file ]]; then
    mkdir -p "$state"
    chmod 700 "$state"
    (
      umask 077
      printf '%s-%s\n' "${HOSTNAME%%.*}" "$(head -c 12 /dev/urandom | base32 | tr -d = | tr '[:upper:]' '[:lower:]')" >"$alias_file"
    )
  fi
  cat "$alias_file"
}

termux_config() {
  local host=${HOSTNAME%%.*}
  host=${host#nixos-}
  cat <<EOF
mkdir -p ~/.ssh && chmod 700 ~/.ssh
cat >> ~/.ssh/known_hosts <<'KH'
$(cat "$SERVEO_KNOWN_HOSTS")
$host $(cut -d' ' -f1,2 /etc/ssh/ssh_host_ed25519_key.pub)
KH
cat >> ~/.ssh/config <<'CFG'
Host $host
  HostName $(tunnel_alias)
  Port $port
  User $USER
  ProxyJump serveo.net
  HostKeyAlias $host
  StrictHostKeyChecking yes
CFG
EOF
}

# The pane, not the session: a global remain-on-exit keeps dead panes around.
is_open() {
  [[ $(tmux list-panes -t "=$session" -F '#{pane_dead}' 2>/dev/null) == 0 ]]
}

# No gpg-agent flush: it would also drop passphrases unlocked at the desk.
# A phone unlock expires with the agent's cache TTLs.
cleanup() {
  notify "phone-tunnel closed ($1)"
}

# sshd-session, not -u sshd: the unit's journal lacks the Disconnected line.
session_log() {
  journalctl SYSLOG_IDENTIFIER=sshd-session --since "@$1" -o cat
}

# The phone's shortcut exits ssh after its unlock or sudo, so the first session
# through the tunnel ending is the signal to close. Any ::1 login counts: the
# phone's key is restricted to loopback, so in practice that is the phone.
watch_session() {
  local since=$1 ssh_pid=$2 peer=
  until [[ $peer ]]; do
    sleep 3
    peer=$(session_log "$since" | grep -m1 -oP 'Accepted publickey for \S+ from ::1 port \K\d+' || true)
  done
  # A session can also end as "Connection closed by ::1 port N" (5 of 32 on p1
  # since 2026-10-15) with no Disconnected line.
  until session_log "$since" | grep -qP "(Disconnected from user \\S+|Connection closed by) ::1 port $peer\\b"; do
    sleep 3
  done
  echo "phone session ended" >"$state/reason"
  kill "$ssh_pid"
}

run() {
  local limit=$1 rc=0 pid watcher
  rm -f "$state/reason"
  # --foreground: otherwise timeout moves ssh out of the pane's foreground
  # process group, and ssh stops (SIGTTIN) on its first read of the tty.
  # </dev/tty: a background job's stdin is /dev/null without job control.
  # Backgrounded so the watcher can kill it by pid.
  timeout --foreground -k 5s "$limit" ssh -tt \
    -o UserKnownHostsFile="$SERVEO_KNOWN_HOSTS" -o StrictHostKeyChecking=yes \
    -o PubkeyAuthentication=no \
    -o ServerAliveInterval=30 -o ServerAliveCountMax=3 \
    -o ExitOnForwardFailure=yes \
    -R "$(tunnel_alias):$port:localhost:$port" serveo.net \
    </dev/tty > >(tee "$state/log") 2>&1 &
  pid=$!
  watch_session "$(date +%s)" "$pid" &
  watcher=$!
  wait "$pid" || rc=$?
  kill "$watcher" 2>/dev/null || true
  if [[ ! -s $state/reason ]]; then
    if ((rc == 124)); then echo "limit $limit reached"; else echo "ssh exited $rc"; fi >"$state/reason"
  fi
  cleanup "$(<"$state/reason")"
}

start() {
  local limit=${1:-$default_limit}
  # timeout(1) treats a zero duration as no limit at all.
  if [[ $limit =~ ^0*\.?0*[smhd]?$ ]]; then
    echo "phone-tunnel: LIMIT must be greater than 0" >&2
    return 2
  fi
  if is_open; then
    status
    return 0
  fi
  local a log="$state/log"
  a=$(tunnel_alias)
  : >"$log"
  printf '%s %s\n' "$(date +%s)" "$limit" >"$state/opened"
  tmux kill-session -t "=$session" 2>/dev/null || true
  tmux new-session -d -s "$session" "$(printf '%q ' "$0" _run "$limit")" \; \
    set-option -w -t "=$session:" remain-on-exit off
  for _ in $(seq 30); do
    if grep -q 'Forwarding TCP traffic' "$log"; then
      echo "open: $a:$port, closes after $limit"
      notify "phone-tunnel open on ${HOSTNAME%%.*} (limit $limit)"
      return 0
    fi
    is_open || break
    sleep 1
  done
  echo "phone-tunnel: no forwarding confirmation from serveo; log:" >&2
  # Seen once: serveo kept refusing an alias after a stuck connection.
  echo "(if serveo refuses the alias, delete $alias_file and update the phone)" >&2
  cat "$log" >&2
  stop
  return 1
}

stop() {
  if is_open; then
    echo "stopped" >"$state/reason"
    tmux kill-session -t "=$session"
    cleanup "stopped"
    echo "closed"
  else
    tmux kill-session -t "=$session" 2>/dev/null || true
    echo "not open"
  fi
}

status() {
  if ! is_open; then
    echo "closed"
    return 0
  fi
  local opened limit
  read -r opened limit <"$state/opened"
  echo "open: $(tunnel_alias):$port since $(date -d "@$opened" +%H:%M), limit $limit"
}

wait_closed() {
  while is_open; do sleep 2; done
  echo "closed: $(cat "$state/reason" 2>/dev/null || echo unknown)"
}

case "${1:-}" in
start) start "${2:-}" ;;
stop) stop ;;
status) status ;;
wait) wait_closed ;;
alias) tunnel_alias ;;
termux-config) termux_config ;;
_run) run "$2" ;;
*)
  usage
  exit 2
  ;;
esac
