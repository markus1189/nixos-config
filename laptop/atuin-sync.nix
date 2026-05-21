{ config, pkgs, ... }:

let
  sshTarget = "nuc";
  tunnelPort = 49888;

  syncScript = pkgs.writeShellApplication {
    name = "atuin-sync-tunneled";
    # coreutils = sleep, gnugrep = grep; writeShellApplication does not
    # bundle them, and the systemd user unit has a lean PATH.
    runtimeInputs = with pkgs; [ openssh iproute2 atuin coreutils gnugrep ];
    text = ''
      readonly TUNNEL_PORT=${toString tunnelPort}
      readonly SSH_TARGET=${sshTarget}

      log() { echo "atuin-sync: $*" >&2; }

      # BatchMode prevents passphrase prompts (would hang the unit).
      # ExitOnForwardFailure makes ssh die if the local port is taken.
      # ConnectTimeout keeps the "not home" path fast.
      ssh -NL "$TUNNEL_PORT:127.0.0.1:$TUNNEL_PORT" \
          -o BatchMode=yes \
          -o ConnectTimeout=5 \
          -o ServerAliveInterval=5 \
          -o ServerAliveCountMax=2 \
          -o ExitOnForwardFailure=yes \
          "$SSH_TARGET" &
      ssh_pid=$!

      cleanup() {
        kill "$ssh_pid" 2>/dev/null || true
        wait "$ssh_pid" 2>/dev/null || true
      }
      trap cleanup EXIT

      # Wait up to ~5s for the forward to come up. If ssh dies first
      # (unreachable, auth unavailable, port collision), bail quietly.
      for ((i=0; i<25; i++)); do
        if ! kill -0 "$ssh_pid" 2>/dev/null; then
          log "tunnel failed to establish (nuc unreachable or auth unavailable) - skipping"
          exit 0
        fi
        if ss -tln | grep -q ":$TUNNEL_PORT "; then
          break
        fi
        sleep 0.2
      done

      if ! atuin sync; then
        log "atuin sync failed despite established tunnel - investigate"
        exit 1
      fi

      log "sync ok"
    '';
  };
in
{
  systemd.user.services.atuin-sync = {
    Unit.Description = "Sync atuin history via SSH tunnel to nuc";
    Service = {
      Type = "oneshot";
      ExecStart = "${syncScript}/bin/atuin-sync-tunneled";
      # NixOS programs.ssh.startAgent = true puts the agent socket at
      # $XDG_RUNTIME_DIR/ssh-agent, which %t resolves to in a user unit.
      Environment = [ "SSH_AUTH_SOCK=%t/ssh-agent" ];
    };
  };

  systemd.user.timers.atuin-sync = {
    Unit.Description = "Periodic SSH-tunneled atuin sync";
    Timer = {
      OnBootSec = "2m";
      OnUnitActiveSec = "5min";
      Persistent = true;
    };
    Install.WantedBy = [ "timers.target" ];
  };
}
