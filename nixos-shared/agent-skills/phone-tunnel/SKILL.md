---
name: phone-tunnel
description: "Opens a temporary end-to-end encrypted SSH path from Termux on the user's phone to this host's sshd through a serveo private alias, for unlocking gpg/pass or running sudo while away from the desk, then closes it. Invoked only by the user via /phone-tunnel."
disable-model-invocation: true
---

# phone-tunnel

The `phone-tunnel` script (nixos-config `nixos-shared/packages/scripts/phone-tunnel.sh`) runs
`ssh -R <alias>:4241:localhost:4241 serveo.net` in tmux. serveo forwards bytes only; the inner
SSH runs from Termux straight to this host's sshd. Background and evidence:
`~/Stuff/2026-10/19-p1/phone-shell-e2e-tunnels.md`.

## Invariants

- **E2E holds only because the phone pins this host's key** (`StrictHostKeyChecking yes` in the
  Termux config). Never tell the user to accept a host-key prompt or work around a
  "REMOTE HOST IDENTIFICATION HAS CHANGED" warning: stop and investigate.
- **No SSH-terminating relays**: upterm (`server/sshstock.go:278/392` terminates and re-dials
  SSH), tmate (same design), sshx (server ships the JS holding the key). Tailscale is excluded
  by the user.
- Open it only for this request, never longer than asked; a longer limit needs the user's word.

## Session

1. `phone-tunnel start [LIMIT]` (default `2h`, timeout(1) syntax). Prints `open: <alias>:4241`
   and sends a Telegram notice. Tell the user it is open and until when.
2. The user runs `ssh <host>` in Termux. To confirm the login came through the tunnel:
   `journalctl -u sshd --since -10min | rg 'Accepted publickey .* from ::1'`.
3. For gpg, the user runs in that Termux shell:
   ```bash
   export GPG_TTY=$(tty); gpg-connect-agent updatestartuptty /bye
   echo test | gpg --clearsign >/dev/null
   ```
   Check the cache: `gpg-connect-agent 'keyinfo --list' /bye | awk '$7=="1"'`. Closing the
   tunnel keeps the cache (it expires with gpg-agent's TTLs), so signing works after `stop`.
4. `phone-tunnel stop` when done (it also closes itself at the limit). Both notify. Remind the
   user that pinentry now points at the closed Termux tty until they run
   `gpg-connect-agent updatestartuptty /bye` in a desktop terminal.

## Trouble

- `start` fails with no forwarding confirmation: it prints the log. `remote port forwarding
  failed` for the alias once followed a stuck connection and lasted 5+ min while other aliases
  worked → `rm ~/.local/state/phone-tunnel/alias`, `start` again, and give the user the new
  `HostName` (`phone-tunnel alias`).
- New phone or host: `phone-tunnel termux-config` prints the block to paste into Termux. Paste
  `ssh-keygen` separately (its prompt eats the next pasted line). The phone's public key goes
  into `nixos-shared/ssh.nix` with `from="127.0.0.1,::1"`, so it only works through a tunnel;
  needs a rebuild.
