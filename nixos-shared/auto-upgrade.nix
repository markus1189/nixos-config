# Unattended hosts (nuc, p1) rebuild nightly from the latest commit on
# GitHub (repo is public): commit + push on p1g8 is the whole pipeline, no
# manual pull on the host. The module passes --refresh itself in flake mode
# (no stale tarball cache); -L puts build logs in the journal. No nightly lock
# updates as root on purpose (builds the committed flake.lock).
{
  config,
  lib,
  pkgs,
  ...
}:

let
  # Flake attr and message label; p1's hostname is nixos-p1.
  host = lib.removePrefix "nixos-" config.networking.hostName;
in
{
  system.autoUpgrade = {
    # Careful when re-installing a host: the timer is Persistent=true, and
    # a freshly installed root has no stamp in /var/lib/systemd/timers, so it
    # fires on the *first boot* rather than at `dates` -- turn this off while
    # master still describes the old disk layout.
    enable = true;
    dates = "04:21";
    flake = "github:markus1189/nixos-config#${host}";
    flags = [ "-L" ];
  };

  # Before the upgrade, so the night's new generation survives it.
  nix.gc = {
    automatic = true;
    dates = "03:15";
    options = "--delete-older-than 30d";
  };

  systemd.services = {
    # A failed nightly upgrade is otherwise silent, a hung one doubly so:
    # oneshot defaults to no timeout, so it blocks the timer without ever
    # failing. TimeoutStartSec, not RuntimeMaxSec (no effect on oneshot).
    nixos-upgrade = {
      onFailure = [ "notify-upgrade-failure.service" ];
      onSuccess = [ "notify-upgrade-success.service" ];
      serviceConfig = {
        TimeoutStartSec = "4h";
        # Stash the pre-switch system: by the time onSuccess runs,
        # /run/current-system is already the new one. Diffing the last two
        # profile generations instead would report a stale change on a no-op
        # night, which does not create a generation at all.
        ExecStartPre = "${pkgs.writeShellScript "stash-pre-upgrade-system" ''
          ${pkgs.coreutils}/bin/readlink -f /run/current-system \
            > /run/nixos-upgrade-previous-system
        ''}";
      };
    };
    notify-upgrade-failure = {
      description = "telegram notification about failed nixos-upgrade";
      serviceConfig = {
        Type = "oneshot";
        User = config.my.userName;
        Group = "users";
        ExecStart = "${pkgs.notifySendTelegram}/bin/notifySendTelegram '${host}: nightly nixos-upgrade failed'";
      };
    };
    # A silent success is the dangerous one: the repo is public and the nightly
    # rebuild activates whatever master says, so the closure diff is the only
    # place an unexpected change surfaces before it has been running for a day.
    notify-upgrade-success = {
      description = "telegram notification about what the nightly nixos-upgrade changed";
      serviceConfig = {
        Type = "oneshot";
        User = config.my.userName;
        Group = "users";
      };
      script = ''
        previous=$(cat /run/nixos-upgrade-previous-system 2>/dev/null || true)
        current=$(readlink -f /run/current-system)

        # Silence on a no-op night is deliberate: a daily "nothing changed"
        # ping is noise you learn to swipe away, and that habit is precisely
        # what this notification exists to prevent.
        if [ -z "$previous" ] || [ "$previous" = "$current" ]; then
          exit 0
        fi

        # diff-closures colours unconditionally -- neither NO_COLOR nor
        # TERM=dumb suppresses it -- and Telegram renders the escapes raw.
        # head -c keeps us under Telegram's 4096-character message limit.
        changes=$(${config.nix.package}/bin/nix store diff-closures "$previous" "$current" \
          | sed 's/\x1b\[[0-9;]*m//g' \
          | head -c 3000)

        rev=$(/run/current-system/sw/bin/nixos-version --configuration-revision)

        ${pkgs.notifySendTelegram}/bin/notifySendTelegram \
          "$(printf '${host}: nixos-upgrade switched to %s\n\n%s' "$rev" "$changes")"
      '';
    };
  };
}
