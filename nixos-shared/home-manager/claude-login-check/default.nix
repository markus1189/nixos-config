# Telegram warning before the Claude Code login (refresh token) expires.
# Whether that expiry slides on token refresh is unmeasured; the journal
# line below records it.
{
  pkgs,
  lib,
  osConfig,
  ...
}:
let
  hours = n: n * 60 * 60;

  check = pkgs.writeShellApplication {
    name = "claude-login-check";
    runtimeInputs = [
      pkgs.jq
      pkgs.coreutils
      pkgs.notifySendTelegram
    ];
    text = ''
      host=${lib.removePrefix "nixos-" osConfig.networking.hostName}
      credentials=$HOME/.claude/.credentials.json
      warn_window=${toString (hours 72)}
      # One daily timer run lands in it, so "expired" is sent once, not on every
      # day of a vacation.
      expired_window=${toString (hours 24)}

      # Fails on a missing file or a renamed/retyped field.
      seconds_left() {
        jq -er '.claudeAiOauth.refreshTokenExpiresAt | numbers | (. / 1000 - now) | floor' "$credentials"
      }

      message_for() {
        local secs=$1
        if [[ $secs == unknown ]]; then
          echo "🔴 $host: Claude login check cannot read refreshTokenExpiresAt from $credentials"
        elif (( secs > 0 && secs <= warn_window )); then
          echo "🟠 $host: Claude login expires $(date -d "+$secs sec" '+%a %d.%m. %H:%M'), run /login"
        elif (( secs <= 0 && secs > -expired_window )); then
          echo "🔴 $host: Claude login expired, run /login"
        fi
      }

      secs=$(seconds_left) || secs=unknown
      # Journal history shows whether the expiry slides on token refresh.
      echo "seconds left: $secs"

      msg=$(message_for "$secs")
      if [[ -n $msg ]]; then
        notifySendTelegram "$msg"
      fi
      [[ $secs != unknown ]]
    '';
  };
in
{
  systemd.user.services.claude-login-check = {
    Unit.Description = "Warn via Telegram before the Claude Code login expires";
    Service = {
      Type = "oneshot";
      ExecStart = "${check}/bin/claude-login-check";
    };
  };
  systemd.user.timers.claude-login-check = {
    Timer = {
      OnCalendar = "*-*-* 09:00";
      Persistent = true;
    };
    Install.WantedBy = [ "timers.target" ];
  };
}
