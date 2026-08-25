{
  bat,
  buku,
  cacert,
  coreutils,
  curl,
  dbus,
  fzf,
  dragon-drop,
  dunst,
  emacs,
  feh,
  ffmpeg,
  findutils,
  firefox,
  gawk,
  git,
  gnugrep,
  gnuplot,
  gnused,
  haskellPackages,
  imagemagick,
  jo,
  jq,
  less,
  lib,
  libnotify,
  mozillavpn,
  nixos-artwork,
  oathToolkit,
  pass,
  playerctl,
  procps,
  psmisc,
  pulseaudioFull,
  python3,
  python3Packages,
  rofi,
  rsstail,
  scrot,
  sqlite,
  systemd,
  tmux,
  unixtools,
  wmctrl,
  wpa_supplicant,
  xclip,
  xdg-utils,
  xdotool,
  xrandr,
  xsel,
  zbar,
  zsh,
  writeShellApplication,
  writers,
  flameshot,
  tesseract,
  gxmessage,
  bluez,

}:

rec {
  tmx = writeShellApplication {
    name = "tmx";
    runtimeInputs = [
      tmux
      zsh
    ];
    inheritPath = true;
    bashOptions = [ "errexit" ];
    text = ''
      set -e

      function main() {
          case "$1" in
              "default" | "sp_upper" | "sp_lower" | "sp_right")
                  attach_or_create "$1" "zsh"
                  ;;
              "im")
                  attach_or_create "im" 'ssh -t mc "tmux attach"'
                  ;;
              *)
                  attach_if_exists "$1" "zsh"
          esac
      }

      function attach_or_create() {
        if ! tmux has-session -t "$1" &>/dev/null ; then
          tmux new-session -s "$1" -d "$2"
        fi
        exec tmux -2 attach -t "$1"
      }

      function attach_if_exists() {
        if tmux has-session -t "$1" &>/dev/null ; then
            exec tmux -2 attach -t "$1"
        else
            echo "Session '$1' not found, refusing to create." >&2
        fi
      }

      main "$@"
    '';
  };

  tmuxPollPane = writeShellApplication {
    name = "tmux-poll-pane";
    runtimeInputs = [
      tmux
      coreutils
      gnugrep
      gnused
    ];
    inheritPath = false;
    bashOptions = [ "errexit" ];
    text = ''
      # tmux-poll-pane - Poll a tmux pane for patterns with timeout
      # A sophisticated busy-wait loop for the discerning tmux user

      readonly SCRIPT_NAME="tmux-poll-pane"
      readonly VERSION="1.0.0"

      # Default values
      INTERVAL=2
      TIMEOUT=60
      SUCCESS_PATTERN=""
      FAILURE_PATTERN=""
      INVERSE_PATTERN=""
      USE_SCROLLBACK=false
      QUIET=false
      PANE=""

      usage() {
          cat >&2 <<EOF
      Usage: $SCRIPT_NAME [OPTIONS] <pane-id>

      Poll a tmux pane for patterns with configurable success/failure conditions.

      ARGUMENTS:
          <pane-id>           Tmux pane identifier (e.g., 0, 135, or {marked})
                              Numeric IDs are automatically prefixed with %

      OPTIONS:
          -s, --success PATTERN    Exit successfully (0) when this pattern is found
          -f, --failure PATTERN    Exit with error (1) when this pattern is found
          -i, --inverse PATTERN    Exit successfully when pattern is NOT found
          -I, --interval SECONDS   Polling interval in seconds (default: 2)
          -t, --timeout SECONDS    Maximum wait time in seconds (default: 60)
          -S, --scrollback         Capture entire scrollback history (default: visible only)
          -q, --quiet              Suppress progress messages, only show result
          -h, --help               Show this help message
          -v, --version            Show version information

      PATTERN SYNTAX:
          Patterns use grep extended regex (-E). Multiple patterns can be separated
          with '|' for OR matching (e.g., "success|complete|done").

      EXIT CODES:
          0   Success pattern found (or inverse pattern not found)
          1   Failure pattern found
          2   Timeout reached
          3   Invalid arguments or tmux error

      EXAMPLES:
          # Wait for build completion
          $SCRIPT_NAME -s "Build succeeded" -f "Build failed|ERROR" -t 300 2

          # Wait for server startup
          $SCRIPT_NAME -s "listening on port" -t 30 3

          # Wait for "Compiling..." to disappear
          $SCRIPT_NAME -i "Compiling" -t 120 1

          # Monitor with 5-second interval and full scrollback
          $SCRIPT_NAME -s "DONE" -S -I 5 -t 600 4

          # Quiet mode for scripting
          $SCRIPT_NAME -q -s "complete" -t 60 2 && echo "Success!" || echo "Failed"

      NOTES:
          - At least one of -s, -f, or -i must be specified
          - If both success and failure patterns are given, failure takes precedence
          - Use -S with caution on panes with large scrollback buffers
          - Patterns are case-sensitive unless you use '(?i)' prefix
      EOF
          exit "''${1:-0}"
      }

      version() {
          echo "$SCRIPT_NAME version $VERSION"
          exit 0
      }

      log() {
          if ! $QUIET; then
              echo "$@" >&2
          fi
      }

      error() {
          echo "Error: $*" >&2
          exit 3
      }

      # Parse arguments
      while [[ $# -gt 0 ]]; do
          case "$1" in
              -s|--success)
                  SUCCESS_PATTERN="$2"
                  shift 2
                  ;;
              -f|--failure)
                  FAILURE_PATTERN="$2"
                  shift 2
                  ;;
              -i|--inverse)
                  INVERSE_PATTERN="$2"
                  shift 2
                  ;;
              -I|--interval)
                  INTERVAL="$2"
                  shift 2
                  ;;
              -t|--timeout)
                  TIMEOUT="$2"
                  shift 2
                  ;;
              -S|--scrollback)
                  USE_SCROLLBACK=true
                  shift
                  ;;
              -q|--quiet)
                  QUIET=true
                  shift
                  ;;
              -h|--help)
                  usage 0
                  ;;
              -v|--version)
                  version
                  ;;
              -*)
                  error "Unknown option: $1 (use -h for help)"
                  ;;
              *)
                  if [[ -z "$PANE" ]]; then
                      PANE="$1"
                      shift
                  else
                      error "Multiple pane IDs specified: $PANE and $1"
                  fi
                  ;;
          esac
      done

      # Validate arguments
      [[ -z "$PANE" ]] && error "Pane ID required (use -h for help)"
      [[ -z "$SUCCESS_PATTERN" && -z "$FAILURE_PATTERN" && -z "$INVERSE_PATTERN" ]] && \
          error "At least one pattern (-s, -f, or -i) must be specified"
      [[ ! "$INTERVAL" =~ ^[0-9]+$ ]] && error "Interval must be a positive integer"
      [[ ! "$TIMEOUT" =~ ^[0-9]+$ ]] && error "Timeout must be a positive integer"

      # Prefix numeric pane IDs with %
      if [[ "$PANE" =~ ^[0-9]+$ ]]; then
          PANE="%$PANE"
      fi

      # Verify pane exists
      if ! tmux list-panes -a -F "#{pane_id}" | grep -qxF "$PANE"; then
          error "Pane '$PANE' not found"
      fi

      # Build capture command
      CAPTURE_CMD=(tmux capture-pane -p -t "$PANE")
      if $USE_SCROLLBACK; then
          CAPTURE_CMD+=(-S - -E -)
      fi

      # Start polling
      START_TIME=$(date +%s)
      readonly START_TIME
      ITERATION=0

      log "Polling pane $PANE (interval: ''${INTERVAL}s, timeout: ''${TIMEOUT}s)"
      [[ -n "$SUCCESS_PATTERN" ]] && log "  Success pattern: $SUCCESS_PATTERN"
      [[ -n "$FAILURE_PATTERN" ]] && log "  Failure pattern: $FAILURE_PATTERN"
      [[ -n "$INVERSE_PATTERN" ]] && log "  Inverse pattern: $INVERSE_PATTERN"

      while true; do
          ITERATION=$((ITERATION + 1))
          ELAPSED=$(($(date +%s) - START_TIME))

          # Capture pane content
          if ! CONTENT=$("''${CAPTURE_CMD[@]}" 2>&1); then
              error "Failed to capture pane $PANE: $CONTENT"
          fi

          # Check failure pattern first (highest priority)
          if [[ -n "$FAILURE_PATTERN" ]] && echo "$CONTENT" | grep -qE "$FAILURE_PATTERN"; then
              MATCH=$(echo "$CONTENT" | grep -E "$FAILURE_PATTERN" | head -1 | sed 's/^[[:space:]]*//;s/[[:space:]]*$//')
              log "✗ Failure pattern found after ''${ELAPSED}s: $MATCH"
              exit 1
          fi

          # Check success pattern
          if [[ -n "$SUCCESS_PATTERN" ]] && echo "$CONTENT" | grep -qE "$SUCCESS_PATTERN"; then
              MATCH=$(echo "$CONTENT" | grep -E "$SUCCESS_PATTERN" | head -1 | sed 's/^[[:space:]]*//;s/[[:space:]]*$//')
              log "✓ Success pattern found after ''${ELAPSED}s: $MATCH"
              exit 0
          fi

          # Check inverse pattern (success when NOT found)
          if [[ -n "$INVERSE_PATTERN" ]] && ! echo "$CONTENT" | grep -qE "$INVERSE_PATTERN"; then
              log "✓ Inverse pattern not found after ''${ELAPSED}s (success)"
              exit 0
          fi

          # Check timeout
          if [[ "$ELAPSED" -ge "$TIMEOUT" ]]; then
              log "⏱ Timeout reached after ''${ELAPSED}s (''${ITERATION} iterations)"
              if [[ -n "$SUCCESS_PATTERN" ]]; then
                  log "  Pattern not found: $SUCCESS_PATTERN"
              fi
              if [[ -n "$INVERSE_PATTERN" ]]; then
                  log "  Pattern still present: $INVERSE_PATTERN"
              fi
              exit 2
          fi

          # Progress indicator (every 10 iterations when not quiet)
          if ! $QUIET && [[ $((ITERATION % 10)) -eq 0 ]]; then
              log "  Still polling... (''${ELAPSED}s elapsed, ''${ITERATION} iterations)"
          fi

          sleep "$INTERVAL"
      done
    '';
  };

  git-pretty-log = writeShellApplication {
    name = "git-pretty-log";
    runtimeInputs = [
      git
      gnused
      less
    ];
    inheritPath = false;
    bashOptions = [ "errexit" ];
    text = ''
      HASH="%C(yellow)%h%Creset"
      RELATIVE_TIME="%Cgreen(%ar)%Creset"
      AUTHOR="%C(bold blue)<%aN>%Creset"
      REFS="%C(red)%d%Creset"
      SUBJECT="%s"

      FORMAT="''${HASH} ''${RELATIVE_TIME} ''${AUTHOR} ''${REFS} ''${SUBJECT}"

      pretty_git_log() {
        git log --graph --color --pretty="tformat:''${FORMAT}" "$@" |
          sed -Ee 's/(^[^<]*) ago\)/\1)/' |
          sed -Ee 's/(^[^<]*), [[:digit:]]+ .*months?\)/\1)/' |
          less -FXRS
      }

      pretty_git_log "$@"
    '';
  };

  isVpnActive = writeShellApplication {
    name = "isVpnActive";
    runtimeInputs = [
      systemd
      procps
      gnugrep
    ];
    inheritPath = false;
    bashOptions = [ ];
    text = ''

      OPENVPN="$(systemctl is-active 'openvpn-*.service' | grep -q active && echo OVP)"
      WIREGUARD="$(systemctl is-active 'wg-quick-*.service' | grep -q active && echo WGD)"
      MOZILLA="" # broken

      COLOR=$(if [[ -n "$OPENVPN" || -n "$WIREGUARD" || -n "$MOZILLA" ]]; then echo lightgreen; else echo red; fi)
      ICON=$(if [[ -n "$OPENVPN" || -n "$WIREGUARD" || -n "$MOZILLA" ]]; then echo ' '; else echo ''; fi)
      LABEL="''${OPENVPN}''${WIREGUARD}''${MOZILLA}"
      echo "<fc=$COLOR>''${LABEL}''${ICON}</fc>"
    '';
  };

  dunstStatus = writeShellApplication {
    name = "dunstStatus";
    runtimeInputs = [
      dunst
      dbus
    ];
    inheritPath = false;
    bashOptions = [ "errexit" ];
    text = ''

      if [[ "$(dunstctl is-paused)" == "true" ]]; then
        echo "<fc=red>PAUSED</fc> "
      fi
    '';
  };

  xmobarSharingIndicator = writeShellApplication {
    name = "xmobarSharingIndicator";
    runtimeInputs = [ xdotool ];
    inheritPath = false;
    bashOptions = [ "errexit" ];
    text = ''
      if xdotool search 'is sharing' &> /dev/null || xdotool search 'as_toolbar' &> /dev/null; then
        echo "<fc=red>⏺SHARING⏺</fc> "
      fi
    '';
  };

  btHeadphoneBattery = writeShellApplication {
    name = "btHeadphoneBattery";
    runtimeInputs = [
      systemd
    ];
    inheritPath = false;
    bashOptions = [ ];
    text = ''
      readonly HEADPHONE_MAC="04:52:C7:34:62:44"
      readonly DEV="/org/bluez/hci0/dev_''${HEADPHONE_MAC//:/_}"

      connected=$(busctl get-property org.bluez "$DEV" org.bluez.Device1 Connected 2>/dev/null)
      [[ "$connected" == *true* ]] || exit 0

      battery=$(busctl get-property org.bluez "$DEV" org.bluez.Battery1 Percentage 2>/dev/null)
      battery_percent=''${battery##* }
      [[ "$battery_percent" =~ ^[0-9]+$ ]] || exit 0

      # Color code based on battery level
      if [[ $battery_percent -gt 70 ]]; then
          color="<fc=lightgreen>"
      elif [[ $battery_percent -ge 30 ]]; then
          color="<fc=orange>"
      else
          color="<fc=red>"
      fi

      echo "''${color} 󰂯 ''${battery_percent}%</fc>"
    '';
  };

  chargeRate = writeShellApplication {
    name = "chargeRate";
    runtimeInputs = [ ];
    inheritPath = false;
    bashOptions = [ ];
    text = ''
      shopt -s nullglob

      # Only meaningful on external power; the %battery% widget covers
      # the on-battery case.
      plugged=
      for u in /sys/class/power_supply/ucsi-source-psy-*; do
        if read -r online 2>/dev/null < "$u/online" && [[ "$online" == 1 ]]; then
          plugged=1
          break
        fi
      done
      [[ -n "$plugged" ]] || exit 0

      read -r e2 2>/dev/null < /sys/class/power_supply/BAT0/energy_now
      [[ "$e2" =~ ^[0-9]+$ ]] || exit 0
      printf -v t2 '%(%s)T' -1

      # Net rate from the energy_now delta between runs. status/power_now
      # lie in the weak-charger "Not charging" limbo (report 0 / not-charging
      # while the battery actually drains); the energy_now delta does not.
      state="''${XDG_RUNTIME_DIR:-/tmp}/xmobar-chargeRate"
      read -r e1 t1 2>/dev/null < "$state" || true
      printf '%s %s\n' "$e2" "$t2" > "$state"

      [[ "$e1" =~ ^[0-9]+$ && "$t1" =~ ^[0-9]+$ ]] || exit 0
      dt=$(( t2 - t1 ))
      (( dt >= 3 && dt <= 120 )) || exit 0

      # Integer math (truncates); the 2W deadband below covers the ~+-2W
      # energy_now quantization noise, so sub-watt rounding is irrelevant.
      net=$(( (e2 - e1) * 3600 / (1000000 * dt) ))

      if   (( net >=  2 )); then echo "<fc=lightgreen>󱐋 +''${net}W</fc> "
      elif (( net <= -2 )); then echo "<fc=red>󱐋 ''${net}W</fc> "
      fi
    '';
  };

  ts = writeShellApplication {
    name = "ts";
    runtimeInputs = [ coreutils ];
    inheritPath = false;
    bashOptions = [ "errexit" ];
    text = ''
      FORMAT="''${1:-"%H:%M:%S.%3N"}"
      while IFS= read -r line; do
        printf '%s: %s\n' "$(date "+$FORMAT")" "$line"
      done
    '';
  };

  gnuplot-quick = writeShellApplication {
    name = "gnuplot-quick";
    runtimeInputs = [ gnuplot ];
    inheritPath = false;
    bashOptions = [ "errexit" ];
    text = ''
      FILE=''${1}

      if [ -z "$1" ]; then
          FILE="-"
      fi

      gnuplot -persist -e "set style line 1 lc rgb '#0060ad' lt 1 lw 2 pt 7 ps 1.5; set autoscale; set grid; plot '$FILE' with linespoints ls 1"
    '';
  };

  xmonadReset = writeShellApplication {
    name = "xmonadReset";
    runtimeInputs = [
      haskellPackages.xmonad
      psmisc
    ];
    inheritPath = false;
    bashOptions = [ "errexit" ];
    text = ''
      killall xmobar
      # Kill accumulated scratchpad ghostty processes from previous XMonad sessions.
      # Without this, each xmonad --restart spawns fresh scratchpads while old ones
      # keep running, accumulating hundreds of processes and gigabytes of RAM over time.
      pkill -f 'ghostty.*--title=sp_' || true
      xmonad --restart
    '';
  };

  centerMouse = writeShellApplication {
    name = "centerMouse";
    runtimeInputs = [ xdotool ];
    inheritPath = false;
    bashOptions = [ "errexit" ];
    text = ''
      xdotool mousemove --window "$(xdotool getwindowfocus)" --polar 0 0
    '';
  };

  lockScreen = writeShellApplication {
    name = "lockScreen";
    runtimeInputs = [ ];
    inheritPath = false;
    bashOptions = [ "errexit" ];
    text = ''
      # i3lock binary comes from programs.i3lock in laptop/programs.nix
      # (setuid wrapper, required for PAM auth; package is i3lock-color)
      /run/wrappers/bin/i3lock \
        --image=${nixos-artwork.wallpapers.simple-dark-gray}/share/artwork/gnome/nix-wallpaper-simple-dark-gray.png \
        --blur=5 \
        --clock \
        --indicator \
        --time-str="%H:%M" \
        --date-str="%A, %Y-%m-%d" \
        --pointer=win \
        --ignore-empty-password \
        --show-failed-attempts \
        --ring-color=ffffffff \
        --ringver-color=00ff00ff \
        --ringwrong-color=ff0000ff \
        --keyhl-color=88ccffff \
        --bshl-color=ff8888ff \
        --time-color=ffffffff \
        --date-color=ffffffff \
        --verif-color=ffffffff \
        --wrong-color=ff8888ff
    '';
  };

  currentSpotifySong = writeShellApplication {
    name = "currentSpotifySong";
    runtimeInputs = [ playerctl ];
    inheritPath = false;
    bashOptions = [ "errexit" ];
    text = ''
      getTag() {
        playerctl -p spotify metadata "xesam:''${1}" || true
      }

      STATUS="$(playerctl -p spotify status || true)"
      TITLE="$(getTag title)"
      ARTIST="$(getTag artist)"
      ALBUM="$(getTag album)"

      if [[ "''${STATUS}" == "Playing" ]]; then
        ALBUM2="$(if [[ "$TITLE" == "$ALBUM" ]]; then echo ; else echo "($ALBUM)"; fi)"
        echo -n " <fc=orange>''${TITLE}</fc> by <fc=orange>''${ARTIST}</fc> ''${ALBUM2}"
      fi
    '';
  };

  emacsAnywhere = writeShellApplication {
    name = "emacsAnywhere";
    runtimeInputs = [
      xdotool
      libnotify
      emacs
      coreutils
    ];
    inheritPath = false;
    bashOptions = [ ];
    text = ''

      function waitForClose {
        until ! xdotool search --name 'Emacs Anywhere'; do
            :
        done
      }
      read -r -d ''' ELISP <<'EOF'
      (defun ea-on-delete (frame)
        (clipboard-kill-ring-save
         (point-min)
         (point-max))
        (sit-for 0.3)
        (kill-buffer "*Emacs Anywhere*"))

      (defun ea-hook ()
        (add-hook 'delete-frame-functions 'ea-on-delete))

      (ea-hook)
      (switch-to-buffer "*Emacs Anywhere*")
      (select-frame-set-input-focus (selected-frame))
      (call-interactively 'find-temp-file)
      EOF

      emacsclient -a "" -c -e "(progn $ELISP)"

      sleep 0.5

      timeout 2s waitForClose

      notify-send -u low "Emacs Anywhere" "Copied to clipboard"
    '';
  };

  rofiDefaults = writeShellApplication {
    name = "rofiDefaults";
    runtimeInputs = [ rofi ];
    inheritPath = true;
    bashOptions = [ "errexit" ];
    text = ''
      rofi -i -monitor -4 -disable-history "$@"
    '';
  };

  notifySendPb = writeShellApplication {
    name = "notifySendPb";
    runtimeInputs = [
      curl
      jo
    ];
    inheritPath = false;
    bashOptions = [ "errexit" ];
    text = ''
      curl --silent --fail \
       --cacert ${cacert}/etc/ssl/certs/ca-bundle.crt \
       --header "Access-Token: $(< /run/agenix/pushbullet)" \
       --header 'Content-Type: application/json' \
       --data-binary "$(jo -- -s type=note -s title="''${1:-no-title}" -s body="''${2:-no-body}")" \
       --request POST \
       https://api.pushbullet.com/v2/pushes > /dev/null
    '';
  };

  sendTelegram =
    chatid: name: parseMode:
    writeShellApplication {
      inherit name;
      runtimeInputs = [
        curl
        jo
        cacert
      ];
      inheritPath = false;
      bashOptions = [ "errexit" ];
      text = ''
        set -a
        # shellcheck source=/dev/null
        . /run/agenix/telegram.env
        set +a

        MESSAGE=''${1:?"Error: no message given!"}
        curl --silent --fail -XPOST \
         --retry-all-errors --retry 3 \
         --cacert ${cacert}/etc/ssl/certs/ca-bundle.crt \
          -H 'Content-Type: application/json' \
          -d "$(jo chat_id=${chatid} ${
            lib.optionalString (parseMode != null) "parse_mode=${parseMode}"
          } text="''${MESSAGE}")" \
          --url "https://api.telegram.org/bot''${TELEGRAM_BOT_TOKEN}/sendMessage"
      '';
    };

  sendTelegramPoll = writeShellApplication {
    name = "sendTelegramPoll";
    runtimeInputs = [
      curl
      jo
      cacert
    ];
    inheritPath = false;
    bashOptions = [ "errexit" ];
    text = ''
      set -a
      # shellcheck source=/dev/null
      . /run/agenix/telegram.env
      set +a

      QUESTION=''${1:?"Error: no message given!"}
      shift
      curl --silent --fail -XPOST \
       --retry-all-errors --retry 3 \
       --cacert ${cacert}/etc/ssl/certs/ca-bundle.crt \
        -H 'Content-Type: application/json' \
        -d "$(jo allows_multiple_answers=true chat_id=299952716 question="''${QUESTION}" options="$(jo -a "$@")")" \
        --url "https://api.telegram.org/bot''${TELEGRAM_BOT_TOKEN}/sendPoll"
    '';
  };

  notifySendTelegram = sendTelegram "299952716" "notifySendTelegram" null;

  notifySendTelegramHtml = sendTelegram "299952716" "notifySendTelegramHtml" "HTML";

  notifySendTelegramMd = sendTelegram "299952716" "notifySendTelegramMd" "MarkdownV2";

  # Internal: the "home" telegram group, used by viessmannOutsideTemperature.
  notifySendHome = sendTelegram "-1001328938887" "notifySendHome" null;

  telegramSendPhoto = writeShellApplication {
    name = "telegramSendPhoto";
    runtimeInputs = [
      curl
      jo
      coreutils
    ];
    inheritPath = false;
    bashOptions = [ "errexit" ];
    text = ''
      set -a
      # shellcheck source=/dev/null
      . /run/agenix/telegram.env
      set +a

      LIMIT=5

      buildArray() {
          # Word splitting is the point here: one compact JSON object per line.
          # shellcheck disable=SC2046
          jo -a $(
              for i in "$@"; do
                  NAME="$(basename "$i")"
                  jo type=photo media="attach://$NAME"
              done | shuf -n "$LIMIT"
          )
      }

      buildParams() {
          for i in "$@"; do
              echo "-F $(basename "$i")=@$i"
          done | shuf -n "$LIMIT"
      }

      if [[ "$#" -gt "$LIMIT" ]]; then
        echo "Warning: using only $LIMIT randomly chosen out of $# given args" > /dev/stderr
      fi

      if [[ "$#" -ge 1 ]]; then
          echo "Uploading" > /dev/stderr

          # buildParams emits one "-F name=@path" per line; splitting it into
          # separate curl arguments is intended.
          # shellcheck disable=SC2046
          curl --silent --fail -XPOST \
                  --cacert ${cacert}/etc/ssl/certs/ca-bundle.crt \
                  --url "https://api.telegram.org/bot''${TELEGRAM_BOT_TOKEN}/sendMediaGroup" \
                  -F chat_id=299952716 \
                  -F media="$(buildArray "$@")" \
                  $(buildParams "$@")
      else
          echo "USAGE: $0 FILE..." > /dev/stderr
      fi
    '';
  };

  telegramPhotosLastYear = writeShellApplication {
    name = "telegramPhotosLastYear";
    runtimeInputs = [
      findutils
      telegramSendPhoto
    ];
    inheritPath = false;
    bashOptions = [ "errexit" ];
    text = ''
      set -o pipefail
      find "''${1:?No path to photos directory given}" -name "*$(date -d '-1 year' +%Y%m%d)*" | head -1 | xargs telegramSendPhoto
    '';
  };

  bukuRun = writeShellApplication {
    name = "bukuRun";
    runtimeInputs = [
      buku
      gnused
      rofi
      coreutils
      findutils
      firefox
    ];
    inheritPath = false;
    bashOptions = [ "errexit" ];
    text = "buku --nostdin -p -f 5 | sed 's/\\t/ /g' | rofi -i -matching fuzzy -dmenu | cut -d ' ' -f 1 | xargs --no-run-if-empty buku -o";
  };

  logArgs = writeShellApplication {
    name = "log-args";
    runtimeInputs = [ systemd ];
    inheritPath = true;
    bashOptions = [ "errexit" ];
    text = ''
      systemd-cat -tlog-args -- bash -c 'echo $@'
    '';
  };

  addToRaindropScript = writeShellApplication {
    name = "add-to-raindrop";
    runtimeInputs = [
      curl
      gnugrep
      jo
      coreutils
      gnused
    ];
    inheritPath = false;
    bashOptions = [ "errexit" ];
    text = ''
      if [[ "''${1:-}" == "--help" || "''${1:-}" == "-h" ]]; then
        echo "Usage: add-to-raindrop <URL> [TAGS]"
        echo ""
        echo "Bookmark a URL to Raindrop.io with automatic tag detection."
        echo ""
        echo "Arguments:"
        echo "  URL     The URL to bookmark (required)"
        echo "  TAGS    Comma-separated tags to add (optional)"
        echo ""
        echo "Auto-detected tags:"
        echo "  youtube.com/watch    → youtube,video"
        echo "  hr-fernsehen.de     → hr,video"
        echo "  fs.blog/raptitude   → deep"
        echo "  reddit.com          → reddit,r/<subreddit>"
        echo "  news.ycombinator    → hackernews"
        echo "  xkcd/monkeyuser     → comic"
        echo ""
        echo "The tag 'newsboat' is always added automatically."
        echo "Retries up to 6 times on failure."
        exit 0
      fi

      if [[ -z "''${1:-}" ]]; then
        echo "Error: URL is required. Use --help for usage." >&2
        exit 1
      fi

      URL="''${1}"
      GIVEN_TAGS="''${2:-}"
      TAGS="''${GIVEN_TAGS},newsboat"

      script_raindrop_access_token="$(< /run/agenix/raindrop)"

      if echo "''${URL}" | grep 'youtube.com/watch'; then
          TAGS="$TAGS,youtube,video"
      fi

      if echo "''${URL}" | grep 'hr-fernsehen.de/sendungen'; then
          TAGS="$TAGS,hr,video"
      fi

      if echo "''${URL}" | grep -e 'fs.blog/' -e 'raptitude.com'; then
          TAGS="$TAGS,deep"
      fi

      if echo "''${URL}" | grep 'reddit.com'; then
          TAGS="$TAGS,reddit"
          if echo "''${URL}" | grep -o 'r/[^/]*'; then
            TAGS="$TAGS,$(echo "''${URL}" | grep -o 'r/[^/]*')"
          fi
      fi

      if echo "''${URL}" | grep 'news.ycombinator.com'; then
          TAGS="$TAGS,hackernews"
      fi

      if echo "''${URL}" | grep -e 'xkcd.com' -e 'monkeyuser.com'; then
          TAGS="$TAGS,comic"
      fi

      # Convert comma-separated tags to JSON array
      TAG_ARRAY=$(echo "$TAGS" | tr ',' '\n' | sed 's/^/"/;s/$/"/' | tr '\n' ',' | sed 's/,$//')

      main() {
        unset c
        until curl --cacert "${cacert}/etc/ssl/certs/ca-bundle.crt" -s --fail -XPOST https://api.raindrop.io/rest/v1/raindrop -H "Authorization: Bearer ''${script_raindrop_access_token}" -H 'content-type: application/json' -d "$(jo link="''${1}" tags="[''${TAG_ARRAY}]" pleaseParse="{}")"; do
          ((c++)) && ((c==6)) && break
          sleep 1
        done
        unset c
        exit "$?"
      }

      main "$1"
    '';
  };

  mkRsstailToRaindropUnit =
    {
      key,
      url,
      intervalSeconds ? 300,
    }:
    let
      name = "rsstail-${key}-script";
      script = writeShellApplication {
        inherit name;
        runtimeInputs = [ ];
        inheritPath = false;
        bashOptions = [ "errexit" ];
        text = ''
          ${rsstail}/bin/rsstail -n0 -i ${toString intervalSeconds} -r -l -u '${url}' \
            | ${gnugrep}/bin/grep --line-buffered '^Link: ' \
            | ${gawk}/bin/awk '{print $2; system("")}' \
            | while read -r i; do
                echo "[rsstail-${key}]: Adding to raindrop: $i"
                ${addToRaindropScript}/bin/add-to-raindrop "$i" "rsstail"
              done
        '';
      };
    in
    {
      "rsstail-${key}" = {
        Unit = {
          Description = "rsstail for ${key}";
        };

        Service = {
          ExecStart = "${script}/bin/${name}";
          RestartSec = 10;
          Restart = "always";
          StartLimitInterval = 2 * 60;
        };

        Install = {
          WantedBy = [ "default.target" ];
        };
      };
    };
  viessmannOutsideTemperature = writeShellApplication {
    name = "viessmannOutsideTemperature";
    runtimeInputs = [
      curl
      cacert
      jq
      notifySendHome
      coreutils
    ];
    inheritPath = false;
    bashOptions = [ "errexit" ];
    text = ''
      INSTALLATION_ID=210377
      GATEWAY_SERIAL=7637415026914199
      DEVICE_ID=0
      REFRESH_TOKEN="$(< /run/agenix/viessmann-refresh-token)"

      # Returns: 0 on success (prints temperature), 1 on transient failure, 2 on terminal failure.
      # On failure prints a human-readable reason to stderr.
      getOutsideTemperature() {
          local token_body token_status access_token features_body features_status

          token_body="$(mktemp -t claude-code.viessmann-token.XXXXXX)"
          features_body="$(mktemp -t claude-code.viessmann-features.XXXXXX)"
          trap 'rm -f "''${token_body}" "''${features_body}"' RETURN

          token_status="$(curl -sS \
                               -o "''${token_body}" \
                               -w '%{http_code}' \
                               -X POST \
                               --url "https://iam.viessmann-climatesolutions.com/idp/v2/token" \
                               -H "Content-Type: application/x-www-form-urlencoded" \
                               -d "grant_type=refresh_token&client_id=45e59eb93fb498140de733c44637d8df&refresh_token=''${REFRESH_TOKEN}")" || {
              echo "token request: network error" >&2
              return 1
          }

          if [[ "''${token_status}" == "400" || "''${token_status}" == "401" ]]; then
              local err
              err="$(jq -r '.error // "unknown"' < "''${token_body}" 2>/dev/null || echo unknown)"
              echo "token request: HTTP ''${token_status} (''${err}) — refresh token likely expired, see packages/default.nix for renewal" >&2
              return 2
          fi

          if [[ "''${token_status}" != "200" ]]; then
              echo "token request: HTTP ''${token_status}" >&2
              return 1
          fi

          access_token="$(jq -r .access_token < "''${token_body}")"
          if [[ -z "''${access_token}" || "''${access_token}" == "null" ]]; then
              echo "token request: response had no access_token" >&2
              return 2
          fi

          features_status="$(curl -sS \
                                  -o "''${features_body}" \
                                  -w '%{http_code}' \
                                  -X GET \
                                  -H "Authorization: Bearer ''${access_token}" \
                                  --url "https://api.viessmann-climatesolutions.com/iot/v2/features/installations/''${INSTALLATION_ID}/gateways/''${GATEWAY_SERIAL}/devices/''${DEVICE_ID}/features")" || {
              echo "features request: network error" >&2
              return 1
          }

          if [[ "''${features_status}" != "200" ]]; then
              echo "features request: HTTP ''${features_status}" >&2
              # 401/403/404 won't fix themselves by retrying.
              case "''${features_status}" in
                  401|403|404) return 2 ;;
                  *) return 1 ;;
              esac
          fi

          local temp
          temp="$(jq -r '.data[] | select(.feature == "heating.sensors.temperature.outside") | .properties.value.value' < "''${features_body}")"
          if [[ -z "''${temp}" || "''${temp}" == "null" ]]; then
              echo "features response: outside temperature feature missing" >&2
              return 2
          fi
          printf '%s' "''${temp}"
      }

      TEMP=""
      REASON=""
      ERR_FILE="$(mktemp -t claude-code.viessmann-err.XXXXXX)"
      trap 'rm -f "''${ERR_FILE}"' EXIT
      c=0
      while :; do
          c=$((c + 1))
          : > "''${ERR_FILE}"
          TEMP_OUT="$(getOutsideTemperature 2>"''${ERR_FILE}")" && rc=0 || rc=$?
          if (( rc == 0 )); then
              TEMP="''${TEMP_OUT}"
              break
          fi
          REASON="$(< "''${ERR_FILE}")"
          if (( rc == 2 )); then
              break
          fi
          if (( c >= 10 )); then
              break
          fi
          sleep 3
      done

      if [[ -n "''${TEMP}" ]]; then
        notifySendHome "$(printf "Aktuelle Temperatur: %.01f °C" "''${TEMP}")"
      else
        notifySendHome "Aktuelle Temperatur konnte nicht ermittelt werden (Versuche: $c): ''${REASON:-unbekannter Fehler}"
      fi
    '';
  };

  emacs-ediff-dispatch = writeShellApplication {
    name = "ediff-dispatch";
    runtimeInputs = [ ];
    inheritPath = true;
    bashOptions = [ "errexit" ];
    text = ''
      GIVEN_ARGS="$*"

      MODE="$1"

      shift

      if [[ "$MODE" == "merge" ]]; then
        emacsclient -c -e "(ediff-merge-files-with-ancestor \"$1\" \"$2\" \"$3\" nil \"$4\")"
      elif [[ "$MODE" == "diff-file" ]]; then
        if [[ -z $3 ]]; then
          emacsclient -c -e "(ediff-files \"$1\" \"$2\")"
        else
          emacsclient -c -e "(ediff-files3 \"$1\" \"$2\" \"$3\")"
        fi
      elif [[ "$MODE" == "diff-dir" ]]; then
        emacsclient -c -e "(ediff-directories \"$1\" \"$2\" \".*\")"
      else
        echo "Invalid arguments: '$GIVEN_ARGS'"
        exit 1
      fi
    '';
  };

  flameshotOcr = writeShellApplication {
    name = "flameshotOcr";

    runtimeInputs = [
      flameshot
      tesseract
      gxmessage
    ];

    text = ''
      bash -c 'flameshot gui -s -r |
        convert - -colorspace Gray -scale 1191x2000 -unsharp 6.8x2.69+0 -resize 500% png:- |
        tesseract - - |
        gxmessage -title "Decoded Data" -fn "Consolas 12" -wrap -geometry 640x480 -file -'
    '';
  };

  ripgrepFzf = writeShellApplication {
    name = "rgf";
    runtimeInputs = [
      bat
      fzf
    ];
    text = ''
      RELOAD='reload:rg --column --color=always --smart-case {q} || :'

      fzf < /dev/null \
          --disabled --ansi \
          --bind "start:$RELOAD" --bind "change:$RELOAD" \
          --bind 'enter:become:emacsclient -n -c +{2} {1}' \
          --bind 'ctrl-o:execute:emacsclient -n -c +{2} {1}' \
          --delimiter : \
          --preview 'bat --style=full --color=always --highlight-line {2} {1}' \
          --preview-window '~4,+{2}+4/3,<80(up)'
    '';
  };

  recordMeeting = writeShellApplication {
    name = "record-meeting";
    runtimeInputs = [
      pulseaudioFull
      ffmpeg
      coreutils
      gnugrep
    ];
    inheritPath = false;
    bashOptions = [ "errexit" ];
    text = ''
      OUTPUT_DIR="."
      LABEL="''${1:+_''${1// /-}}"
      FILENAME="$OUTPUT_DIR/meeting_$(date +%Y%m%d_%H%M)''${LABEL}.mp3"

      find_mic() {
        # Prefer RUNNING mic, fall back to any non-monitor source
        pactl list short sources | grep -v monitor | grep RUNNING | head -1 | cut -f2 \
          || pactl list short sources | grep -v monitor | grep -v SUSPENDED | head -1 | cut -f2 \
          || pactl list short sources | grep -v monitor | head -1 | cut -f2 \
          || true
      }

      find_speaker() {
        # Prefer RUNNING monitor, fall back to any monitor source
        pactl list short sources | grep monitor | grep RUNNING | head -1 | cut -f2 \
          || pactl list short sources | grep monitor | grep -v SUSPENDED | head -1 | cut -f2 \
          || pactl list short sources | grep monitor | head -1 | cut -f2 \
          || true
      }

      MIC="$(find_mic)"
      SPK="$(find_speaker)"

      # If no speaker monitor yet (meeting not started), wait for one
      if [[ -z "$SPK" ]]; then
        echo "Waiting for speaker output to become available..."
        for i in $(seq 1 60); do
          sleep 2
          SPK="$(find_speaker)"
          if [[ -n "$SPK" ]]; then
            break
          fi
          echo "  still waiting... (''${i}/60)"
        done
      fi

      if [[ -z "$SPK" ]]; then
        echo "ERROR: No speaker output found after 2 minutes" >&2
        exit 1
      fi

      # Re-check mic in case it became available while waiting
      if [[ -z "$MIC" ]]; then
        MIC="$(find_mic)"
      fi

      echo "$FILENAME"

      if [[ -z "$MIC" ]]; then
        echo "WARNING: No mic found, recording speaker only" >&2
        ffmpeg -hide_banner -loglevel quiet -stats \
               -f pulse -i "$SPK" -ac 1 -ar 24000 -b:a 64k "$FILENAME"
      else
        echo "Recording both sides:"
        echo "  Mic:     $MIC"
        echo "  Speaker: $SPK"
        echo "  Output:  $FILENAME"
        echo "  Press Ctrl+C to stop"
        echo
        ffmpeg -hide_banner -loglevel quiet -stats \
               -f pulse -i "$SPK" \
               -f pulse -i "$MIC" \
               -filter_complex amix=inputs=2:duration=longest \
               -ac 1 -ar 24000 -b:a 64k "$FILENAME"
      fi

      echo ""
      echo "Saved: $FILENAME"
    '';
  };

  mpv-watch-later-overview = writers.writePython3Bin "mpv-watch-later-overview" { } (
    builtins.readFile ./mpv-watch-later-overview.py
  );

  claude-history = writers.writePython3Bin "claude-history" {
    libraries = [ python3Packages.colorama ];
  } (builtins.readFile ./claude-history.py);

  gemini-vision = writeShellApplication {
    name = "gemini-vision";
    runtimeInputs = [
      coreutils
      curl
      jq
      pass
    ];
    text = builtins.readFile ./gemini-vision.sh;
  };

  chronic-file = writeShellApplication {
    name = "chronic-file";
    runtimeInputs = [ coreutils ];
    text = ''
      show_time=0
      if [[ "''${1:-}" == "--time" ]]; then
          show_time=1
          shift
      fi

      if [[ $# -eq 0 ]]; then
          echo "Usage: chronic-file [--time] <command> [args...]" >&2
          exit 1
      fi

      tmpfile=$(mktemp -t chronic-file.XXXXXX.log)

      start=$(date +%s%3N)

      exit_code=0
      "$@" >"$tmpfile" 2>&1 || exit_code=$?

      end=$(date +%s%3N)
      elapsed_ms=$(( end - start ))

      if (( elapsed_ms < 1000 )); then
          duration="''${elapsed_ms}ms"
      else
          duration="$(( elapsed_ms / 1000 )).$( printf '%03d' $(( elapsed_ms % 1000 )) | cut -c1-1 )s"
      fi

      if [[ $exit_code -eq 0 ]]; then
          rm -f "$tmpfile"
          if [[ $show_time -eq 1 ]]; then
              echo "OK (''${duration})" >&2
          fi
      else
          echo "FAILED (exit $exit_code, ''${duration}): $tmpfile" >&2
          exit $exit_code
      fi
    '';
  };

  rofiDownloadsPicker = writeShellApplication {
    name = "rofiDownloadsPicker";
    runtimeInputs = [
      coreutils
      findutils
      rofi
      xclip
      libnotify
      dragon-drop
      xdg-utils
    ];
    # pure = false: xdg-open dispatches to whatever handler the user
    # has installed, so it needs the session PATH, not just deps.
    inheritPath = true;
    bashOptions = [ "errexit" ];
    text = ''
      downloads="$HOME/Downloads"
      if [ ! -d "$downloads" ]; then
        notify-send "rofi-downloads" "no ~/Downloads"
        exit 1
      fi

      ret=0
      # Recurse into subdirs; %P yields paths relative to $downloads
      # (kept mtime-sorted, newest first) so "$downloads/$rel" holds.
      # -multi-select: Shift+Enter marks rows; rofi returns one per line.
      choice=$(find "$downloads" -type f -printf '%T@\t%P\n' 2>/dev/null \
        | sort -rn | cut -f2- \
        | rofi -dmenu -i -matching fuzzy -sort -multi-select -p "downloads" \
            -kb-custom-1 "Alt+d" \
            -kb-custom-2 "Alt+o" \
            -mesg "Enter: copy  |  Alt+d: drag  |  Alt+o: open  |  Shift+Enter: mark") || ret=$?
      [ -z "$choice" ] && exit 0

      # One absolute path per selected line.
      mapfile -t rels <<< "$choice"
      full=()
      for rel in "''${rels[@]}"; do
        full+=("$downloads/$rel")
      done

      case "$ret" in
        10)
          xdragon -x "''${full[@]}"
          ;;
        11)
          for f in "''${full[@]}"; do xdg-open "$f"; done
          ;;
        *)
          # Command substitution strips the trailing newline, so a single
          # selection copies clean and many copy newline-joined.
          clip=$(printf '%s\n' "''${full[@]}")
          printf '%s' "$clip" | xclip -i -selection clipboard
          notify-send "Copied path(s)" "$choice"
          ;;
      esac
    '';
  };

  rofiStuffTodayPicker = writeShellApplication {
    name = "rofiStuffTodayPicker";
    runtimeInputs = [
      coreutils
      findutils
      rofi
      xclip
      libnotify
      dragon-drop
      xdg-utils
    ];
    # pure = false: xdg-open dispatches to whatever handler the user
    # has installed, so it needs the session PATH, not just deps.
    inheritPath = true;
    bashOptions = [ "errexit" ];
    text = ''
      # ~/Stuff/Today is a symlink kept pointing at today's dated
      # scratch dir by the `cdt` zsh function. -d follows it, so a
      # dangling/missing link fails this guard.
      today="$HOME/Stuff/Today"
      if [ ! -d "$today" ]; then
        notify-send "rofi-today" "no ~/Stuff/Today"
        exit 1
      fi

      ret=0
      # -L follows the Today symlink, then recurse; %P yields paths
      # relative to $today (mtime-sorted) so "$today/$rel" holds.
      # -multi-select: Shift+Enter marks rows; rofi returns one per line.
      choice=$(find -L "$today" -type f -printf '%T@\t%P\n' 2>/dev/null \
        | sort -rn | cut -f2- \
        | rofi -dmenu -i -matching fuzzy -sort -multi-select -p "today" \
            -kb-custom-1 "Alt+d" \
            -kb-custom-2 "Alt+o" \
            -mesg "Enter: copy  |  Alt+d: drag  |  Alt+o: open  |  Shift+Enter: mark") || ret=$?
      [ -z "$choice" ] && exit 0

      # One absolute path per selected line.
      mapfile -t rels <<< "$choice"
      full=()
      for rel in "''${rels[@]}"; do
        full+=("$today/$rel")
      done

      case "$ret" in
        10)
          xdragon -x "''${full[@]}"
          ;;
        11)
          for f in "''${full[@]}"; do xdg-open "$f"; done
          ;;
        *)
          # Command substitution strips the trailing newline, so a single
          # selection copies clean and many copy newline-joined.
          clip=$(printf '%s\n' "''${full[@]}")
          printf '%s' "$clip" | xclip -i -selection clipboard
          notify-send "Copied path(s)" "$choice"
          ;;
      esac
    '';
  };
}
