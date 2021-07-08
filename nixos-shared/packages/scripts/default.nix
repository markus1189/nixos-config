{ buku
, cacert
, coreutils
, curl
, dragon-drop
, emacs
, feh
, findutils
, firefox
, gawk
, git
, gnugrep
, gnuplot
, gnused
, haskellPackages
, i3lock
, imagemagick
, jo
, jq
, less
, lib
, libnotify
, markus-wallpapers
, nixos-artwork
, oathToolkit
, playerctl
, procps
, psmisc
, pulseaudioFull
, python38
, python38Packages
, rofi
, rsstail
, scrot
, sqlite
, stdenv
, systemd
, tmux
, unixtools
, wmctrl
, wpa_supplicant
, writeScriptBin
, xclip
, xdotool
, xorg
, xsel
, xsv
, zbar
, zsh

}:

rec {
  writeXrandrScript = args: text:
    writeShellScript args ''
      ${text}

      notify-send xrandr "${args.name}"
    '';

  # Create a shell script from the given text.
  writeShellScript =
    { name # the filename
    , pure ? true # if pure is true, only include the specified dependencies in PATH
    , deps ? [ ] # dependencies to include in PATH
    , failFast ? true
    }:
    text:
    writeScriptBin name ''
      #!${stdenv.shell}

      ${lib.optionalString (failFast) ''
        set -e

      ''}
      export PATH=${lib.makeBinPath deps}${lib.optionalString (!pure) ":$PATH"}

      ${text}
    '';

  tmx = writeShellScript
    {
      name = "tmx";
      pure = false;
      deps = [ tmux zsh ];
    } ''
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

    main $*
  '';

  git-pretty-log = writeShellScript
    {
      name = "git-pretty-log";
      deps = [ git gnused less ];
    } ''
    HASH="%C(yellow)%h%Creset"
    RELATIVE_TIME="%Cgreen(%ar)%Creset"
    AUTHOR="%C(bold blue)<%aN>%Creset"
    REFS="%C(red)%d%Creset"
    SUBJECT="%s"
    GPG="%C(yellow)%G?%Creset"

    FORMAT="''${HASH} ''${RELATIVE_TIME} ''${AUTHOR} ''${REFS} ''${SUBJECT}"

    pretty_git_log() {
      git log --graph --color --pretty="tformat:''${FORMAT}" $* |
        sed -Ee 's/(^[^<]*) ago\)/\1)/' |
        sed -Ee 's/(^[^<]*), [[:digit:]]+ .*months?\)/\1)/' |
        less -FXRS
    }

    pretty_git_log $*
  '';

  isVpnActive = writeShellScript
    {
      name = "isVpnActive";
      deps = [ systemd procps gnugrep ];
      failFast = false;
    } ''

    OPENVPN="$(systemctl is-active 'openvpn-*.service' | grep active)"
    WIREGUARD="$(systemctl is-active 'wg-quick-*.service' | grep active)"
    COLOR=$(if [[ -n "$OPENVPN" || -n "$WIREGUARD" ]]; then echo lightgreen; else echo red; fi)
    LABEL=$(if [[ -n "$OPENVPN" ]]; then echo "OVP"; elif [[ -n "$WIREGUARD" ]]; then echo "WGD"; else echo "VPN"; fi)
    echo "<fc=$COLOR>''${LABEL}</fc>"
  '';

  togglTimer = togglApiToken: writeShellScript
    {
      name = "togglTimer";
      deps = [ jq cacert curl coreutils ];
      failFast = false;
    } ''
      if curl -s -u ${togglApiToken}:api_token -X GET https://api.track.toggl.com/api/v8/time_entries/current | jq -e '.data' > /dev/null; then
        OUTPUT_SUM=$(curl -s -u ${togglApiToken}:api_token -G --data-urlencode "start_date=$(date -d '12 hours ago' --iso-8601=s)" 'https://api.track.toggl.com/api/v8/time_entries' |
            jq -e -r 'map(if .duration < 0 then now + .duration else .duration end) | add')

        if [[ "$?" == 0 ]]; then
            echo "''${OUTPUT_SUM}" | jq -r '{hours: (. / 3600 | floor), minutes: (. % 3600 / 60 | round)} | "\(.hours)h \(.minutes)m "'
        else
            echo "0h 0m "
        fi
      else
        echo ""
      fi
      '';

  ts = writeShellScript
    {
      name = "ts";
      deps = [ coreutils ];
    } ''
    FORMAT="''${1:-"%H:%M:%S"}"
    while read -r line ; do echo "$(date "+$FORMAT"): $line"; done
  '';

  multiheadLeft = writeXrandrScript
    {
      name = "multiheadLeft";
      deps = [ xorg.xrandr libnotify ];
    } ''
    xrandr --output VIRTUAL1 --off \
           --output eDP1 --mode 1920x1080 --pos 1920x0 --rotate normal \
           --output DP1 --off \
           --output HDMI3 --off \
           --output HDMI2 --off \
           --output HDMI1 --mode 1920x1080 --pos 0x0 --rotate normal \
           --output VGA2 --off \
           --output DP2 --off
  '';

  multiheadRight = writeXrandrScript
    {
      name = "multiheadRight";
      deps = [ xorg.xrandr libnotify ];
    } ''
    xrandr --output VIRTUAL1 --off \
           --output eDP1 --mode 1920x1080 --pos 0x0 --rotate normal \
           --output DP1 --off \
           --output HDMI3 --off \
           --output HDMI2 --off \
           --output HDMI1 --mode 1920x1080 --pos 1920x0 --rotate normal \
           --output VGA2 --off \
           --output DP2 --off
  '';

  singlehead = writeXrandrScript
    {
      name = "singlehead";
      deps = [ xorg.xrandr libnotify feh ];
    } ''
    xrandr --output VIRTUAL1 --off \
           --output eDP1 --mode 1920x1080 --pos 0x0 --rotate normal \
           --output DP1 --off \
           --output HDMI3 --off \
           --output HDMI2 --off \
           --output HDMI1 --off \
           --output VGA2 --off \
           --output DP2 --off

    feh --no-fehbg --bg-fill ${markus-wallpapers.cc} &
  '';

  asusRight = writeXrandrScript
    {
      name = "asusRight";
      deps = [ xorg.xrandr libnotify ];
    } ''
    xrandr --output VIRTUAL1 --off \
           --output eDP1 --primary --mode 1920x1080 --pos 0x0 --rotate normal \
           --output DP1 --mode 1920x1080 --pos 1920x0 --rotate normal \
           --output HDMI2 --off \
           --output HDMI1 --off \
           --output DP2 --off
  '';

  multihead4k = writeXrandrScript
    {
      name = "multihead4k";
      deps = [ xorg.xrandr libnotify ];
    } ''
    xrandr --output VIRTUAL1 --off \
           --output eDP1 --primary --mode 1920x1080 --pos 1920x0 --scale 1x1 --rotate normal \
           --output DP1 --mode 3840x2160 --scale 0.5x0.5 --pos 0x0 --rotate normal \
           --output HDMI2 --off \
           --output HDMI1 --off \
           --output DP2 --off
  '';

  multihead4khdmi = writeXrandrScript
    {
      name = "multihead4khdmi";
      deps = [ xorg.xrandr libnotify ];
    } ''
    xrandr --output VIRTUAL1 --off \
           --output eDP1 --primary --mode 1920x1080 --pos 0x0 --scale 1x1 --rotate normal \
           --output HDMI1 --mode 3840x2160 --scale 0.5x0.5 --pos 1920x0 --rotate normal \
           --output HDMI2 --off \
           --output DP1 --off \
           --output DP2 --off
  '';

  widePbpRight = writeXrandrScript
    {
      name = "widePbpRight";
      deps = [ xorg.xrandr libnotify ];
    } ''
    xrandr --output eDP1 --primary --mode 1920x1080 --pos 1920x0 --rotate normal \
           --output DP1 --off \
           --output DP2 --off \
           --output HDMI1 --mode 1920x1600 --pos 0x0 --rotate normal \
           --output HDMI2 --off \
           --output VIRTUAL1 --off
  '';

  widePbpBoth = writeXrandrScript
    {
      name = "widePbpBoth";
      deps = [ xorg.xrandr libnotify feh ];
    } ''
    xrandr --output eDP1 --off \
           --output DP1 --off \
           --output DP2 --primary --mode 1920x1600 --pos 0x0 --rotate normal \
           --output HDMI1 --mode 1920x1600 --pos 1920x0 --rotate normal \
           --output HDMI2 --off \
           --output VIRTUAL1 --off

    feh --no-fehbg --bg-fill ${markus-wallpapers.shrike-rape-10x8-flipped} ${markus-wallpapers.shrike-rape-10x8} &
  '';

  wideUltra = writeXrandrScript
    {
      name = "wideUltra";
      deps = [ xorg.xrandr libnotify feh ];
    } ''
    xrandr --output eDP1 --mode 1920x1080 --pos 3840x0 --rotate normal \
           --output DP1 --primary --mode 3840x1600 --pos 0x0 --rotate normal \
           --output DP2 --off \
           --output HDMI1 --off \
           --output HDMI2 --off \
           --output VIRTUAL1 --off

    feh --no-fehbg --bg-fill ${markus-wallpapers.shrike-rape-21x9} &
  '';

  autoMonitorConfig = wirelessInterface: writeShellScript
    {
      name = "autoMonitorConfig";
      pure = true;
      deps = [
        wpa_supplicant
        gnugrep
        libnotify
        coreutils
        multihead4k
        multihead4khdmi
      ];
    } ''
    CURRENT="$(wpa_cli -i ${wirelessInterface} status | grep '^ssid' | cut -d'=' -f 2)"

    if [[ -z "''${CURRENT}" ]]; then
        notify-send wpa_cli "Could not find current SSID!"
        exit 1
    fi

    case "''${CURRENT}" in
        "cc-wlan")
            multihead4k
            ;;
        "Our FRITZ Box")
            ${widePbpBoth}/bin/widePbpBoth || ${widePbpRight}/bin/widePbpRight || ${wideUltra}/bin/wideUltra
            ;;
        *)
            echo "Unknown network: ''${CURRENT}" > /dev/stderr
    esac
  '';

  wpaSelectNetwork = { id, network ? id }:
    device:
    writeShellScript
      {
        name = "wpaCliSelectNetwork${network}";
        deps = [ wpa_supplicant gnugrep libnotify ];
      } ''
      set -e

      wpa_cli -i ${device} select_network ${id}

      notify-send wpa_cli "Switched network: ${network}"
    '';

  wpaOurFritzBox = wpaSelectNetwork {
    id = "4";
    network = "our-fritzbox";
  };

  wpaWannaCry = wpaSelectNetwork {
    id = "43";
    network = "wanna-cry";
  };

  sysdig-trace-in = writeShellScript { name = "sysdig-trace-in"; } ''
    echo ">:''${2:-pp}:''${1:-$0}::" > /dev/null
  '';

  sysdig-trace-out = writeShellScript { name = "sysdig-trace-out"; } ''
    echo "<:''${2:-pp}:''${1:-$0}::" > /dev/null
  '';

  takeScreenshot = writeShellScript
    {
      name = "takeScreenshot";
      deps = [ coreutils scrot libnotify xclip dragon-drop ];
    } ''
    sleep 0.5
    notify-send -t 1000 'Screenshot' 'Select area to capture'
    FILEPATH="$(scrot -q 100 -s -c -e 'mv $f /tmp/ && echo -n /tmp/$f')"
    echo "''${FILEPATH}" | xclip -sel clipboard -i
    dragon -x "''${FILEPATH}"
  '';

  gnuplot-quick = writeShellScript
    {
      name = "gnuplot-quick";
      deps = [ gnuplot ];
    } ''
    FILE=''${1}

    if [ -z "$1" ]; then
        FILE="-"
    fi

    gnuplot -persist -e "set style line 1 lc rgb '#0060ad' lt 1 lw 2 pt 7 ps 1.5; set autoscale; set grid; plot '$FILE' with linespoints ls 1"
  '';

  acConnected = writeShellScript
    {
      name = "acConnected";
      deps = [ coreutils findutils ];
    } ''
    cat /sys/class/backlight/intel_backlight/max_brightness > \
      /sys/class/backlight/intel_backlight/brightness

    echo -n 2 > $(find /sys/class/leds -name '*::kbd_backlight')/brightness
  '';

  acDisconnected = writeShellScript { name = "acDisconnected"; } ''
    echo -n 700 > /sys/class/backlight/intel_backlight/brightness
    echo -n 0 > $(find /sys/class/leds -name '*::kbd_backlight')/brightness
  '';

  xmonadReset = writeShellScript
    {
      name = "xmonadReset";
      deps = [ haskellPackages.xmonad psmisc ];
    } ''
    killall xmobar
    xmonad --restart
  '';

  centerMouse = writeShellScript
    {
      name = "centerMouse";
      deps = [ xdotool ];
    } ''
    xdotool mousemove --window $(xdotool getwindowfocus) --polar 0 0
  '';

  toggleSoundMute = writeShellScript
    {
      name = "toggleSoundMute";
      deps = [ pulseaudioFull ];
    } ''
    pactl set-sink-mute '@DEFAULT_SINK@' toggle
  '';

  chooseNetwork = device:
    writeShellScript
      {
        name = "chooseNetwork";
        deps = [ libnotify gnugrep coreutils wpa_supplicant gnused rofi ];
        pure = true;
      } ''
      NETWORK_NAME="$(wpa_cli list_networks |
        sed -e  's/^[[:digit:]]\+[[:space:]]*\(.*\)any.*/\1/' |
          sed -e 's/[[:space:]]*$//' |
            grep -v 'network id' |
              tail -n +2 |
                rofi -matching fuzzy -monitor -4 -sort -dmenu -i -p ssid:)"

      NETWORK_ID=$(wpa_cli -i ${device} list_networks | grep -wF "''${NETWORK_NAME}" | grep -o '^[[:digit:]]\+')

      echo "Selecting network $NETWORK_NAME with id $NETWORK_ID"
      echo "Using command: wpa_cli -i ${device} select_network $NETWORK_ID"

      if wpa_cli -i ${device} select_network $NETWORK_ID ; then
        notify-send wpa_cli "Switched network: ''${NETWORK_NAME}"
      else
        notify-send -u critical select_network "Failed with code $?"
      fi
    '';

  lockScreen = writeShellScript
    {
      name = "lockScreen";
      deps = [ i3lock ];
    } ''
    i3lock -p win -e -f -i ${nixos-artwork.wallpapers.simple-dark-gray}/share/artwork/gnome/nix-wallpaper-simple-dark-gray.png

  '';

  currentSpotifySong = writeShellScript
    {
      name = "currentSpotifySong";
      deps = [ playerctl ];
    } ''
    getTag() {
      playerctl -p spotify metadata xesam:''${1} || true
    }

    STATUS="$(playerctl -p spotify status || true)"
    TITLE="$(getTag title)"
    ARTIST="$(getTag artist)"
    ALBUM="$(getTag album)"

    if [[ "''${STATUS}" == "Playing" ]]; then
      ALBUM2="$(if [[ "$TITLE" == "$ALBUM" ]]; then echo ; else echo "($ALBUM)"; fi)"
      echo -n "<fc=orange>''${TITLE}</fc> by <fc=orange>''${ARTIST}</fc> ''${ALBUM2}"
    fi
  '';

  emacsAnywhere = writeShellScript
    {
      name = "emacsAnywhere";
      deps = [ xdotool libnotify emacs coreutils ];
      failFast = false;
    } ''

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

  rofiDefaults = writeShellScript
    {
      name = "rofiDefaults";
      deps = [ rofi ];
      pure = false;
    } ''
    rofi -i -monitor -4 -disable-history "$@"
  '';

  browserHistory = writeShellScript
    {
      name = "browserHistory";
      deps = [ sqlite xsv coreutils rofi xclip findutils ];
      pure = true;
    } ''
    FIREFOX_HISTORY="$(find ~/.mozilla/ -name places.sqlite)"
    CHROMIUM_HISTORY="$(find ~/.config/chromium -name History)"

    cp "''${FIREFOX_HISTORY}" /tmp/firefox-history &
    cp "''${CHROMIUM_HISTORY}" /tmp/chromium-history &

    wait

    {
      sqlite3 -csv /tmp/chromium-history "SELECT title, url, visit_count, 'chromium' FROM urls ORDER BY visit_count DESC"
      sqlite3 -csv /tmp/firefox-history "SELECT title, url, visit_count, 'firefox' FROM moz_places ORDER BY visit_count DESC"
    } | xsv sort -NRs3 | rofi -dmenu -disable-history -i -monitor -4 | xsv select 2 | xclip -sel clipboard -i
  '';

  notifySendPb = pushBulletToken:
    writeShellScript
      {
        name = "notifySendPb";
        deps = [ curl jo ];
        pure = true;
      } ''
      curl --silent --fail \
       --cacert ${cacert}/etc/ssl/certs/ca-bundle.crt \
       --header 'Access-Token: ${pushBulletToken}' \
       --header 'Content-Type: application/json' \
       --data-binary "$(jo -- -s type=note -s title="''${1:-no-title}" -s body="''${2:-no-body}")" \
       --request POST \
       https://api.pushbullet.com/v2/pushes > /dev/null
    '';

  sendTelegram = chatid: name: botToken:
    writeShellScript
      {
        inherit name;
        deps = [ curl jo cacert ];
        pure = true;
      } ''
      MESSAGE=''${1:?"Error: no message given!"}
      curl --silent --fail -XPOST \
       --retry-all-errors --retry 3 \
       --cacert ${cacert}/etc/ssl/certs/ca-bundle.crt \
        -H 'Content-Type: application/json' \
        -d "$(jo chat_id=${chatid} text="''${MESSAGE}")" \
        --url "https://api.telegram.org/bot${botToken}/sendMessage"
    '';

  sendTelegramPoll = botToken:
    writeShellScript
      {
        name = "sendTelegramPoll";
        deps = [ curl jo cacert ];
        pure = true;
      } ''
      QUESTION=''${1:?"Error: no message given!"}
      shift
      curl --silent --fail -XPOST \
       --retry-all-errors --retry 3 \
       --cacert ${cacert}/etc/ssl/certs/ca-bundle.crt \
        -H 'Content-Type: application/json' \
        -d "$(jo allows_multiple_answers=true chat_id=299952716 question="''${QUESTION}" options="$(jo -a $*)")" \
        --url "https://api.telegram.org/bot${botToken}/sendPoll"
    '';

  notifySendTelegram = sendTelegram "299952716" "notifySendTelegram";

  notifySendHome = sendTelegram "-1001328938887" "notifySendHome";

  telegramSendPhoto = botToken:
    writeShellScript
      {
        name = "telegramSendPhoto";
        deps = [ curl jo coreutils ];
        pure = true;
      } ''
      LIMIT=5

      buildArray() {
          jo -a $(
              for i in "$@"; do
                  NAME="$(basename "$i")"
                  jo type=photo media="attach://$NAME"
              done | shuf -n $LIMIT
          )
      }

      buildParams() {
          for i in "$@"; do
              echo "-F $(basename "$i")=@$i"
          done | shuf -n $LIMIT
      }

      if [[ "$#" -gt "$LIMIT" ]]; then
        echo Warning: using only $LIMIT randomly chosen out of $# given args > /dev/stderr
      fi

      if [[ "$#" -ge 1 ]]; then
          echo "Uploading" > /dev/stderr

          curl --silent --fail -XPOST \
                  --cacert ${cacert}/etc/ssl/certs/ca-bundle.crt \
                  --url "https://api.telegram.org/bot${botToken}/sendMediaGroup" \
                  -F chat_id=299952716 \
                  -F media="$(buildArray "$@")" \
                  $(buildParams "$@")
      else
          echo "USAGE: $0 FILE..." > /dev/stderr
      fi
    '';

  telegramPhotosLastYear = botToken:
    writeShellScript
      {
        name = "telegramPhotosLastYear";
        deps = [ findutils (telegramSendPhoto botToken) ];
        pure = true;
      } ''
      set -o pipefail
      find ''${1:?No path to photos directory given} -name "*$(date -d '-1 year' +%Y%m%d)*" | head -1 | xargs telegramSendPhoto
    '';

  bukuRun = writeShellScript
    {
      name = "bukuRun";
      deps = [ rofi buku gawk unixtools.column firefox ];
      pure = true;
    } ''
    export BROWSER=firefox
    _rofi () {
        rofi -dmenu -i -no-levenshtein-sort -width 1000 "$@"
    }

    max_str_width=100

    main () {
        HELP="Buku Bookmarks"
        content=$(parseBuku)
        menu=$(echo "''${content}" | _rofi -p '> ' -mesg "''${HELP}")
        id=$(getId "$content" "$menu")
        for bm in ''${id}; do
            buku -o "''${bm}"
        done
    }

    parseBuku () {
      echo "$(buku --nc -p | gawk -v max="70" '
    BEGIN {
      RS=""
      FS="\n"
    }
    {
      if ($3 == "")
        $3 = " # NOTAG"
      id = gensub(/([0-9]+)\.(.*)/, "\\1", "g", $1)
      url = substr(gensub(/\s+> (.*)/, "\\1", "g", $2),0,max)
      tags = gensub(/\s+# (.*)/, "\\1", "g", $3)
      title = substr(gensub(/[0-9]+\.\s*(.*)/, "\\1", "g", $1),0,max)

      print id " " title " " url " " tags
    }
    ' | column -t -s $'\t')"
    }

    getId () {
      id=$(echo "''${2%% *}")
      if [ -z "$id" ]; then
        prev=""
        IFS=$'\n'
        for line in $1; do
          if [ "$2" = "$line" ]; then
            id=$(echo "''${prev%% *}")
            break
          else
            prev="$line"
          fi
        done
      fi
      echo $id
    }

    getTitleFromId () {
      echo "$(buku --nc -p $1 | gawk '
      BEGIN {
        RS=""
        FS="\n"
      }
      {
        print gensub(/[0-9]+\.\s*(.*)/, "\\1", "g", $1)
      }
      ')"
    }

    getUrlFromId () {
      echo "$(buku --nc -p $1 | gawk '
      BEGIN {
        RS=""
        FS="\n"
      }
      {
        print gensub(/\s+> (.*)/, "\\1", "g", $2)
      }
      ')"
    }

    getTagsFromId () {
      echo "$(buku --nc -p $1 | gawk '
      BEGIN {
        RS=""
        FS="\n"
      }
      {
        print gensub(/\s+# (.*)/, "\\1", "g", $3)
      }
      ')"
    }

    main
  '';

  logArgs = writeShellScript
    {
      name = "log-args";
      deps = [ systemd ];
      pure = false;
    } ''
    systemd-cat -tlog-args -- bash -c 'echo $@'
  '';

  addToPocketScript = { consumer_key, access_token }:
    writeShellScript
      {
        name = "add-to-pocket";
        deps = [ curl gnugrep jo ];
        pure = true;
      } ''
      URL="''${1}"
      GIVEN_TAGS="''${2}"
      TAGS="''${GIVEN_TAGS},newsboat"

      script_pocket_consumer_key=${consumer_key}
      script_pocket_access_token=${access_token}

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

      main() {
        unset c
        until curl --cacert "${cacert}/etc/ssl/certs/ca-bundle.crt" -s --fail -XPOST https://getpocket.com/v3/add -H 'content-type: application/json' -d "$(jo url="''${1}" consumer_key="''${script_pocket_consumer_key}" access_token="''${script_pocket_access_token}" tags="''${TAGS}")"; do
          ((c++)) && ((c==6)) && break
          sleep 1
        done
        unset c
        exit "$?"
      }

      main "$1"
    '';

  mkRsstailToPocketUnit = { consumer_key, access_token }:
    { key, url, intervalSeconds ? 300 }:
    let
      name = "rsstail-${key}-script";
      script = writeShellScript
        {
          inherit name;
          pure = true;
          deps = [ ];
        } ''
        ${rsstail}/bin/rsstail -n0 -i ${toString intervalSeconds} -r -l -u '${url}' \
          | ${gnugrep}/bin/grep --line-buffered '^Link: ' \
          | ${gawk}/bin/awk '{print $2; system("")}' \
          | while read i; do
              echo [rsstail-${key}]: Adding to pocket: "$i"
              ${
                addToPocketScript { inherit consumer_key access_token; }
              }/bin/add-to-pocket "$i" "rsstail"
            done
      '';
    in
    {
      "rsstail-${key}" = {
        Unit = { Description = "rsstail for ${key}"; };

        Service = {
          ExecStart = "${script}/bin/${name}";
          RestartSec = 10;
          Restart = "always";
        };

        Install = { WantedBy = [ "default.target" ]; };
      };
    };
  pyvicare =
    let
      mypython = with python38Packages;
        let
          pyvicare = buildPythonPackage rec {
            pname = "PyViCare";
            version = "0.2.4";

            src = fetchPypi {
              inherit pname version;
              sha256 =
                "sha256:1i64iazl5m0h2c862sgd5p73bnizbp2f0jq6i8k3c5x6494vklav";
            };

            propagatedBuildInputs = [ simplejson requests_oauthlib ];
            doCheck = false;
          };
        in
        python38.withPackages (ps: [ pyvicare ]);
    in
    { username, password }:
    writeScriptBin "pyvicare" ''
      #!${mypython}/bin/python

      import os
      from PyViCare.PyViCareGazBoiler import GazBoiler

      t=GazBoiler('${username}','${password}')

      print(t.getOutsideTemperature())
    '';
  sendCurrentTemperature = { pyvicareSecret, botToken }: writeShellScript
    {
      name = "sendCurrentTemperature";
      deps = [ (notifySendHome botToken) (pyvicare pyvicareSecret) coreutils ];
      pure = true;
    } ''
    unset c
    until TEMP="$(pyvicare)"; do
      ((c++)) && ((c==6)) && break
      sleep 1
    done
    unset c

    notifySendHome "$(printf "Aktuelle Temperatur: %.01f °C" "''${TEMP}")"
  '';

  captureTOTP = writeShellScript
    {
      name = "captureTOTP";
      deps = [ imagemagick zbar ];
      pure = true;
    } ''
    import -window root -quality 90 - | zbarimg -q --raw /dev/stdin 2>/dev/null
  '';
}
