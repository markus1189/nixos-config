{ buku
, cacert
, curl
, coreutils
, dragon-drop
, emacs
, feh
, findutils
, firefox
, gawk
, gnugrep
, gnuplot
, gnused
, haskellPackages
, i3lock
, jo
, jq
, less
, lib,  git
, libnotify
, markus-wallpapers
, nixos-artwork
, oathToolkit
, playerctl
, psmisc
, procps
, pulseaudioFull
, rofi
, sqlite
, scrot
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
, zsh
}:

rec {
  writeXrandrScript = args: text: writeShellScript args ''
    ${text}

    notify-send xrandr "${args.name}"
  '';

  # Create a shell script from the given text.
  writeShellScript =
    { name # the filename
    , pure ? true # if pure is true, only include the specified dependencies in PATH
    , deps ? [] # dependencies to include in PATH
    , failFast ? true
    }: text:
    writeScriptBin name ''
      #!${stdenv.shell}

      ${lib.optionalString (failFast) ''
      set -e

      ''}
      export PATH=${lib.makeBinPath deps}${lib.optionalString (!pure) ":$PATH"}

      ${text}
    '';

  tmx = writeShellScript { name = "tmx"; pure = false; deps = [ tmux zsh ]; } ''
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

  git-pretty-log = writeShellScript {
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

  isVpnActive = writeShellScript {
    name = "isVpnActive";
    deps = [ systemd procps ];
    failFast = false;
  } ''
    OUTPUT="$(systemctl is-active openvpn-*.service)"
    OPENCONNECT="$(pgrep openconnect)"
    VPNC="$(pgrep vpnc)"
    COLOR=$(if [[ "$OUTPUT" == active || ! -z "$OPENCONNECT" || ! -z "$VPNC" ]]; then echo lightgreen; else echo red; fi)
    echo "<fc=$COLOR>VPN</fc>"
  '';

  ts = writeShellScript {
    name = "ts";
    deps = [ coreutils ];
  } ''
    FORMAT="''${1:-"%H:%M:%S"}"
    while read -r line ; do echo "$(date "+$FORMAT"): $line"; done
  '';

  multiheadLeft = writeXrandrScript {
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

  multiheadRight = writeXrandrScript {
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

  singlehead = writeXrandrScript {
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

  asusRight = writeXrandrScript {
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

  multihead4k = writeXrandrScript {
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

  multihead4khdmi = writeXrandrScript {
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

  widePbpRight = writeXrandrScript {
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

  widePbpBoth = writeXrandrScript {
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

  wideUltra = writeXrandrScript {
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

  autoMonitorConfig = writeShellScript {
    name = "autoMonitorConfig";
    pure = true;
    deps = [ wpa_supplicant gnugrep libnotify coreutils multihead4k multihead4khdmi ];
  } ''
    CURRENT="$(wpa_cli -i wlp2s0 status | grep '^ssid' | cut -d'=' -f 2)"

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

  wpaSelectNetwork = { id , network ? id }: device:
    writeShellScript {
      name = "wpaCliSelectNetwork${network}";
      deps = [ wpa_supplicant gnugrep libnotify ];
    } ''
      set -e

      wpa_cli -i ${device} select_network ${id}

      notify-send wpa_cli "Switched network: ${network}"
    '';

  wpaOurFritzBox = wpaSelectNetwork { id = "4"; network = "our-fritzbox"; };

  wpaWannaCry = wpaSelectNetwork { id = "43"; network = "wanna-cry"; };

  sysdig-trace-in = writeShellScript {
    name = "sysdig-trace-in";
  } ''
    echo ">:''${2:-pp}:''${1:-$0}::" > /dev/null
  '';

  sysdig-trace-out = writeShellScript {
    name = "sysdig-trace-out";
  } ''
    echo "<:''${2:-pp}:''${1:-$0}::" > /dev/null
  '';

  takeScreenshot = writeShellScript {
    name = "takeScreenshot";
    deps = [ coreutils scrot libnotify xclip dragon-drop ];
  } ''
   sleep 0.5
   notify-send -t 1000 'Screenshot' 'Select area to capture'
   FILEPATH="$(scrot -q 100 -s -c -e 'mv $f /tmp/ && echo -n /tmp/$f')"
   echo "''${FILEPATH}" | xclip -sel clipboard -i
   dragon -x "''${FILEPATH}"
  '';

  gnuplot-quick = writeShellScript {
    name = "gnuplot-quick";
    deps = [ gnuplot ];
  } ''
    FILE=''${1}

    if [ -z "$1" ]; then
        FILE="-"
    fi

    gnuplot -persist -e "set style line 1 lc rgb '#0060ad' lt 1 lw 2 pt 7 ps 1.5; set autoscale; set grid; plot '$FILE' with linespoints ls 1"
  '';

  acConnected = writeShellScript { name = "acConnected"; deps = [ coreutils ]; } ''
    cat /sys/class/backlight/intel_backlight/max_brightness > \
      /sys/class/backlight/intel_backlight/brightness

    echo -n 2 > /sys/class/leds/dell::kbd_backlight/brightness
  '';

  acDisconnected = writeShellScript { name = "acDisconnected"; } ''
    echo -n 700 > /sys/class/backlight/intel_backlight/brightness
    echo -n 0 > /sys/class/leds/dell::kbd_backlight/brightness
  '';

  xmonadReset = writeShellScript { name = "xmonadReset"; deps = [ haskellPackages.xmonad psmisc ]; } ''
    killall xmobar
    xmonad --restart
  '';

  centerMouse = writeShellScript { name = "centerMouse"; deps = [ xdotool ]; } ''
    xdotool mousemove --window $(xdotool getwindowfocus) --polar 0 0
  '';

  toggleSoundMute = writeShellScript {
    name = "toggleSoundMute";
    deps = [ pulseaudioFull ];
  } ''
    pactl set-sink-mute '@DEFAULT_SINK@' toggle
  '';

  chooseNetwork = device: writeShellScript { name = "chooseNetwork"; deps = [ libnotify gnugrep coreutils wpa_supplicant gnused rofi ]; pure = true; } ''
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

  lockScreen = writeShellScript {
    name = "lockScreen";
    deps = [ i3lock ];
  } ''
    i3lock -p win -e -f -i ${nixos-artwork.wallpapers.simple-dark-gray}/share/artwork/gnome/nix-wallpaper-simple-dark-gray.png

  '';

  selectSpotifyPlayer = writeShellScript {
    name = "selectSpotifyPlayer";
    deps = [ wmctrl ];
  } "wmctrl -x -a spotify";

  currentSpotifySong = writeShellScript {
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

  emacsAnywhere = writeShellScript {
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

  mfaHelper = writeShellScript {
    name = "mfaHelper";
    deps = [ xsel oathToolkit coreutils gnugrep ];
    failFast = true;
  } ''
    if [[ -z "''${1}" ]]; then
        cat ~/.2fa | cut -d= -f 1
    else
        SECRET="$(grep -F "''${1}=" ~/.2fa | head -n 1 | cut -d= -f 2)"
        if [[ -z "''${SECRET}" ]]; then
            rofi -e "Could not find a secret for \"''${1}\"!"
        else
            TOTP="$(oathtool --totp -b "''${SECRET}")"
            echo -n "''${TOTP}" | xsel --clipboard > /dev/null
            exit 0
        fi
    fi
  '';

  rofiDefaults = writeShellScript {
    name = "rofiDefaults";
    deps = [ rofi ];
    pure = false;
  } ''
    rofi -i -monitor -4 -disable-history "$@"
  '';

  browserHistory = writeShellScript {
    name = "browserHistory";
    deps = [ sqlite xsv coreutils rofi xclip ];
    pure = true;
  } ''
    FIREFOX_HISTORY="$HOME/.mozilla/firefox/gxr7euud.default-1519975571727/places.sqlite"
    CHROMIUM_HISTORY="$HOME/.config/chromium/Profile 3/History"

    cp "''${FIREFOX_HISTORY}" /tmp/firefox-history
    cp "''${CHROMIUM_HISTORY}" /tmp/chromium-history

    {
      sqlite3 -csv /tmp/chromium-history "SELECT title, url, visit_count, 'chromium' FROM urls ORDER BY visit_count DESC"
      sqlite3 -csv /tmp/firefox-history "SELECT title, url, visit_count, 'firefox' FROM moz_places ORDER BY visit_count DESC"
    } | xsv sort -NRs3 | rofi -dmenu -disable-history -i -monitor -4 | xsv select 2 | xclip -sel clipboard -i
  '';

  notifySendPb = pushBulletToken: writeShellScript {
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

  notifySendTelegram = botToken: writeShellScript {
    name = "notifySendTelegram";
    deps = [ curl jo cacert ];
    pure = true;
  } ''
    MESSAGE=''${1:?"Error: no message given!"}
    curl --silent --fail -XPOST \
     --cacert ${cacert}/etc/ssl/certs/ca-bundle.crt \
      -H 'Content-Type: application/json' \
      -d "$(jo chat_id=299952716 text="''${MESSAGE}")" \
      --url "https://api.telegram.org/bot${botToken}/sendMessage"
    '';

  telegramSendPhoto = botToken: writeShellScript {
    name = "telegramSendPhoto";
    deps = [ curl jo coreutils ];
    pure = true;
  } ''
    LIMIT=10

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

  telegramPhotosLastYear = botToken: writeShellScript {
    name = "telegramPhotosLastYear";
    deps = [ findutils (telegramSendPhoto botToken) ];
    pure = true;
  } ''
    set -o pipefail
    find ''${1:?No path to photos directory given} -name "*$(date -d '-1 year' +%Y%m%d)*" | head -1 | xargs telegramSendPhoto
    '';

  bukuRun = writeShellScript {
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


  logArgs = writeShellScript {
    name = "log-args";
    deps = [ systemd ];
    pure = false;
  } ''
    systemd-cat -tlog-args -- bash -c 'echo $@'
  '';
}
