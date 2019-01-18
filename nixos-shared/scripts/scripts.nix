{ pkgs ? import <nixpkgs> {} }:

with pkgs;
rec {
  writeXrandrScript = args: text: writeShellScriptBin args ''
    ${text}

    notify-send xrandr "${args.name}"
  '';

  # Create a shell script from the given text.
  writeShellScriptBin =
    { name # the filename
    , pure ? true # if pure is true, only include the specified dependencies in PATH
    , deps ? [] # dependencies to include in PATH
    , failFast ? true
    }: text:
    writeScriptBin name ''
      #!${pkgs.stdenv.shell}

      ${pkgs.lib.optionalString (failFast) ''
      set -e

      ''}
      export PATH=${lib.makeBinPath deps}${pkgs.lib.optionalString (!pure) ":$PATH"}

      ${text}
    '';

  tmx = writeShellScriptBin { name = "tmx"; pure = false; deps = [ tmux zsh ]; } ''
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

  git-pretty-log = writeShellScriptBin {
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

  ts = writeShellScriptBin {
    name = "ts";
    deps = [ coreutils ];
  } ''
    FORMAT="''${1:-"%H:%M:%S".%N}"
    while read -r line ; do echo "$(date "+$FORMAT"): $line"; done
  '';

  multihead-left = writeXrandrScript {
    name = "multihead-left";
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

  multihead-right = writeXrandrScript {
    name = "multihead-right";
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
    deps = [ xorg.xrandr libnotify ];
  } ''
    xrandr --output VIRTUAL1 --off \
           --output eDP1 --mode 1920x1080 --pos 0x0 --rotate normal \
           --output DP1 --off \
           --output HDMI3 --off \
           --output HDMI2 --off \
           --output HDMI1 --off \
           --output VGA2 --off \
           --output DP2 --off
  '';

  multihead-4k = writeXrandrScript {
    name = "multihead-4k";
    deps = [ xorg.xrandr libnotify ];
  } ''
    xrandr --output VIRTUAL1 --off \
           --output eDP1 --primary --mode 1920x1080 --pos 1920x0 --scale 1x1 --rotate normal \
           --output DP1 --mode 3840x2160 --scale 0.5x0.5 --pos 0x0 --rotate normal \
           --output HDMI2 --off \
           --output HDMI1 --off \
           --output DP2 --off
  '';

  wpa-select-network = { id , network ? id }: device:
    writeShellScriptBin {
      name = "wpa-cli-select-network-${network}";
      deps = [ wpa_supplicant gnugrep libnotify ];
    } ''
      set -e

      wpa_cli -i ${device} select_network ${id}

      notify-send wpa_cli "Switched network: ${network}"
    '';

  wpa-our-fritz-box = wpa-select-network { id = "4"; network = "our-fritzbox"; };

  wpa-wanna-cry = wpa-select-network { id = "43"; network = "wanna-cry"; };

  sysdig-trace-in = writeShellScriptBin {
    name = "sysdig-trace-in";
  } ''
    echo ">:''${2:-pp}:''${1:-$0}::" > /dev/null
  '';

  sysdig-trace-out = writeShellScriptBin {
    name = "sysdig-trace-out";
  } ''
    echo "<:''${2:-pp}:''${1:-$0}::" > /dev/null
  '';

  take-screenshot = writeShellScriptBin {
    name = "take-screenshot";
    deps = [ coreutils scrot libnotify xclip ];
  } ''
   sleep 0.5
   notify-send -t 1000 'Screenshot' 'Select area to capture'
   scrot -q 100 -s -c -e 'mv -v $f /tmp/ && echo -n /tmp/$f | xclip -sel clipboard -i'
  '';

  gnuplot-quick = writeShellScriptBin {
    name = "gnuplot-quick";
    deps = [ gnuplot ];
  } ''
    FILE=''${1}

    if [ -z "$1" ]; then
        FILE="-"
    fi

    gnuplot -persist -e "set style line 1 lc rgb '#0060ad' lt 1 lw 2 pt 7 ps 1.5; set autoscale; set grid; plot '$FILE' with linespoints ls 1"
  '';

  acConnected = writeShellScriptBin { name = "acConnected"; deps = [ coreutils ]; } ''
    cat /sys/class/backlight/intel_backlight/max_brightness > \
      /sys/class/backlight/intel_backlight/brightness

    echo -n 2 > /sys/class/leds/dell::kbd_backlight/brightness
  '';

  acDisconnected = writeShellScriptBin { name = "acDisconnected"; } ''
    echo -n 700 > /sys/class/backlight/intel_backlight/brightness
    echo -n 0 > /sys/class/leds/dell::kbd_backlight/brightness
  '';

  xmonad-reset = writeShellScriptBin { name = "xmonad-reset"; deps = [ haskellPackages.xmonad psmisc ]; } ''
    killall xmobar
    xmonad --restart
  '';

  center-mouse = writeShellScriptBin { name = "center-mouse"; deps = [ xdotool ]; } ''
    xdotool mousemove --window $(xdotool getwindowfocus) --polar 0 0
  '';

  toggleSoundMute = writeShellScriptBin {
    name = "toggleSoundMute";
    deps = [ pulseaudioFull ];
  } ''
    pactl set-sink-mute '@DEFAULT_SINK@' toggle
  '';

  chooseNetwork = device: writeShellScriptBin { name = "chooseNetwork"; deps = [ libnotify gnugrep coreutils wpa_supplicant gnused rofi ]; pure = true; } ''
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

  lock-screen = writeShellScriptBin {
    name = "lock-screen";
    deps = [ i3lock ];
  } ''
    i3lock -p win -e -f -i ${nixos-artwork.wallpapers.simple-dark-gray}/share/artwork/gnome/nix-wallpaper-simple-dark-gray.png
  '';

  select-spotify = writeShellScriptBin {
    name = "select-spotify";
    deps = [ wmctrl ];
  } "wmctrl -x -a spotify";

  current-spotify-song = writeShellScriptBin {
    name = "current-spotify-song";
    deps = [ playerctl ];
  } ''
    getTag() {
      playerctl -p spotify metadata xesam:''${1} || echo
    }

    STATUS="$(playerctl -p spotify status)"
    TITLE="$(getTag title)"
    ARTIST="$(getTag artist)"
    ALBUM="$(getTag album)"

    if [[ "''${STATUS}" == "Playing" ]]; then
      ALBUM2="$(if [[ "$TITLE" == "$ALBUM" ]]; then echo ; else echo "($ALBUM)"; fi)"
      echo -n "<fc=orange>''${TITLE}</fc> by <fc=orange>''${ARTIST}</fc> ''${ALBUM2}"
    fi
  '';

  nix-frun = writeShellScriptBin {
    name = "nix-frun";
    deps = [ nix coreutils jq fzf zsh gnused ];
    pure = false;
  } ''
    args="$(nix search --json | jq -r 'keys[]' | sed 's/nixpkgs[.]//' | fzf -m)"

    packages=$(echo "''${args}" | while read i; do
                   echo "nixpkgs.''${i}"
               done | paste -s -d' '
            )

    echo "Running shell with: ''${args}"

    env IN_NIX_SHELL=nixrun nix run nixpkgs.zsh ''${packages} -c zsh
  '';
}
