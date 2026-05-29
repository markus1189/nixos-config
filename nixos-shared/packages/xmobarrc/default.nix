{ mutate, dunstStatus, xmobarSharingIndicator, currentSpotifySong, isVpnActive, wirelessInterface, togglTimer, btHeadphoneBattery, chargeRate, writeShellApplication, iw }:

let
  # Replacement for xmobar's `Run Wireless`, which reads /proc/net/wireless via
  # libiw (Wireless Extensions). Newer iwlwifi drivers (e.g. Intel BE201 on
  # kernel >=7.0) no longer populate that file, so the legacy plugin renders
  # blank. nl80211 via `iw` still works.
  wirelessSsid = writeShellApplication {
    name = "wirelessSsid";
    runtimeInputs = [ iw ];
    text = ''
      iface=${wirelessInterface}
      link=$(iw dev "$iface" link 2>/dev/null) || exit 0
      case "$link" in
        "Not connected."*|"") exit 0 ;;
      esac
      ssid=$(printf '%s\n' "$link" | sed -n 's/^[[:space:]]*SSID: //p')
      signal=$(printf '%s\n' "$link" | sed -n 's/^[[:space:]]*signal: \(-\{0,1\}[0-9]\{1,\}\).*/\1/p')
      [ -z "$ssid" ] && exit 0
      [ -z "$signal" ] && exit 0
      qual=$(( 2 * (signal + 100) ))
      if [ "$qual" -gt 100 ]; then qual=100; fi
      if [ "$qual" -lt 0 ]; then qual=0; fi
      if   [ "$qual" -lt 40 ]; then color=red
      elif [ "$qual" -gt 70 ]; then color="#42d3a5"
      else                          color=lightgray
      fi
      printf '<fc=orange>%s</fc> (<fc=%s>%d%%</fc>)\n' "$ssid" "$color" "$qual"
    '';
  };
in
{
  upper = mutate ./xmobarrc_upper { inherit isVpnActive wirelessInterface togglTimer dunstStatus xmobarSharingIndicator btHeadphoneBattery chargeRate wirelessSsid; };
  lower = mutate ./xmobarrc_lower { inherit currentSpotifySong; };
}
