args@{ autoMonitorConfig
, autorandr
, browserHistory
, bukuRun
, dunst
, centerMouse
, chooseNetwork
, clipmenu
, ddgr
, emacsAnywhere
, flameshot
, lockScreen
, multihead4k
, mutate
, playerctl
, rofi
, rofiDefaults
, rxvt-unicode
, singlehead
, takeScreenshot
, tmx
, wyrd
, xdotool
, xkill
, xmobar
, xmobarLower
, xmobarUpper
, xmonadReset
, zsh
}:

mutate ./xmonad.hs (builtins.removeAttrs (args // { rxvtUnicode = rxvt-unicode; }) [ "mutate" "rxvt-unicode" ])
