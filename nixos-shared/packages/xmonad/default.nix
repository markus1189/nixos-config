args@{ autoMonitorConfig
, autorandr
, browserHistory
, bukuRun
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
, singlehead
, takeScreenshot
, tmx
, xdotool
, xkill
, xmobar
, xmobarLower
, xmobarUpper
, xmonadReset
}:

mutate ./xmonad.hs (builtins.removeAttrs args [ "mutate" ])
