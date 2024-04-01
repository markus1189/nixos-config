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
# , normcap
, multihead4k
, mutate
, playerctl
, rofi
, rofiDefaults
, rxvt-unicode
, singlehead
, takeScreenshot
, tesseract4
, tmx
, warpd
, wyrd
, xclip
, xdotool
, xkill
, xmobar
, xmobarLower
, xmobarUpper
, xmonadReset
, zsh
}:

mutate ./xmonad.hs (builtins.removeAttrs (args // { rxvtUnicode = rxvt-unicode; }) [ "mutate" "rxvt-unicode" ])
