args@{ mutate, xmobar, xkill, xmobarLower, xmobarUpper, playerctl, rofi
, chooseNetwork, xmonadReset, singlehead, takeScreenshot, lockScreen
, centerMouse, multihead4k, emacsAnywhere, clipmenu, rofiDefaults
, autoMonitorConfig, tmx, browserHistory, bukuRun, ddgr, flameshot, autorandr }:

mutate ./xmonad.hs (builtins.removeAttrs args [ "mutate" ])
