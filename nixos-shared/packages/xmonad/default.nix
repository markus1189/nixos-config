args@{ mutate, xmobar, xkill, xmobarLower, xmobarUpper, playerctl
, selectSpotifyPlayer, rofi, chooseNetwork, xmonadReset, singlehead
, takeScreenshot, lockScreen, centerMouse, multihead4k, emacsAnywhere, mfaHelper
, clipmenu, rofiDefaults, autoMonitorConfig, tmx, browserHistory, bukuRun, ddgr
, flameshot
}:

mutate ./xmonad.hs (builtins.removeAttrs args [ "mutate" ])
