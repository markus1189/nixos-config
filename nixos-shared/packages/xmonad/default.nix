args@{ alacritty, autoMonitorConfig, autorandr, bukuRun, dunst
, centerMouse, chooseNetwork, clipmenu, ddgr, emacsAnywhere, flameshot
, lockScreen, flameshotOcr, multihead4k, mutate, pamixer, playerctl, rofi
, rofiDefaults, singlehead, takeScreenshot, tesseract4, tmx, warpd, wyrd, xclip
, xdotool, xkill, xmobar, xmobarLower, xmobarUpper, xmonadReset, zsh }:

mutate ./xmonad.hs (builtins.removeAttrs args [ "mutate" ])
