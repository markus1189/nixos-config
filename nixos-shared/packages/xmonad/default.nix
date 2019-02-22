args@{ mutate, xmobar, xkill, xmobarLower, xmobarUpper, playerctl, selectSpotifyPlayer, rofi, chooseNetwork, xmonadReset, singlehead, takeScreenshot, lockScreen, centerMouse, multihead4k, emacsAnywhere, greenclip, mfaHelper, clipmenu, rofiDefaults, autoMonitorConfig, tmx}:

mutate ./xmonad.hs (builtins.removeAttrs args ["mutate"])
