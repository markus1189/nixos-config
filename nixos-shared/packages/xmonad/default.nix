args@{
  ghostty,
  autorandr,
  bukuRun,
  dunst,
  centerMouse,
  clipcat,
  emacsAnywhere,
  flameshot,
  lockScreen,
  flameshotOcr,
  replaceVars,
  pamixer,
  playerctl,
  rofi,
  rofiDownloadsPicker,
  rofiStuffTodayPicker,
  tmx,
  warpd,
  xdotool,
  xkill,
  xmobar,
  xmobarLower,
  xmobarUpper,
  xmonadReset,
  # custom script
  recordScript,
}:

replaceVars ./xmonad.hs (
  builtins.removeAttrs args [ "replaceVars" ]
  // {
    recordScript = "${recordScript}/bin/recordScript";
  }
)
