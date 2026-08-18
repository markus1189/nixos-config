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
  mutate,
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

mutate ./xmonad.hs (
  builtins.removeAttrs args [ "mutate" ] // { recordScript = "${recordScript}/bin/recordScript"; }
)
