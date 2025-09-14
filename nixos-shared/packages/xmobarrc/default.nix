{ mutate, dunstStatus, xmobarSharingIndicator, currentSpotifySong, isVpnActive, wirelessInterface, togglTimer, btHeadphoneBattery }:

{
  upper = mutate ./xmobarrc_upper { inherit isVpnActive wirelessInterface togglTimer dunstStatus xmobarSharingIndicator btHeadphoneBattery; };
  lower = mutate ./xmobarrc_lower { inherit currentSpotifySong; };
}
