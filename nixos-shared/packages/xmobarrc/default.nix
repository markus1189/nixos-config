{ mutate, dunstStatus, xmobarSharingIndicator, currentSpotifySong, isVpnActive, wirelessInterface, togglTimer }:

{
  upper = mutate ./xmobarrc_upper { inherit isVpnActive wirelessInterface togglTimer dunstStatus xmobarSharingIndicator; };
  lower = mutate ./xmobarrc_lower { inherit currentSpotifySong; };
}
