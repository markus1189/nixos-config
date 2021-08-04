{ mutate, dunstStatus, currentSpotifySong, isVpnActive, wirelessInterface, togglTimer }:

{
  upper = mutate ./xmobarrc_upper { inherit isVpnActive wirelessInterface togglTimer dunstStatus; };
  lower = mutate ./xmobarrc_lower { inherit currentSpotifySong; };
}
