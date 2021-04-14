{ mutate, currentSpotifySong, isVpnActive, wirelessInterface, togglTimer }:

{
  upper = mutate ./xmobarrc_upper { inherit isVpnActive wirelessInterface togglTimer; };
  lower = mutate ./xmobarrc_lower { inherit currentSpotifySong; };
}
