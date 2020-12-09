{ mutate, currentSpotifySong, isVpnActive, wirelessInterface }:

{
  upper = mutate ./xmobarrc_upper { inherit isVpnActive wirelessInterface; };
  lower = mutate ./xmobarrc_lower { inherit currentSpotifySong; };
}
