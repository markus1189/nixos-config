{ mutate, currentSpotifySong, isVpnActive }:

{
  upper = mutate ./xmobarrc_upper { inherit isVpnActive; };
  lower = mutate ./xmobarrc_lower { inherit currentSpotifySong; };
}
