echo activating "$(date)"
sudo nixos-rebuild -I nixos-config=/home/mediacenter/repos/nixos-config/nuc/configuration.nix switch
echo activated "$(date)"
