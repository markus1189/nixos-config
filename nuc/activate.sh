sudo rm -r /etc/nixos/ /etc/nixos-shared ; sudo mkdir /etc/nixos && sudo cp -r ~/repos/nixos-config/nixos-shared /etc/ && sudo cp ~/repos/nixos-config/nuc/* /etc/nixos/ && sudo nixos-rebuild switch
