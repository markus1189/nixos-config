host:
{ config, pkgs, ... }:

# Generate new relay server lists:
# 1) Cleanup existing files: 
# rm -v ~/mozwire/*.conf
# 2) Save the new files:
# nix shell nixpkgs#mozwire nixpkgs#wireguard-tools nixpkgs#remarshal --command "mozwire" relay save -o ~/mozwire 
# 3) Update ./secrets.nix with the content of the file

let
  secrets = import ./secrets.nix;
in
{
  networking = {
    wg-quick = {
      interfaces = {
        wg-nyc = secrets.wireguard.mozilla.${host}.wg-nyc;
      };
    };
  };
}
