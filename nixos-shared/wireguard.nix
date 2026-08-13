host:
{ config, pkgs, ... }:

# Generate a new relay config:
# 1) nix shell nixpkgs#mozwire nixpkgs#wireguard-tools --command mozwire relay save -o ~/mozwire
# 2) agenix -e secrets/wg-nyc-<host>.age and paste the chosen .conf verbatim

{
  age.secrets.wg-nyc = {
    file = ../secrets + "/wg-nyc-${host}.age";
  };

  networking = {
    wg-quick = {
      interfaces = {
        wg-nyc.configFile = config.age.secrets.wg-nyc.path;
      };
    };
  };
}
