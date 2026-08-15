{
  config,
  lib,
  pkgs,
  ...
}:

# Generate a new relay config:
# 1) nix shell nixpkgs#mozwire nixpkgs#wireguard-tools --command mozwire relay save -o ~/mozwire
# 2) agenix -e secrets/wg-nyc-<profile>.age and paste the chosen .conf verbatim

{
  options.my.wgProfile = lib.mkOption {
    type = lib.types.str;
    default = config.networking.hostName;
    defaultText = lib.literalExpression "config.networking.hostName";
    description = "Suffix of the secrets/wg-nyc-<profile>.age WireGuard config";
  };

  config = {
    age.secrets.wg-nyc = {
      file = ../secrets + "/wg-nyc-${config.my.wgProfile}.age";
    };

    networking = {
      wg-quick = {
        interfaces = {
          wg-nyc.configFile = config.age.secrets.wg-nyc.path;
        };
      };
    };
  };
}
