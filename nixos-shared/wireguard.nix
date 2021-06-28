{ config, pkgs, ... }:

let
  secrets = import ./secrets.nix;
in
{
  networking = {
    wg-quick = {
      interfaces = {
        wg0 = {
          address = [ "10.67.44.173/32" "fc00:bbbb:bbbb:bb01::4:2cac/128" ];
          dns = [ "10.64.0.1" ];
          privateKey = secrets.wireguard.mozilla.privateKey;

          peers = [{
            allowedIPs = [ "0.0.0.0/0" "::0/0" ];
            endpoint = "194.110.113.51:51820";
            publicKey = "3DsVxyx5NU+RiwTKqUKqG8/9yiEk+SFd+21DEZ0dAwo=";
          }];
        };
      };
    };
  };
}
