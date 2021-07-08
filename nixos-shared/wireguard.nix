{ config, pkgs, ... }:

let
  secrets = import ./secrets.nix;
in
{
  networking = {
    wg-quick = {
      interfaces = {
        wg-us101 = {
          address = [ "10.68.64.184/32" "fc00:bbbb:bbbb:bb01::5:40b7/128" ];
          dns = [ "10.64.0.1" ];
          privateKey = secrets.wireguard.mozilla.privateKeys.us101;

          peers = [{
            allowedIPs = [ "0.0.0.0/0" "::0/0" ];
            endpoint = "86.106.121.158:51820";
            publicKey = "iuFDwOiNNnWfTmtprPCLscUjonu+KKfXu39TL/SSzh4=";
          }];
        };
      };
    };
  };
}
