{ config, pkgs, ... }:

{
  services = {
    openvpn.servers = {
      ottoVPN = {
        config = ''
          config /home/markus/repos/otto/vpn/ottogroup_unmanged_v1.3.ovpn
        '';
        autoStart = false;
        updateResolvConf = true;
      };
    };
  };
}
