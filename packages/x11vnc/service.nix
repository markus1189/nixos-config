{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.x11vnc;
in {
  options.services.x11vnc = {
    enable = mkEnableOption "x11vnc server";

    auth = mkOption {
      description = "Path to .XAuthority file";
      type = types.string;
      default = "guess";
      example = "/home/me/.Xauthority";
    };

    password = mkOption {
      description = "X11vnc password";
      type = types.string;
    };
  };

  config = mkIf cfg.enable {
    networking.firewall.allowedTCPPorts = [5900];

    systemd.services.x11vnc = {
      description = "x11vnc server";
      wantedBy = [ "multi-user.target" ];
      path = with pkgs; [
        gawk
        nettools
      ];
      serviceConfig = {
        ExecStart = "${pkgs.x11vnc}/bin/x11vnc -display :0 -auth ${cfg.auth} -passwd ${cfg.password} -ncache";
        Restart = "always";
      };
    };
  };
}
