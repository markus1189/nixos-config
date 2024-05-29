{ config, lib, pkgs, ... }:

with lib;

let cfg = config.services.x11vnc;
in {
  options.services.x11vnc = {
    enable = mkEnableOption "x11vnc server";

    auth = mkOption {
      description = "Path to .XAuthority file";
      type = types.str;
      default = "guess";
      example = "/home/me/.Xauthority";
    };

    password = mkOption {
      description = "X11vnc password";
      type = types.str;
    };

    autoStart = mkOption {
      default = false;
      type = types.bool;
      description = "Whether the x11vnc server is started automatically";
    };

    port = mkOption {
      default = 5900;
      type = types.int;
      description = "Port for the x11vnc server";
    };

    shared = mkOption {
      default = true;
      type = types.bool;
      description = "Allow sharing of the screen";
    };

    viewonly = mkOption {
      default = false;
      type = types.bool;
      description = "Add the -viewonly flag if true";
    };
  };

  config = let
    flags = [
      "-forever"
      "-display :0"
      "-auth ${cfg.auth}"
      "-passwd ${cfg.password}"
      "-ncache"
      "-rfbport ${toString cfg.port}"
    ] ++ optional cfg.viewonly "-viewonly"
      ++ (if cfg.shared then [ "-shared" ] else [ "-noshared" ]);
  in mkIf cfg.enable {
    networking.firewall.allowedTCPPorts = [ cfg.port ];

    systemd.services.x11vnc = {
      description = "x11vnc server";
      wantedBy = optional cfg.autoStart "graphical-session.target";
      path = with pkgs; [ gawk nettools ];
      serviceConfig = {
        ExecStart = "${pkgs.x11vnc}/bin/x11vnc ${concatStringsSep " " flags}";
        Restart = "always";
      };
    };
  };
}
