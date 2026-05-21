{ config, lib, pkgs, ... }:

{
  services.atuin = {
    enable = true;
    host = "127.0.0.1";
    port = 49888;
    openRegistration = false;   # p1 + p1g8 enrolled; window closed
    openFirewall = false;
    maxHistoryLength = 8192;

    database = {
      createLocally = false;
      uri = "sqlite:///var/lib/atuin/atuin.db";
    };
  };

  # Upstream module runs the unit with DynamicUser=true, so /var/lib/atuin
  # ends up under a transient UID via /var/lib/private. Pin a real user and
  # StateDirectory so the sqlite file is a normal, backup-friendly path.
  users.users.atuin = {
    isSystemUser = true;
    group = "atuin";
  };
  users.groups.atuin = { };

  systemd.services.atuin.serviceConfig = {
    DynamicUser = lib.mkForce false;
    User = "atuin";
    Group = "atuin";
    StateDirectory = "atuin";
    ProtectSystem = lib.mkForce "strict";
    ReadWritePaths = [ "/var/lib/atuin" ];
  };
}
