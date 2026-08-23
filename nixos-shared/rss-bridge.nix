{ config, pkgs, ... }:

{
  virtualisation.oci-containers.containers = {
    rssBridge = {
      autoStart = true;
      image = "rssbridge/rss-bridge@sha256:3d151be86e9b8935ee184670b3a1e0809316a3be0b9656d0c749b3dae458d09a"; # sha-309af5b, 2026-08-18
      ports = [ "127.0.0.1:9998:80" ];
    };
  };
}
