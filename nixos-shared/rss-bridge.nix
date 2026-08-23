{ config, pkgs, ... }:

{
  virtualisation.oci-containers.containers = {
    rssBridge = {
      autoStart = true;
      image = "rssbridge/rss-bridge";
      ports = [ "127.0.0.1:9998:80" ];
    };
  };
}
